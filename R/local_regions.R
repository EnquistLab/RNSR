# Regions, and the spatial relations between them.
#
# Sources publish against different geographies: WCVP against WGSRPD level-3 areas
# (biogeographic), VASCAN and Flora do Brasil against GADM level-1 units (political).
# Both are kept as they are. A coordinate is located in each system directly; a query
# given only division names is carried between systems by a link table derived from the
# polygons themselves, so nothing is matched by name.

#' Paths of the region rasters
#' @keywords internal
#' @noRd
nsr_raster_path <- function(what, dir = nsr_cache_dir()) {
  switch(what,
    wgsrpd3 = file.path(dir, "nsr-wgsrpd3-30s.tif"),
    wgsrpd3_index = file.path(dir, "nsr-wgsrpd3-index.gz.parquet"),
    stop("unknown raster: ", what)
  )
}

#' Rasterize the WGSRPD level-3 polygons
#'
#' Internal.  30 arc-seconds, to match the GADM index GVS builds, so the two can be
#' cross-tabulated cell for cell.  Polygons come from \code{rWCVPdata}, the published
#' TDWG shapes; nothing is downloaded here.
#' @keywords internal
#' @noRd
nsr_build_wgsrpd_raster <- function(dir = nsr_cache_dir(create = TRUE), resolution = 1 / 120,
                                    quiet = FALSE) {
  for (pkg in c("sf", "terra")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Needs the '", pkg, "' package.", call. = FALSE)
  }
  pol <- nsr_wgsrpd_polygons()
  idx <- data.frame(unit = seq_len(nrow(pol)), area_code_l3 = pol$LEVEL3_COD,
                    area_name = pol$LEVEL3_NAM, stringsAsFactors = FALSE)
  nanoparquet::write_parquet(idx, nsr_raster_path("wgsrpd3_index", dir), compression = "gzip")
  if (!quiet) message("Rasterizing ", nrow(pol), " WGSRPD level-3 areas at ",
                      round(resolution * 3600), " arc-seconds ...")
  pol$unit <- idx$unit
  tmpl <- terra::rast(terra::ext(-180, 180, -90, 90), resolution = resolution, crs = "EPSG:4326")
  terra::rasterize(terra::vect(pol), tmpl, field = "unit",
                   filename = nsr_raster_path("wgsrpd3", dir), overwrite = TRUE,
                   wopt = list(datatype = "INT2U", gdal = c("COMPRESS=DEFLATE", "TILED=YES")))
  if (!quiet) message("  done")
  invisible(nsr_raster_path("wgsrpd3", dir))
}

#' The TDWG level-3 polygons
#' @keywords internal
#' @noRd
nsr_wgsrpd_polygons <- function() {
  if (requireNamespace("rWCVPdata", quietly = TRUE)) {
    p <- try(rWCVPdata::wgsrpd3, silent = TRUE)
    if (!inherits(p, "try-error")) return(sf::st_as_sf(p))
  }
  if (requireNamespace("rWCVP", quietly = TRUE)) {
    e <- new.env()
    utils::data("wgsrpd3", package = "rWCVP", envir = e)
    return(sf::st_as_sf(get("wgsrpd3", envir = e)))
  }
  stop("The WGSRPD level-3 polygons need the 'rWCVPdata' (or 'rWCVP') package.", call. = FALSE)
}

#' Spatial links between region systems
#'
#' Internal.  Cross-tabulates the WGSRPD level-3 raster against the GADM unit index GVS
#' builds, in blocks of rows, and records for each overlapping pair how much of each
#' region lies in the other.  \code{fraction_from} is the share of the FROM region inside
#' the TO region, so \code{within} means the from-region is (almost) entirely inside.
#'
#' @return Invisibly, the link table.
#' @keywords internal
#' @noRd
nsr_build_region_links <- function(dir = nsr_cache_dir(), gadm_raster = NULL, block_rows = 500,
                                   within_threshold = 0.95, quiet = FALSE) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Needs the 'terra' package.", call. = FALSE)
  if (is.null(gadm_raster)) gadm_raster <- file.path(dir, "gadmindex-units-30s.tif")
  if (!file.exists(gadm_raster)) {
    stop("The GADM unit index is not in ", dir, ".\n",
         "Build it with GVS (GVS_local_build(\"index\")); NSR reuses it rather than making its own.",
         call. = FALSE)
  }
  if (!file.exists(nsr_raster_path("wgsrpd3", dir))) nsr_build_wgsrpd_raster(dir, quiet = quiet)

  w <- terra::rast(nsr_raster_path("wgsrpd3", dir))
  g <- terra::rast(gadm_raster)
  if (!isTRUE(all.equal(as.vector(terra::ext(w)), as.vector(terra::ext(g)))) ||
      terra::ncol(w) != terra::ncol(g) || terra::nrow(w) != terra::nrow(g)) {
    stop("The WGSRPD and GADM rasters do not align; rebuild one at the other's resolution.",
         call. = FALSE)
  }
  units <- as.data.frame(nanoparquet::read_parquet(file.path(dir, "gadmindex-units.gz.parquet")))
  widx <- as.data.frame(nanoparquet::read_parquet(nsr_raster_path("wgsrpd3_index", dir)))

  nr <- terra::nrow(w)
  nc <- terra::ncol(w)
  terra::readStart(w); terra::readStart(g)
  on.exit({terra::readStop(w); terra::readStop(g)}, add = TRUE)
  acc <- new.env(parent = emptyenv())
  if (!quiet) message("Cross-tabulating WGSRPD x GADM (", nr, " rows) ...")
  for (s in seq(1L, nr, by = block_rows)) {
    e <- min(nr, s + block_rows - 1L)
    wv <- terra::readValues(w, row = s, nrows = e - s + 1L, col = 1L, ncols = nc)
    gv <- terra::readValues(g, row = s, nrows = e - s + 1L, col = 1L, ncols = nc)
    ok <- !is.na(wv) & !is.na(gv) & wv > 0 & gv > 0
    if (!any(ok)) next
    key <- paste(wv[ok], gv[ok])
    tb <- table(key)
    for (k in names(tb)) {
      acc[[k]] <- (if (is.null(acc[[k]])) 0L else acc[[k]]) + as.integer(tb[[k]])
    }
    rm(wv, gv, ok, key, tb)
  }
  keys <- ls(acc)
  if (!length(keys)) stop("No overlap found between the two rasters.", call. = FALSE)
  parts <- do.call(rbind, strsplit(keys, " ", fixed = TRUE))
  cells <- vapply(keys, function(k) acc[[k]], integer(1))
  d <- data.frame(w_unit = as.integer(parts[, 1]), g_unit = as.integer(parts[, 2]),
                  cells = as.integer(cells), stringsAsFactors = FALSE)
  d$w_key <- paste0("wgsrpd3:", widx$area_code_l3[match(d$w_unit, widx$unit)])
  gi <- match(d$g_unit, units$unit)
  d$g_key <- ifelse(!is.na(units$gid_1[gi]), paste0("gadm1:", units$gid_1[gi]),
                    paste0("gadm0:", units$gid_0[gi]))
  # A query about a country must find links too, so the level-0 pairs are built as well -
  # but each level needs its own denominators. Pooling them counts every cell twice (once
  # for its state, once for its country), which halves every fraction and turns even
  # coextensive regions into "overlaps".
  d$g_key0 <- paste0("gadm0:", units$gid_0[gi])
  fracs <- function(gk) {
    dd <- stats::aggregate(cells ~ w_key + g_key,
                           data = data.frame(w_key = d$w_key, g_key = gk, cells = d$cells,
                                             stringsAsFactors = FALSE), FUN = sum)
    wt <- stats::aggregate(cells ~ w_key, data = dd, FUN = sum)
    gt <- stats::aggregate(cells ~ g_key, data = dd, FUN = sum)
    dd$frac_w <- dd$cells / wt$cells[match(dd$w_key, wt$w_key)]
    dd$frac_g <- dd$cells / gt$cells[match(dd$g_key, gt$g_key)]
    dd
  }
  d <- rbind(fracs(d$g_key), fracs(d$g_key0))
  d <- d[!duplicated(paste(d$w_key, d$g_key)), , drop = FALSE]

  # Coextensive regions (California the GADM state and CAL the WGSRPD area) are the same
  # place, and must not demote a native opinion to mere presence. The threshold is 0.95,
  # not 0.99: these fractions come from 30 arc-second rasters of two independent
  # coastlines, so Austria and WGSRPD's AUT score 0.990/0.988 rather than 1.0.
  rel <- function(frac_from, frac_to) {
    ifelse(frac_from >= within_threshold & frac_to >= within_threshold, "same",
           ifelse(frac_from >= within_threshold, "within",
                  ifelse(frac_to >= within_threshold, "contains", "overlaps")))
  }
  links <- rbind(
    data.frame(from_region = d$w_key, to_region = d$g_key,
               relation = rel(d$frac_w, d$frac_g), fraction = d$frac_w, stringsAsFactors = FALSE),
    data.frame(from_region = d$g_key, to_region = d$w_key,
               relation = rel(d$frac_g, d$frac_w), fraction = d$frac_g, stringsAsFactors = FALSE)
  )
  nanoparquet::write_parquet(links, nsr_table_path("region-links", dir), compression = "gzip")
  if (!quiet) message("  ", format(nrow(links), big.mark = ","), " links (",
                      sum(links$relation == "within"), " within, ",
                      sum(links$relation == "contains"), " contains, ",
                      sum(links$relation == "overlaps"), " overlaps)")
  invisible(links)
}

#' Locate coordinates in both region systems
#'
#' Internal.  Raster lookup in the WGSRPD level-3 raster and in the GADM unit index, so a
#' record with coordinates is placed in each source's own geography without any crosswalk.
#' @return data.frame with \code{wgsrpd3} and \code{gadm} region keys (and the GADM
#'   country key), one row per point.
#' @keywords internal
#' @noRd
nsr_locate_regions <- function(longitude, latitude, dir = nsr_cache_dir()) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Needs the 'terra' package.", call. = FALSE)
  n <- length(longitude)
  out <- data.frame(wgsrpd3 = rep(NA_character_, n), gadm = NA_character_, gadm0 = NA_character_,
                    stringsAsFactors = FALSE)
  ok <- is.finite(longitude) & is.finite(latitude)
  if (!any(ok)) return(out)
  xy <- cbind(longitude[ok], latitude[ok])

  wf <- nsr_raster_path("wgsrpd3", dir)
  if (file.exists(wf)) {
    widx <- as.data.frame(nanoparquet::read_parquet(nsr_raster_path("wgsrpd3_index", dir)))
    u <- as.integer(terra::extract(terra::rast(wf), xy)[, 1])
    out$wgsrpd3[ok] <- ifelse(is.na(u) | u == 0, NA_character_,
                              paste0("wgsrpd3:", widx$area_code_l3[match(u, widx$unit)]))
  }
  gf <- file.path(dir, "gadmindex-units-30s.tif")
  if (file.exists(gf)) {
    units <- as.data.frame(nanoparquet::read_parquet(file.path(dir, "gadmindex-units.gz.parquet")))
    u <- as.integer(terra::extract(terra::rast(gf), xy)[, 1])
    i <- match(u, units$unit)
    out$gadm[ok] <- ifelse(is.na(i), NA_character_,
                           ifelse(!is.na(units$gid_1[i]), paste0("gadm1:", units$gid_1[i]),
                                  paste0("gadm0:", units$gid_0[i])))
    out$gadm0[ok] <- ifelse(is.na(i), NA_character_, paste0("gadm0:", units$gid_0[i]))
  }
  out
}

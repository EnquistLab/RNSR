#' Build the local native-status reference
#'
#' Reads each checklist source, resolves its names against WCVP with
#' \code{TNRS::TNRS_local()} and its political divisions against the GNRS backbone, and
#' writes the shared cache tables.  Sources are acquired on the user's machine; nothing
#' derived ships with the package.
#'
#' \strong{The archives are not downloaded for you.}  \code{vascan} and \code{flbr} must
#' be supplied through \code{files}, and each stops with the URL to fetch if they are
#' not; \code{wcvp} is the exception, since it reads the WCVP archive already in the
#' TNRS cache when there is one.  This is why \code{sources} defaults to \code{"wcvp"}
#' alone: it is the only source that can build unattended.
#'
#' \code{"powo"} is accepted as a synonym for \code{"wcvp"} - it is the name the live
#' service reports for the same Kew dataset - but \code{wcvp} is what gets written to
#' \code{native_status_sources}.
#'
#' Tables written: \code{nsr-sources} (one per source, with licence and whether it is
#' comprehensive), \code{nsr-taxa} (one per taxon, keyed on the WCVP accepted id),
#' \code{nsr-regions} (one per region, in the geography its source publishes against) and
#' \code{nsr-checklist} (one per taxon x region x source).  \code{nsr-region-links}, the
#' spatial relations between geographies, is built too when the GADM index from GVS is in
#' the cache.
#'
#' @param sources Which sources to build.  See \code{NSR_local_status()}.
#' @param dir Cache directory, shared with GNRS and GVS.
#' @param files Named list of local archives to read, e.g.
#'   \code{list(flbr = "flbr_dwca.zip")}.  Required for \code{vascan} and \code{flbr}.
#'   For \code{wcvp}, the WCVP zip; if absent, the copy in the TNRS cache is used when
#'   there is one.
#' @param overwrite Rebuild sources that are already built?
#' @param quiet Suppress progress messages?
#' @return Invisibly, \code{NSR_local_status()}.
#' @export
NSR_local_build <- function(sources = "wcvp",
                            dir = nsr_cache_dir(create = TRUE),
                            files = list(), overwrite = FALSE, quiet = FALSE) {
  for (pkg in c("nanoparquet", "TNRS")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Building the local NSR needs the '", pkg, "' package.", call. = FALSE)
    }
  }
  reg <- nsr_builtin_registry()
  sources <- match.arg(nsr_canonical_source(sources), names(reg), several.ok = TRUE)
  if (length(files)) names(files) <- nsr_canonical_source(names(files))
  msg <- function(...) if (!quiet) message(...)

  existing <- lapply(nsr_tables(), function(t) {
    p <- nsr_table_path(t, dir)
    if (file.exists(p)) as.data.frame(nanoparquet::read_parquet(p)) else NULL
  })
  names(existing) <- nsr_tables()
  done <- if (is.null(existing$sources)) character(0) else existing$sources$source_name
  todo <- setdiff(sources, if (overwrite) character(0) else done)
  if (!length(todo)) {
    msg("Nothing to build (", paste(sources, collapse = ", "), " already built).")
    return(invisible(NSR_local_status(dir)))
  }

  bb <- nsr_gnrs_backbone(dir)
  raw <- list()
  for (s in todo) {
    msg("Importing ", s, " ...")
    t0 <- Sys.time()
    raw[[s]] <- switch(s,
      wcvp = nsr_import_wcvp(files$wcvp, bb, quiet),
      vascan = nsr_import_vascan(files$vascan, bb, quiet),
      flbr = nsr_import_flbr(files$flbr, bb, quiet)
    )
    msg("  ", format(nrow(raw[[s]]), big.mark = ","), " records (",
        round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")
  }

  # ---- names --------------------------------------------------------------------
  # sources keyed on WCVP already carry the taxon id (POWO is the backbone); the rest
  # are resolved once, together, against the same backbone
  need <- unique(unlist(lapply(raw, function(x) x$taxon_name[is.na(x$taxon_id)])))
  need <- need[!is.na(need) & nzchar(need)]
  res <- if (length(need)) {
    msg("Resolving ", format(length(need), big.mark = ","), " distinct names against WCVP ...")
    # POWO's species table, when it is being built in this run, keys the others
    sp_index <- do.call(rbind, lapply(raw, function(d) {
      if (!all(c("taxon_id", "species_name") %in% names(d))) return(NULL)
      x <- unique(d[!is.na(d$taxon_id), c("taxon_id", "species_name")])
      x[!duplicated(x$species_name), , drop = FALSE]
    }))
    if (is.null(sp_index) && !is.null(existing$taxa)) {
      sp_index <- existing$taxa[, c("taxon_id", "species_name")]
    }
    r <- nsr_resolve_names(need, quiet = quiet, species_index = sp_index)
    msg("  matched ", format(sum(!is.na(r$taxon_id)), big.mark = ","), " (",
        round(100 * mean(!is.na(r$taxon_id)), 1), "%)")
    r
  } else {
    data.frame(name = character(0), taxon_id = character(0), species_name = character(0),
               family = character(0), genus = character(0), rank = character(0),
               stringsAsFactors = FALSE)
  }

  # ---- assemble ---------------------------------------------------------------------
  keep_old <- function(x, col) if (is.null(x)) NULL else x[!x[[col]] %in% todo, , drop = FALSE]
  # A cache built before extinct records were retained has no is_extinct column, and the
  # rows kept from it are rbind()ed with new rows that do; give it the column first, or
  # adding a source to an older cache fails on the bind.
  if (!is.null(existing$checklist) && is.null(existing$checklist$is_extinct)) {
    existing$checklist$is_extinct <- 0L
  }
  chk <- lapply(names(raw), function(s) {
    d <- raw[[s]]
    fill <- is.na(d$taxon_id)
    d$taxon_id[fill] <- res$taxon_id[match(d$taxon_name[fill], res$name)]
    d <- d[!is.na(d$taxon_id) & !is.na(d$region_key), , drop = FALSE]
    data.frame(taxon_id = d$taxon_id, region_key = d$region_key, status = d$status,
               is_cultivated = d$is_cultivated,
               is_extinct = if (is.null(d$is_extinct)) 0L else as.integer(d$is_extinct),
               source_name = s, stringsAsFactors = FALSE)
  })
  checklist <- unique(do.call(rbind, c(list(keep_old(existing$checklist, "source_name")), chk)))

  taxa_res <- res[!is.na(res$taxon_id), c("taxon_id", "family", "genus", "species_name", "rank")]
  taxa_src <- do.call(rbind, lapply(raw, function(d) {
    if (!all(c("family", "genus", "species_name", "rank") %in% names(d))) return(NULL)
    x <- d[!is.na(d$taxon_id), c("taxon_id", "family", "genus", "species_name", "rank")]
    x[!duplicated(x$taxon_id), , drop = FALSE]
  }))
  taxa_new <- rbind(taxa_res, taxa_src)
  taxa <- unique(rbind(existing$taxa, taxa_new))
  taxa <- taxa[!duplicated(taxa$taxon_id), , drop = FALSE]

  reg_new <- unique(do.call(rbind, lapply(raw, function(d) {
    x <- d[!is.na(d$region_key), c("region_key", "region_name")]
    x[!duplicated(x$region_key), , drop = FALSE]
  })))
  reg_new$system <- sub(":.*", "", reg_new$region_key)
  reg_new$code <- sub("^[^:]*:", "", reg_new$region_key)
  regions <- unique(rbind(existing$regions, reg_new))
  regions <- regions[!duplicated(regions$region_key), , drop = FALSE]

  src_new <- do.call(rbind, lapply(names(raw), function(s) {
    r <- reg[[s]]
    n <- sum(checklist$source_name == s)
    data.frame(source_name = s, source_name_full = r$source_name_full, version = r$version,
               url = r$url, licence = r$licence, citation = r$citation,
               is_comprehensive = r$is_comprehensive, region_system = r$region_system,
               coverage = r$coverage, date_accessed = as.character(Sys.Date()),
               n_records = n, n_taxa = length(unique(checklist$taxon_id[checklist$source_name == s])),
               stringsAsFactors = FALSE)
  }))
  srcs <- rbind(keep_old(existing$sources, "source_name"), src_new)

  for (nm in nsr_tables()) {
    x <- switch(nm, sources = srcs, taxa = taxa, regions = regions, checklist = checklist)
    nanoparquet::write_parquet(x, nsr_table_path(nm, dir), compression = "gzip")
  }
  saveRDS(list(built = as.character(Sys.Date()), sources = srcs$source_name,
               n_records = nrow(checklist), n_taxa = nrow(taxa), n_regions = nrow(regions)),
          nsr_provenance_path("nsr", dir))

  # ---- geography ---------------------------------------------------------------------
  # The WGSRPD raster is what places coordinates in a WCVP region, so it is needed
  # whenever a source publishes against WGSRPD - not only when there is a second
  # geography to link it to.  A wcvp-only build needs it just as much.
  systems <- unique(regions$system)
  raster_ok <- TRUE
  if ("wgsrpd3" %in% systems && !file.exists(nsr_raster_path("wgsrpd3", dir))) {
    msg("Building the WGSRPD level-3 raster ...")
    r <- try(nsr_build_wgsrpd_raster(dir = dir, quiet = quiet), silent = TRUE)
    raster_ok <- !inherits(r, "try-error")
    if (!raster_ok) {
      warning("The WGSRPD level-3 raster could not be built: ",
              conditionMessage(attr(r, "condition")), "\n",
              "The checklist tables are written, but until this raster exists no query ",
              "can reach a WCVP opinion: coordinates cannot be placed in a WCVP region, ",
              "and division names resolve to GADM, which needs the link table below. ",
              "Install 'rWCVPdata' (or 'rWCVP'), then re-run NSR_local_build(overwrite = TRUE).",
              call. = FALSE)
    }
  }

  # Names always resolve to GADM through the GNRS backbone, whatever geography the
  # sources use, so a WGSRPD source is unreachable by name until the two are linked.
  # That holds for a wcvp-only build as much as a mixed one - hence no test on the
  # number of systems, which is what used to leave the default build unanswerable.
  needs_links <- "wgsrpd3" %in% systems || length(systems) > 1
  gadm_index <- file.path(dir, "gadmindex-units-30s.tif")
  if (needs_links && raster_ok && file.exists(gadm_index)) {
    msg("Linking region systems (", paste(c(systems, "gadm"), collapse = ", "), ") ...")
    r <- try(nsr_build_region_links(dir = dir, quiet = quiet), silent = TRUE)
    if (inherits(r, "try-error")) {
      warning("The region link table could not be built: ",
              conditionMessage(attr(r, "condition")), "\n",
              "The cache is written, but queries by division name will not reach sources ",
              "published against another geography. Re-run NSR_local_build(overwrite = TRUE) ",
              "once the cause is fixed.", call. = FALSE)
    }
  } else if (needs_links && raster_ok) {
    warning("GVS's GADM index is not in ", dir, ", so the geographies cannot be linked. ",
            "Queries by division name resolve to GADM and will not reach sources published ",
            "against WGSRPD; coordinates still work. Build it with GVS, then re-run ",
            "NSR_local_build(overwrite = TRUE).", call. = FALSE)
  }
  msg("Built: ", format(nrow(checklist), big.mark = ","), " checklist records, ",
      format(nrow(taxa), big.mark = ","), " taxa, ", format(nrow(regions), big.mark = ","),
      " regions.")
  invisible(NSR_local_status(dir))
}

#' The GNRS political-division backbone, as NSR keys on it
#' @keywords internal
#' @noRd
nsr_gnrs_backbone <- function(dir) {
  f <- function(x) file.path(dir, paste0("gnrs-", x, ".gz.parquet"))
  if (!file.exists(f("country")) || !file.exists(f("state_province"))) {
    stop("The GNRS backbone is not in ", dir, ".\n",
         "Build it first with GNRS::GNRS_local_build(); NSR keys its divisions on GNRS ids.",
         call. = FALSE)
  }
  list(country = as.data.frame(nanoparquet::read_parquet(f("country"))),
       state = as.data.frame(nanoparquet::read_parquet(f("state_province"))))
}

#' Political division ids for a set of country (and optionally state) names
#'
#' Internal.  Exact, case- and accent-insensitive matching against the GNRS backbone;
#' anything unmatched comes back NA and is reported by the caller, rather than being
#' guessed at.
#' @keywords internal
#' @noRd
nsr_match_poldiv <- function(country, state = NULL, bb) {
  norm <- function(x) {
    x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
    tolower(trimws(gsub("[^A-Za-z0-9 ]", "", ifelse(is.na(x), "", x))))
  }
  ci <- match(norm(country), norm(bb$country$country))
  out <- data.frame(
    country_id = bb$country$country_id[ci], country = bb$country$country[ci],
    state_province_id = NA_integer_, state_province = NA_character_,
    stringsAsFactors = FALSE
  )
  if (!is.null(state)) {
    key <- paste(norm(country), norm(state))
    # the backbone spells some divisions bilingually ("New Brunswick/Nouveau-Brunswick"),
    # so every variant is a valid target
    variants <- function(col) {
      parts <- strsplit(ifelse(is.na(col), "", col), "/", fixed = TRUE)
      data.frame(row = rep(seq_along(parts), lengths(parts)),
                 name = norm(unlist(parts)), stringsAsFactors = FALSE)
    }
    v <- unique(rbind(
      data.frame(row = seq_len(nrow(bb$state)), name = norm(bb$state$state_province)),
      data.frame(row = seq_len(nrow(bb$state)), name = norm(bb$state$state_province_std)),
      variants(bb$state$state_province), variants(bb$state$state_province_std)))
    v$key <- paste(norm(bb$state$country)[v$row], v$name)
    v <- v[!duplicated(v$key), , drop = FALSE]
    si <- v$row[match(key, v$key)]
    out$state_province_id <- bb$state$state_province_id[si]
    out$state_province <- bb$state$state_province[si]
  }
  out$poldiv_level <- ifelse(!is.na(out$state_province_id), "state_province",
                             ifelse(!is.na(out$country_id), "country", NA_character_))
  out$poldiv_id <- ifelse(!is.na(out$state_province_id), paste0("S", out$state_province_id),
                          ifelse(!is.na(out$country_id), paste0("C", out$country_id), NA_character_))
  out
}

#' Resolve checklist names to WCVP accepted species
#'
#' Internal.  The id returned is the SPECIES' id, not an infraspecific one, so every
#' source keys on the same unit as POWO after its infraspecifics are rolled up.
#' @keywords internal
#' @noRd
nsr_resolve_names <- function(names_in, quiet = FALSE, species_index = NULL) {
  r <- TNRS::TNRS_local(taxonomic_names = names_in, sources = "wcvp", matches = "best",
                        build_missing = FALSE, quiet = quiet)
  acc <- r$Accepted_species
  acc[is.na(acc) | !nzchar(acc)] <- NA_character_
  # prefer the species' own id where we know it, so subspecies land on their species
  id <- ifelse(is.na(acc), NA_character_, r$Accepted_name_id)
  if (!is.null(species_index)) {
    sid <- species_index$taxon_id[match(acc, species_index$species_name)]
    id <- ifelse(!is.na(sid), sid, id)
  }
  data.frame(
    name = r$Name_submitted,
    taxon_id = id,
    species_name = acc,
    family = r$Accepted_family,
    genus = sub(" .*", "", acc),
    rank = r$Accepted_name_rank,
    stringsAsFactors = FALSE
  )
}

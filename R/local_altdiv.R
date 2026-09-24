# ===========================================================================
# Divisions that belong to another division system
#
# A large part of the record stream declares a state that is not a GADM division:
# Swedish landskap and lappmarker, Watsonian vice-counties, Norwegian counties from
# after the 2018 and 2020 reforms. GNRS records these as units in their own right
# and, where it knows which GADM units they cover, writes that extent to the shared
# cache (altdiv-*). NSR reads those tables the same way it reads the GNRS reference:
# straight from the cache, without depending on the package.
#
# Only a unit whose extent is known is of any use here: it turns a declared name
# that matched nothing into the set of divisions the question is really about, so
# native status can be answered below country level. A unit with no extent yet adds
# no regions, which leaves the query where it was - answered at country level.
# ===========================================================================

#' The alternative divisions whose extent is known, keyed by country and name
#'
#' Internal.  NULL when the component has not been built.  Cached per directory
#' for the session, like the other reference tables.
#' @keywords internal
#' @noRd
nsr_altdiv <- function(dir) {
  key <- paste0("altdiv:", dir)
  if (!is.null(nsr_session[[key]])) return(nsr_session[[key]])
  f <- function(x) file.path(dir, paste0("altdiv-", x, ".gz.parquet"))
  if (!all(file.exists(f(c("units", "names", "extent"))))) return(NULL)
  units <- as.data.frame(nanoparquet::read_parquet(f("units")))
  names_ <- as.data.frame(nanoparquet::read_parquet(f("names")))
  extent <- as.data.frame(nanoparquet::read_parquet(f("extent")))
  # only exact names, and only units that have an extent: a regex system (the
  # vice-counties) has none yet, and a unit without one adds no regions
  names_ <- names_[names_$match == "exact" & names_$entity_key %in% extent$entity_key, , drop = FALSE]
  iso <- units$country_iso[match(names_$entity_key, units$entity_key)]
  out <- list(
    key = paste(toupper(iso), tolower(trimws(names_$name)), sep = "\u001f"),
    entity = names_$entity_key,
    extent = split(extent$gid, extent$entity_key),
    level = vapply(split(extent$level, extent$entity_key), function(x) as.integer(x[1]), integer(1))
  )
  nsr_session[[key]] <- out
  out
}

#' Region keys for a declared division that belongs to another system
#'
#' Internal.  Vectorised over rows; returns a list of character vectors, empty
#' where the declared name is not such a division or its extent is unknown.
#' @keywords internal
#' @noRd
nsr_altdiv_keys <- function(country_iso, name, dir) {
  n <- length(name)
  out <- vector("list", n)
  for (i in seq_len(n)) out[[i]] <- character(0)
  a <- nsr_altdiv(dir)
  if (is.null(a)) return(out)
  nm <- tolower(trimws(ifelse(is.na(name), "", name)))
  cc <- toupper(ifelse(is.na(country_iso), "", country_iso))
  hit <- match(paste(cc, nm, sep = "\u001f"), a$key)
  for (i in which(!is.na(hit))) {
    k <- a$entity[hit[i]]
    gid <- a$extent[[k]]
    # NSR's regions are countries and states; a county-level extent has nothing to map
    # onto yet, so it is skipped rather than answered at the wrong level
    if (!length(gid) || a$level[[k]] != 1L) next
    out[[i]] <- paste0("gadm1:", gid)
  }
  out
}

#' Build the local native-status reference
#'
#' Downloads (or reads) each checklist source, resolves its names against WCVP with
#' \code{TNRS::TNRS_local()} and its political divisions against the GNRS backbone, and
#' writes the shared cache tables.  Sources are fetched on the user's machine; nothing
#' derived ships with the package.
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
#' @param files Named list of local archives to read instead of downloading, e.g.
#'   \code{list(flbr = "flbr_dwca.zip")}.  For \code{powo}, the WCVP zip; if absent, the
#'   copy in the TNRS cache is used when there is one.
#' @param overwrite Rebuild sources that are already built?
#' @param quiet Suppress progress messages?
#' @return Invisibly, \code{NSR_local_status()}.
#' @export
NSR_local_build <- function(sources = c("powo", "vascan", "flbr"),
                            dir = nsr_cache_dir(create = TRUE),
                            files = list(), overwrite = FALSE, quiet = FALSE) {
  for (pkg in c("nanoparquet", "TNRS")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Building the local NSR needs the '", pkg, "' package.", call. = FALSE)
    }
  }
  reg <- nsr_builtin_registry()
  sources <- match.arg(sources, names(reg), several.ok = TRUE)
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
      powo = nsr_import_powo(files$powo, bb, quiet),
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
  chk <- lapply(names(raw), function(s) {
    d <- raw[[s]]
    fill <- is.na(d$taxon_id)
    d$taxon_id[fill] <- res$taxon_id[match(d$taxon_name[fill], res$name)]
    d <- d[!is.na(d$taxon_id) & !is.na(d$region_key), , drop = FALSE]
    data.frame(taxon_id = d$taxon_id, region_key = d$region_key, status = d$status,
               is_cultivated = d$is_cultivated, source_name = s, stringsAsFactors = FALSE)
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

  # spatial links between geographies, where GVS's GADM index is available
  systems <- unique(regions$system)
  if (length(systems) > 1 && file.exists(file.path(dir, "gadmindex-units-30s.tif"))) {
    msg("Linking region systems (", paste(systems, collapse = ", "), ") ...")
    try(nsr_build_region_links(dir = dir, quiet = quiet), silent = FALSE)
  } else if (length(systems) > 1) {
    warning("Sources use several geographies but GVS's GADM index is not in the cache, ",
            "so they cannot be linked; queries by division name will only see their own system.",
            call. = FALSE)
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

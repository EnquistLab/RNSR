#' Determine native status without an internet connection
#'
#' Offline Native Status Resolver.  For each taxon and place, the status every built
#' checklist gives it, reduced to one answer.
#'
#' Output carries \code{\link{NSR}}'s columns, and adds: \code{isEndemic};
#' \code{native_status_conflict} and \code{native_status_conflict_type}, saying whether
#' and how the sources disagreed; \code{native_status_scope}, which of the polygons
#' bearing on the place produced the answer; \code{n_subpolygons_native} and
#' \code{n_subpolygons_introduced}; \code{native_status_opinions}, every opinion consulted;
#' \code{native_status_coordinates}, \code{native_status_names} and \code{place_conflict}
#' (see Place); \code{regions_matched}, what the place was matched on; and
#' \code{taxon_evaluable}, whether any source holds native status for the taxon at all -
#' which is what separates \code{A} (absent) from \code{UNK} (no data).  The coordinates
#' given are echoed as \code{latitude} and \code{longitude}.
#'
#' \strong{Place.}  By default the answer comes from political division names
#' (\code{country}, \code{state_province}, \code{county_parish}), which are resolved to
#' GADM units through the GNRS backbone and carried to each source's own geography by the
#' spatial link table.  Any \code{latitude} and \code{longitude} present are echoed but
#' not used: placing every record by point costs a raster lookup per call, and turning
#' coordinates into political divisions is what GVS already does.  Resolve them there and
#' pass the divisions here, or set \code{use_coordinates = TRUE} to have this function do
#' it.
#'
#' With \code{use_coordinates = TRUE}, a record is placed by its point in each source's
#' own geography (WCVP against WGSRPD level-3 areas, VASCAN and Flora do Brasil against
#' GADM states), with no crosswalk between them - the finer question, where the data
#' supports it.  Given coordinates and names together, the two are independent claims
#' about where the record is and either can be wrong: a transposed longitude and a
#' mistyped province are equally easy mistakes, so neither is authoritative.  Where they
#' agree, both are used.  Where they disagree, \code{place_conflict} is \code{TRUE},
#' \code{native_status} is \code{UNK}, and the two bases are answered separately in
#' \code{native_status_coordinates} and \code{native_status_names} for you to judge.  The
#' service has no combined mode to follow here: \code{\link{NSR}} takes names and
#' \code{\link{NSR_from_coordinates}} takes coordinates.
#'
#' \strong{Precedence.}  If any source says native, the answer is native: asserting
#' nativity is a positive claim, whereas "introduced" and "absent" are often artefacts of
#' a list's scope, age or purpose.  Disagreement is recorded, not hidden, in
#' \code{native_status_conflict} and \code{native_status_opinions}.
#'
#' \strong{Which polygons answer.}  A place is judged by the polygons it lies IN.  An
#' opinion about a polygon containing the place applies to it (POWO's finest statement
#' about Guadeloupe is "native in the Leeward Islands"), and among those any native
#' opinion wins.  Polygons INSIDE the place describe only parts of it, so they are not its
#' status: they answer only when they all agree, and otherwise the answer is \code{P} with
#' the reason saying the status varies and how many sub-polygons say what
#' (\code{n_subpolygons_native}, \code{n_subpolygons_introduced}).  Give coordinates and
#' the question does not arise: the record is judged on the ground it sits on.
#' \code{native_status_scope} records which of these produced the answer.
#'
#' \strong{Endemism is the exception}, deliberately: \code{Ne} and \code{Ie} are claims
#' about the taxon's whole range rather than about one polygon, so they draw on evidence
#' from elsewhere.  A record of a taxon confined to California, found in Michigan, is
#' introduced there however little Michigan's own checklists say.
#' @param occurrence_dataframe A data.frame with \code{species}, and either coordinates
#'   or political division names (see Place).
#' @param dir Cache directory, shared with GNRS and GVS.
#' @param resolve_names Resolve submitted names against WCVP with
#'   \code{TNRS::TNRS_local()}?  Names already matching WCVP accepted names need none.
#' @param use_coordinates Place each record by its \code{latitude} and \code{longitude}
#'   as well as by its division names?  Default \code{FALSE}: coordinates cost a raster
#'   lookup per call, and resolving them to political divisions is GVS's job.  See Place.
#' @param min_overlap Ignore region links covering less than this share of a region.
#' @param exclude_extinct Ignore distribution records the source marks extinct?  Default
#'   \code{TRUE}, which answers about the present day.  Set \code{FALSE} to model a past
#'   distribution, where a region the taxon has since been lost from still counts.  See
#'   Extinct records.
#' @param quiet Suppress progress messages?
#' @return A data.frame, one row per input row, carrying \code{user_id} from the input
#'   (or sequential ids where the input has none), as \code{\link{NSR}} does.
#' @section Extinct records:
#' WCVP marks a distribution extinct with a bare flag and no date, so the record says
#' only "considered no longer present as of this release".  Whether it should count is
#' therefore a property of the question rather than of the data: a present-day status
#' wants it out, a historical distribution wants it in, and the cache keeps both so
#' \code{exclude_extinct} can decide per call.
#'
#' Excluding it never makes the taxon introduced there.  The endemism rules read the
#' taxon's whole native range, extinct records included, whatever this argument says -
#' a region a taxon has been lost from is still a region it was native to, so \code{Ie}
#' ("introduced, inferred from endemism elsewhere") cannot fire against it.  The answer
#' for such a record is \code{A}: gone, not foreign.
#' @section County and parish:
#' \code{county_parish} is accepted and echoed, but not resolved: the political-division
#' backbone stops at state and province, so \code{native_status_county_parish} is always
#' \code{NA} and the answer is given at the finest level that could be matched.  A
#' warning says so.  Coordinates are the way to ask a finer question.
#' @export
NSR_local <- function(occurrence_dataframe, dir = nsr_cache_dir(), resolve_names = TRUE,
                      use_coordinates = FALSE, min_overlap = 0.01, exclude_extinct = TRUE,
                      quiet = FALSE) {
  if (!inherits(occurrence_dataframe, "data.frame")) {
    stop("occurrence_dataframe should be a data.frame", call. = FALSE)
  }
  x <- occurrence_dataframe
  col <- function(nm) if (nm %in% names(x)) as.character(x[[nm]]) else rep(NA_character_, nrow(x))
  num <- function(nm) if (nm %in% names(x)) suppressWarnings(as.numeric(x[[nm]])) else rep(NA_real_, nrow(x))
  species <- col("species")
  country <- col("country")
  state <- col("state_province")
  county <- col("county_parish")
  lon <- num("longitude")
  lat <- num("latitude")
  n <- nrow(x)

  # user_id on NSR()'s terms, so a result can be joined back to its input either way
  user_id <- if ("user_id" %in% names(x)) x[["user_id"]] else rep(NA, n)
  if (all(is.na(user_id))) user_id <- seq_len(n)
  if (any(duplicated(user_id))) {
    stop("user_id should be either null or populated by unique values", call. = FALSE)
  }

  # No county polygons exist offline, so county_parish cannot be answered.  The service
  # answers it, so say plainly that this one does not rather than returning a silent NA.
  if (any(!is.na(county) & nzchar(county))) {
    warning("county_parish is not resolved offline: the backbone carries no county ",
            "polygons, so native_status_county_parish is NA and the answer is given at ",
            "the finest level that could be matched. Give coordinates for a finer answer.",
            call. = FALSE)
  }

  db <- nsr_local_db(dir)

  # ---- taxa -------------------------------------------------------------------------
  # WCVP records distributions for genera too, so a genus query is answerable directly.
  # What must not happen is a bare genus being RESOLVED to some species of that genus,
  # which would answer confidently about the wrong taxon.
  above_species <- !is.na(species) & nzchar(species) &
    lengths(strsplit(trimws(species), "[[:space:]]+")) < 2
  taxon_id <- db$name_index$taxon_id[match(species, db$name_index$species_name)]
  need <- unique(species[is.na(taxon_id) & !is.na(species) & nzchar(species) & !above_species])
  if (resolve_names && length(need)) {
    if (!quiet) message("Resolving ", format(length(need), big.mark = ","), " names ...")
    r <- nsr_resolve_names(need, quiet = quiet, species_index = db$name_index)
    taxon_id[is.na(taxon_id)] <- r$taxon_id[match(species[is.na(taxon_id)], r$name)]
  }

  # ---- place ------------------------------------------------------------------------
  if (!use_coordinates && any(is.finite(lon) & is.finite(lat)) && !quiet) {
    message("Coordinates present but not used: answering from division names. ",
            "Set use_coordinates = TRUE to place each record by its point instead.")
  }
  places <- nsr_query_regions(lon, lat, country, state, county, dir, db, use_coordinates)

  # ---- opinions ---------------------------------------------------------------------
  use_set <- !is.null(db$chk_dt) && nrow(occurrence_dataframe) >= getOption("NSR.set_min", 200)
  resolve <- function(tid, pl) if (use_set)
    nsr_resolve_status_set(tid, pl, db, min_overlap, exclude_extinct) else
    nsr_resolve_status(tid, pl, db, min_overlap, exclude_extinct)
  res <- resolve(taxon_id, places)

  # ---- records whose two statements of place disagree ---------------------------------
  # Neither basis is taken as authoritative, so the pooled answer - which drew on
  # evidence for both places and describes neither - is withdrawn, and each basis is
  # answered on its own.  The two columns are populated only for these rows; elsewhere
  # they are NA because there is nothing to compare, and native_status is the answer.
  status_xy <- rep(NA_character_, n)
  status_nm <- rep(NA_character_, n)
  cf <- places$conflict
  if (any(cf)) {
    status_xy[cf] <- resolve(taxon_id[cf], nsr_split_keys(places$xy[cf]))$code
    status_nm[cf] <- resolve(taxon_id[cf], nsr_split_keys(places$nm[cf]))$code
    res$code[cf] <- "UNK"
    res$reason[cf] <- paste0(
      "Coordinates and division names resolve to different places, so no single answer ",
      "is given; see native_status_coordinates and native_status_names")
    res$scope[cf] <- "none"
    res$sources[cf] <- NA_character_
    res$opinions[cf] <- NA_character_
    res$country_code[cf] <- NA_character_
    res$state_code[cf] <- NA_character_
    res$cultivated[cf] <- NA_integer_
    res$conflict[cf] <- FALSE
    res$conflict_type[cf] <- "none"
    res$n_sub_native[cf] <- 0L
    res$n_sub_introduced[cf] <- 0L
    if (!quiet) message(sum(cf), " record(s) whose coordinates and division names ",
                        "disagree; see place_conflict.")
  }

  out <- data.frame(
    family = db$taxa$family[match(taxon_id, db$taxa$taxon_id)],
    genus = db$taxa$genus[match(taxon_id, db$taxa$taxon_id)],
    species = species,
    country = country, state_province = state, county_parish = county,
    latitude = lat, longitude = lon,
    poldiv_full = places$label,
    poldiv_type = places$level,
    native_status_country = res$country_code,
    native_status_state_province = res$state_code,
    native_status_county_parish = rep(NA_character_, n),
    native_status = res$code,
    native_status_reason = res$reason,
    native_status_sources = res$sources,
    isIntroduced = as.integer(res$code %in% c("I", "Ie")),
    isEndemic = as.integer(res$code == "Ne"),
    isCultivatedNSR = res$cultivated,
    # NA, not production's 0: the flag is dropped (a taxon-level use flag with no
    # polygon attached says nothing about the record in hand) and a never-populated
    # column should not read as a real negative. See dev_notes/01-offline-nsr-design.md,
    # open question 3. isCultivatedNSR above is the version that has a geography.
    is_cultivated_taxon = NA_integer_,
    native_status_conflict = res$conflict,
    native_status_conflict_type = res$conflict_type,
    native_status_scope = res$scope,
    n_subpolygons_native = res$n_sub_native,
    n_subpolygons_introduced = res$n_sub_introduced,
    native_status_opinions = res$opinions,
    native_status_coordinates = status_xy,
    native_status_names = status_nm,
    place_conflict = places$conflict,
    regions_matched = places$matched,
    taxon_evaluable = !is.na(taxon_id) & taxon_id %in% db$evaluable,
    user_id = user_id,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

nsr_session <- new.env(parent = emptyenv())

#' Load the local tables once per call
#' @keywords internal
#' @noRd
nsr_local_db <- function(dir) {
  nsr_need("nanoparquet")
  stamp <- paste(normalizePath(dir), file.mtime(nsr_table_path("checklist", dir)),
                 file.mtime(nsr_table_path("region-links", dir)))
  hit <- nsr_session$db
  if (!is.null(hit) && identical(hit$stamp, stamp)) return(hit$value)
  need <- c("sources", "taxa", "regions", "checklist")
  miss <- vapply(need, function(t) !file.exists(nsr_table_path(t, dir)), logical(1))
  if (any(miss)) {
    stop("The local NSR is not built in ", dir, ".\nRun NSR_local_build().", call. = FALSE)
  }
  db <- lapply(need, function(t) as.data.frame(nanoparquet::read_parquet(nsr_table_path(t, dir))))
  names(db) <- need
  lf <- nsr_table_path("region-links", dir)
  db$links <- if (file.exists(lf)) as.data.frame(nanoparquet::read_parquet(lf)) else
    data.frame(from_region = character(0), to_region = character(0), relation = character(0),
               fraction = numeric(0), stringsAsFactors = FALSE)
  db <- nsr_index_db(db)
  nsr_session$db <- list(stamp = stamp, value = db)
  db
}

#' Derive every lookup index the resolvers use from the four cache tables
#'
#' Internal.  Split out from \code{nsr_local_db()} so the resolvers can be exercised on
#' tables held in memory, without a built cache: the row path and the set path must give
#' the same answer for the same query, and that is only testable if a db can be made
#' without a multi-gigabyte build.
#' @keywords internal
#' @noRd
nsr_index_db <- function(db) {
  # Caches built before extinct records were retained have no such column; they simply
  # hold no extinct rows, so 0 is the truth for every row they do hold.
  if (is.null(db$checklist$is_extinct)) db$checklist$is_extinct <- 0L
  db$link_idx <- split(seq_len(nrow(db$links)), db$links$from_region)
  db$chk_idx <- split(seq_len(nrow(db$checklist)),
                      paste(db$checklist$taxon_id, db$checklist$region_key))
  db$chk_env <- list2env(db$chk_idx, envir = new.env(hash = TRUE, parent = emptyenv()))
  db$link_env <- list2env(db$link_idx, envir = new.env(hash = TRUE, parent = emptyenv()))
  # GADM's key carries its own hierarchy: gid_1 "BRA.25_1" sits in gid_0 "BRA".  That
  # containment is exact by construction, which is why it is kept OUT of the link table:
  # link rows carry overlap fractions and are filtered by min_overlap, and a state is a
  # legitimately tiny share of its country (Distrito Federal is 0.07% of Brazil), so as
  # a fraction it would be discarded as a sliver.  Only regions that actually carry
  # checklist rows are listed, since those are the only ones an answer can draw on.
  g1 <- unique(db$regions$region_key[db$regions$system == "gadm1"])
  g1 <- g1[g1 %in% db$checklist$region_key]
  parent <- if (length(g1)) paste0("gadm0:", sub("\\..*$", "", sub("^gadm1:", "", g1))) else character(0)
  db$gadm_parent <- stats::setNames(parent, g1)
  db$gadm_children <- if (length(g1)) split(g1, parent) else list()
  if (requireNamespace("data.table", quietly = TRUE)) {
    db$gadm_edges_dt <- if (length(g1)) {
      data.table::data.table(from_region = c(g1, parent), to_region = c(parent, g1),
                             relation = rep(c("within", "contains"), each = length(g1)))
    } else {
      data.table::data.table(from_region = character(0), to_region = character(0),
                             relation = character(0))
    }
    data.table::setkeyv(db$gadm_edges_dt, "from_region")
    db$chk_dt <- data.table::as.data.table(db$checklist)
    data.table::setkeyv(db$chk_dt, c("taxon_id", "region_key"))
    db$links_dt <- data.table::as.data.table(db$links)
    data.table::setkeyv(db$links_dt, "from_region")
    nat <- unique(db$chk_dt[status == "native", list(taxon_id, region_key)])
    db$native_dt <- nat
    data.table::setkeyv(db$native_dt, "taxon_id")
    db$confined_dt <- nsr_confined_ranges(nat, db)
  }
  # WCVP sometimes carries the same name at two ranks (a variety row also called
  # "Pinus ponderosa"), and only one of them holds the distributions: index names to the
  # species-rank id, and among those to the one with opinions
  tx <- db$taxa
  tx$is_species <- tolower(tx$rank) %in% c("species", "")
  tx$has_rows <- tx$taxon_id %in% db$checklist$taxon_id
  tx <- tx[order(!tx$has_rows, !tx$is_species), , drop = FALSE]
  db$name_index <- tx[!duplicated(tx$species_name), c("species_name", "taxon_id")]
  db$chk_status <- db$checklist$status
  db$chk_source <- db$checklist$source_name
  db$chk_cult <- db$checklist$is_cultivated
  db$chk_extinct <- as.integer(db$checklist$is_extinct) %in% 1L
  db$chk_region <- db$checklist$region_key
  db$link_to <- db$links$to_region
  db$link_rel <- db$links$relation
  db$link_frac <- db$links$fraction
  db$chk_idx_taxon <- split(seq_len(nrow(db$checklist)), db$checklist$taxon_id)
  db$evaluable <- unique(db$checklist$taxon_id)
  db$comprehensive <- db$sources$source_name[db$sources$is_comprehensive %in% TRUE]
  db$covered <- unique(db$checklist$region_key[db$checklist$source_name %in% db$comprehensive])
  db
}

#' The regions a query refers to, in every system
#'
#' Internal.  With coordinates, each system is looked up directly.  With names, the GADM
#' unit comes from the GNRS backbone and other systems are reached through the link
#' table.  Returns, per row, the regions to consult and how each relates to the query.
#' @keywords internal
#' @noRd
nsr_query_regions <- function(lon, lat, country, state, county, dir, db,
                              use_coordinates = FALSE) {
  n <- length(lon)
  direct <- vector("list", n)
  fine <- vector("list", n)
  ctry <- vector("list", n)
  xy <- vector("list", n)
  nm <- vector("list", n)
  conflict <- rep(FALSE, n)
  label <- rep(NA_character_, n)
  level <- rep("country", n)
  matched <- rep("none", n)

  loc <- if (use_coordinates && any(is.finite(lon) & is.finite(lat)))
    nsr_locate_regions(lon, lat, dir) else NULL
  bb <- try(nsr_gnrs_backbone(dir), silent = TRUE)
  pd <- if (!inherits(bb, "try-error")) nsr_match_poldiv(country, state, bb) else NULL
  pd0 <- if (!inherits(bb, "try-error")) nsr_match_poldiv(country, NULL, bb) else NULL
  # a declared state that belongs to another division system (a Norwegian county from
  # after the 2018 or 2020 reform, a lan under the name records write) contributes the
  # GADM units it covers, so the question is answered below country level instead of
  # falling back to the country
  iso0 <- if (!is.null(pd0)) bb$country$iso[match(pd0$country_id, bb$country$country_id)] else rep(NA_character_, n)
  alt_keys <- nsr_altdiv_keys(iso0, state, dir)

  at <- function(k, p) grep(p, k, value = TRUE)
  for (i in seq_len(n)) {
    kx <- if (!is.null(loc))
      as.character(stats::na.omit(c(loc$wgsrpd3[i], loc$gadm[i], loc$gadm0[i]))) else character(0)
    kn <- character(0)
    if (!is.null(pd)) {
      gid1 <- if (!is.na(pd$state_province_id[i]))
        bb$state$gid_1[match(pd$state_province_id[i], bb$state$state_province_id)] else NA_character_
      gid0 <- if (!is.na(pd0$country_id[i]))
        bb$country$gid_0[match(pd0$country_id[i], bb$country$country_id)] else NA_character_
      if (!is.na(gid1)) kn <- c(kn, paste0("gadm1:", gid1))
      if (is.na(gid1) && length(alt_keys[[i]])) kn <- c(kn, alt_keys[[i]])
      if (!is.na(gid0)) kn <- c(kn, paste0("gadm0:", gid0))
    }
    xy[[i]] <- kx
    nm[[i]] <- kn
    conflict[i] <- !nsr_places_agree(kx, kn)
    keys <- unique(c(kx, kn))
    keys <- keys[!is.na(keys)]
    # the finest place the query actually names, kept apart from its country: a question
    # about Amazonas must not inherit Brazil's answer
    ctry[[i]] <- at(keys, "^gadm0:")
    fine[[i]] <- grep("^gadm0:", keys, value = TRUE, invert = TRUE)
    direct[[i]] <- keys
    has_state <- any(grepl("^gadm1:", keys)) || (!is.null(pd) && !is.na(pd$state_province_id[i]))
    level[i] <- if (has_state) "state_province" else "country"
    matched[i] <- if (!length(keys)) "none" else if (conflict[i]) "conflict" else
      if (!is.null(loc) && !is.na(loc$gadm[i])) "coordinates" else "names"
    label[i] <- paste(stats::na.omit(c(country[i], if (!is.na(state[i]) && nzchar(state[i])) state[i])),
                      collapse = ":")
    if (!nzchar(label[i]) && length(keys)) label[i] <- paste(keys, collapse = " + ")
  }
  list(direct = direct, fine = fine, country = ctry, xy = xy, nm = nm,
       conflict = conflict, label = label, level = level, matched = matched)
}

#' Do coordinates and division names describe the same place?
#'
#' Internal.  Coordinates and names are two independent claims about where a record is,
#' and either can be wrong: a transposed longitude and a mistyped province are equally
#' easy mistakes, so neither is authoritative.  They are compared in the geography they
#' share, GADM, and at each level separately, so a right country with a wrong state is
#' caught as readily as a wrong country.  A level only one of them speaks to is not a
#' disagreement.  Where they agree, pooling the keys costs nothing and the wider set
#' answers; where they do not, \code{\link{NSR_local}} answers each on its own.
#' @keywords internal
#' @noRd
nsr_places_agree <- function(kx, kn) {
  lvl <- function(p) {
    a <- grep(p, kx, value = TRUE); b <- grep(p, kn, value = TRUE)
    !length(a) || !length(b) || identical(sort(a), sort(b))
  }
  lvl("^gadm0:") && lvl("^gadm1:")
}

#' Split a set of region keys into the finest level named and its country
#' @keywords internal
#' @noRd
nsr_split_keys <- function(keys) {
  list(fine = lapply(keys, function(z) grep("^gadm0:", z, value = TRUE, invert = TRUE)),
       country = lapply(keys, function(z) grep("^gadm0:", z, value = TRUE)))
}

#' Gather and reduce every opinion bearing on each row
#' @keywords internal
#' @noRd
nsr_resolve_status <- function(taxon_id, places, db, min_overlap = 0.01,
                               exclude_extinct = TRUE) {
  n <- length(taxon_id)
  blank <- rep(NA_character_, n)
  out <- list(code = blank, reason = blank, sources = blank, opinions = blank,
              country_code = blank, state_code = blank, scope = blank,
              conflict = rep(FALSE, n), conflict_type = rep(NA_character_, n),
              cultivated = rep(NA_integer_, n), n_sub_native = rep(NA_integer_, n),
              n_sub_introduced = rep(NA_integer_, n))
  for (i in seq_len(n)) {
    tid <- taxon_id[i]
    fine <- places$fine[[i]]
    ctry <- places$country[[i]]
    keys <- if (length(fine)) fine else ctry          # answer at the finest level named
    if (is.na(tid) || !length(c(fine, ctry))) {
      out$code[i] <- "UNK"
      out$reason[i] <- if (is.na(tid)) "Taxon not matched to a species in the backbone" else
        "Place not matched to any region"
      # nothing was consulted, which is a definite "no disagreement, no sub-polygons"
      # rather than an unknown; the set path says the same for these rows
      out$scope[i] <- "none"
      out$conflict_type[i] <- "none"
      out$n_sub_native[i] <- 0L
      out$n_sub_introduced[i] <- 0L
      next
    }
    op <- nsr_opinions_for(tid, keys, db, min_overlap, exclude_extinct)
    # a coarser polygon the place sits in (its country) also contains it, so opinions
    # recorded ON that polygon apply; its OTHER sub-polygons do not, so only direct rows
    # are taken, never its links
    anc <- setdiff(ctry, keys)
    if (length(anc)) {
      rows <- unlist(mget(paste(tid, anc), db$chk_env, ifnotfound = list(NULL)), use.names = FALSE)
      if (exclude_extinct && length(rows)) rows <- rows[!db$chk_extinct[rows]]
      if (length(rows)) {
        add <- list(status = db$chk_status[rows], source_name = db$chk_source[rows],
                    is_cultivated = db$chk_cult[rows], relation = rep("within", length(rows)))
        op <- if (is.null(op)) add else Map(c, op, add)
      }
    }
    # the ancestor country supplies opinions above, so absence has to be read against it
    # too, or a state with no coverage of its own inside a comprehensively listed country
    # answers UNK here and A in the set path, which reads Q's country row
    consulted <- nsr_consulted_regions(c(keys, anc), db, min_overlap)
    ev <- tid %in% db$evaluable
    r <- nsr_reduce(op, consulted, ev, db)
    if (identical(r$code, "N") && nsr_is_endemic(tid, keys, db, min_overlap)) {
      r$code <- "Ne"
      r$reason <- paste0(sub("^Native", "Native and endemic", r$reason))
    }
    if (identical(r$code, "A")) {
      ee <- nsr_endemic_elsewhere(tid, keys, db, min_overlap)
      if (!is.na(ee)) {
        r$code <- "Ie"
        r$reason <- paste0("Absent from this region and endemic to ", ee,
                           ", so introduced here (inferred)")
      }
    }
    for (f in names(r)) out[[f]][i] <- r[[f]]
    # per-level codes, for the service's columns
    out$country_code[i] <- if (length(ctry)) {
      nsr_reduce(nsr_opinions_for(tid, ctry, db, min_overlap, exclude_extinct),
                 nsr_consulted_regions(ctry, db, min_overlap), ev, db)$code
    } else NA_character_
    out$state_code[i] <- if (length(fine)) {
      nsr_reduce(nsr_opinions_for(tid, fine, db, min_overlap, exclude_extinct),
                 nsr_consulted_regions(fine, db, min_overlap), ev, db)$code
    } else NA_character_
  }
  out
}

#' The GADM units directly below, or directly above, the regions given
#'
#' Internal.  Exact containment read off the key, so no overlap fraction applies and
#' \code{min_overlap} does not filter it.  See \code{nsr_index_db()}.
#' @keywords internal
#' @noRd
nsr_gadm_children <- function(keys, db) {
  if (!length(keys) || !length(db$gadm_children)) return(character(0))
  k <- keys[startsWith(keys, "gadm0:")]
  if (!length(k)) return(character(0))
  setdiff(as.character(unlist(db$gadm_children[k], use.names = FALSE)), keys)
}

#' @keywords internal
#' @noRd
nsr_gadm_parents <- function(keys, db) {
  if (!length(keys)) return(character(0))
  k <- keys[startsWith(keys, "gadm1:")]
  p <- if (length(k) && length(db$gadm_parent))
    as.character(unname(db$gadm_parent[k])) else character(0)
  setdiff(p[!is.na(p)], keys)
}

#' The countries a set of regions lies in, a country counting as its own
#'
#' Internal.  For the confined-range test only.  A taxon native both to a country row
#' and to one of that country's states is confined to it, so gadm0 has to answer for
#' itself here - it has no parent to look up, and its links only reach other geographies.
#' @keywords internal
#' @noRd
nsr_gadm_countries <- function(keys, db) {
  unique(c(nsr_gadm_parents(keys, db), keys[startsWith(keys, "gadm0:")]))
}

#' Opinions about one taxon bearing on a set of regions
#'
#' Internal.  Direct opinions, plus opinions about regions the query's regions are inside
#' of or contain, with the relation recorded so the reducer can apply the one-way
#' inheritance rules.
#' @keywords internal
#' @noRd
nsr_opinions_for <- function(taxon_id, keys, db, min_overlap = 0.01, exclude_extinct = TRUE) {
  if (!length(keys)) return(NULL)
  live <- function(r) if (exclude_extinct && length(r)) r[!db$chk_extinct[r]] else r
  rows <- live(unlist(mget(paste(taxon_id, keys), db$chk_env, ifnotfound = list(NULL)),
                      use.names = FALSE))
  rel <- if (length(rows)) rep("same", length(rows)) else character(0)
  li <- unlist(mget(keys, db$link_env, ifnotfound = list(NULL)), use.names = FALSE)
  if (length(li)) {
    to <- db$link_to[li]; rl <- db$link_rel[li]; fr <- db$link_frac[li]
    known <- unique(sub(":.*", "", keys))        # geographies this place is located in
    good <- !(to %in% keys) & fr >= min_overlap & !(sub(":.*", "", to) %in% known)
    if (any(good)) {
      to <- to[good]; rl <- rl[good]
      idx <- lapply(mget(paste(taxon_id, to), db$chk_env, ifnotfound = list(NULL)), live)
      n <- lengths(idx)
      if (sum(n)) {
        rows <- c(rows, unlist(idx, use.names = FALSE))
        rel <- c(rel, rep(rl, n))
      }
    }
  }
  # The states inside a queried country: a source keyed on GADM states (VASCAN, Flora do
  # Brasil) is otherwise invisible to a country query, which breaks native-up propagation.
  # A state query does not come back here for its country - that is the ancestor path in
  # nsr_resolve_status(), which takes direct rows only, so a country's OTHER states never
  # reach it.
  kids <- nsr_gadm_children(keys, db)
  if (length(kids)) {
    idx <- lapply(mget(paste(taxon_id, kids), db$chk_env, ifnotfound = list(NULL)), live)
    if (sum(lengths(idx))) {
      rows <- c(rows, unlist(idx, use.names = FALSE))
      rel <- c(rel, rep("contains", sum(lengths(idx))))
    }
  }
  if (!length(rows)) return(NULL)
  list(status = db$chk_status[rows], source_name = db$chk_source[rows],
       is_cultivated = db$chk_cult[rows], relation = rel)
}

#' Is the taxon native only inside the queried place?
#'
#' Internal.  Endemism (\code{Ne}) is read off the checklist: every region any source
#' calls it native must be the queried region, inside it, or overlapping it - never a
#' region lying outside.  Only claimed when at least one source has a native opinion.
#' @keywords internal
#' @noRd
nsr_is_endemic <- function(taxon_id, keys, db, min_overlap = 0.01) {
  rows <- db$chk_idx_taxon[[as.character(taxon_id)]]
  if (is.null(rows)) return(FALSE)
  nat <- db$chk_region[rows][db$chk_status[rows] == "native"]
  if (!length(nat)) return(FALSE)
  li <- unlist(mget(keys, db$link_env, ifnotfound = list(NULL)), use.names = FALSE)
  inside <- if (!length(li)) keys else
    c(keys, db$link_to[li][db$link_rel[li] %in% c("same", "contains") & db$link_frac[li] >= min_overlap])
  inside <- c(inside, nsr_gadm_children(keys, db))
  all(nat %in% inside)
}

#' Is the taxon endemic to somewhere else?
#'
#' Internal.  The service's `Ie`: a taxon absent from the queried region but whose whole
#' native range lies elsewhere and is confined - endemic to one region, or to one country -
#' cannot be native here, so an occurrence must be introduced.  A widely native species
#' merely unrecorded here stays `A`: absence is not evidence of introduction unless the
#' species could not have been native in the first place.
#' @return The name of the region it is endemic to, or NA.
#' @keywords internal
#' @noRd
nsr_endemic_elsewhere <- function(taxon_id, keys, db, min_overlap = 0.01) {
  rows <- db$chk_idx_taxon[[as.character(taxon_id)]]
  if (is.null(rows)) return(NA_character_)
  nat <- unique(db$chk_region[rows][db$chk_status[rows] == "native"])
  if (!length(nat)) return(NA_character_)
  if (any(nat %in% nsr_consulted_regions(keys, db, min_overlap))) return(NA_character_)
  nm <- function(k) {
    i <- match(k, db$regions$region_key)
    if (is.na(i)) k else db$regions$region_name[i]
  }
  if (length(nat) == 1) return(nm(nat))
  containers <- lapply(nat, function(r) {
    li <- db$link_env[[r]]
    if (is.null(li)) return(nsr_gadm_countries(r, db))
    keep <- db$link_rel[li] %in% c("same", "within") & db$link_frac[li] >= min_overlap &
      startsWith(db$link_to[li], "gadm0:")
    c(db$link_to[li][keep], nsr_gadm_countries(r, db))
  })
  common <- Reduce(intersect, containers)
  if (length(common)) nm(common[1]) else NA_character_
}

#' Every region an answer may draw on: the query's own, and those linked to them
#'
#' Internal.  Used to decide whether absence is interpretable: a query about California
#' finds no source publishing against GADM California, but WGSRPD's CAL is the same place
#' and is comprehensively listed, so absence there does mean something.
#'
#' \code{min_overlap} applies here for the same reason it applies to opinions: a link
#' covering a sliver of a region is not that region being listed, and letting one through
#' would turn "no opinion" into \code{A}, or suppress an \code{Ie}, on the strength of a
#' shared coastline.  The set path filters its links once, when it builds them.
#' @keywords internal
#' @noRd
nsr_consulted_regions <- function(keys, db, min_overlap = 0.01) {
  if (!length(keys)) return(character(0))
  li <- unlist(mget(keys, db$link_env, ifnotfound = list(NULL)), use.names = FALSE)
  if (!length(li)) return(unique(c(keys, nsr_gadm_children(keys, db))))
  keep <- db$link_rel[li] %in% c("same", "within", "contains") & db$link_frac[li] >= min_overlap
  unique(c(keys, db$link_to[li][keep], nsr_gadm_children(keys, db)))
}

#' What kind of disagreement is this?
#'
#' Internal.  Two quite different things look like conflict.  Sources can genuinely
#' disagree about a polygon (VASCAN calls a plant introduced in Ontario where POWO calls it
#' native).  Or one source can hold both statuses for the same polygon because WCVP records
#' one subspecies as native and another as introduced, and we roll infraspecific taxa up to
#' the species - no one disagrees with anyone there.  The flag says which.
#' @return "none", "sources", "within source" or "both".
#' @keywords internal
#' @noRd
nsr_conflict_type <- function(status, source) {
  if (!length(status)) return("none")
  by_src <- split(status, source)
  within <- any(vapply(by_src, function(z) length(unique(z)) > 1, logical(1)))
  sets <- vapply(by_src, function(z) paste(sort(unique(z)), collapse = "+"), character(1))
  between <- length(unique(sets)) > 1
  if (within && between) "both" else if (between) "sources" else if (within) "within source" else "none"
}

#' Reduce opinions to one status code
#'
#' Internal.  A place is judged by the polygons it lies IN: an opinion about a polygon
#' containing it applies to it, and among those, any native opinion wins (BM's
#' precedence).  Polygons INSIDE the place describe only parts of it, so they are not
#' its status: they answer only when they all agree, and otherwise the answer is that it
#' depends where the record falls - report `P` and say so.  Polygons merely overlapping
#' describe neither.  This keeps a record's verdict tied to the evidence for the ground
#' it actually sits on, which is what coordinates give directly.
#'
#' Internal.  Inheritance: an opinion about a region the query sits INSIDE ("within"
#' from the query's side) carries introduced downward but not native; an opinion about a
#' region inside the query ("contains") carries native upward but not introduced;
#' partial overlap carries neither.  Then: any native wins, else introduced, else
#' present; else absent where a comprehensive source covers the region and the taxon is
#' evaluable, else unknown.
#' @keywords internal
#' @noRd
nsr_reduce <- function(op, keys, evaluable, db) {
  if (is.null(op)) {
    st <- src <- rel <- character(0); cult <- integer(0)
  } else {
    st <- op$status; src <- op$source_name; rel <- op$relation; cult <- op$is_cultivated
  }
  opinions <- if (!length(st)) NA_character_ else
    paste(paste0(src, ":", st, ifelse(rel == "same", "", paste0("(", rel, ")"))), collapse = "; ")
  cultivated <- if (!length(cult)) NA_integer_ else as.integer(any(cult %in% c(1, "1", TRUE)))
  inc <- rel %in% c("same", "within")        # polygons the place lies in
  sub <- rel == "contains"                   # polygons inside the place
  ovl <- rel == "overlaps"
  n_sub_n <- sum(sub & st == "native"); n_sub_i <- sum(sub & st == "introduced")
  out <- function(code, reason, scope, srcs = character(0), conflict = FALSE,
                  conflict_type = "none") {
    list(code = code, reason = reason, scope = scope,
         sources = if (!length(srcs)) NA_character_ else paste(sort(unique(srcs)), collapse = ", "),
         opinions = opinions, conflict = conflict, conflict_type = conflict_type,
         cultivated = cultivated, n_sub_native = n_sub_n, n_sub_introduced = n_sub_i)
  }
  if (any(inc)) {
    si <- st[inc]
    ct <- nsr_conflict_type(si, src[inc])
    code <- if ("native" %in% si) "N" else if ("introduced" %in% si) "I" else "P"
    want <- c(N = "native", I = "introduced", P = "present")[[code]]
    stated <- any(rel == "same" & st == want)
    word <- c(N = "Native", I = "Introduced", P = "Present")[[code]]
    reason <- if (ct != "none" && code == "N") {
      paste0("Native to this polygon as per checklist (", ct, " disagree; any native opinion is taken)")
    } else if (stated) paste0(word, " in this polygon, as per checklist")
    else paste0(word, " in a polygon containing this place, as per checklist")
    return(out(code, reason, if (stated) "polygon" else "containing polygon", src[inc],
               ct != "none", ct))
  }
  if (any(sub)) {
    ss <- unique(st[sub])
    if (length(ss) == 1) {
      code <- c(native = "N", introduced = "I", present = "P")[[ss]]
      word <- c(native = "Native", introduced = "Introduced", present = "Present")[[ss]]
      return(out(code, paste0(word, " in every listed polygon within this place, as per checklist"),
                 "sub-polygons agree", src[sub]))
    }
    return(out("P", paste0("Status varies among the polygons within this place (", n_sub_n,
                           " native, ", n_sub_i, " introduced); give coordinates or a finer division"),
               "sub-polygons differ", src[sub]))
  }
  if (any(ovl)) {
    return(out("UNK", "Only polygons partly overlapping this place have a status; none covers it",
               "overlapping only", src[ovl]))
  }
  if (!evaluable) return(out("UNK", "No source holds native status information for this taxon", "none"))
  if (any(keys %in% db$covered)) return(out("A", "Absent from the comprehensive checklists for this polygon", "polygon"))
  out("UNK", "No comprehensive checklist covers this polygon", "none")
}

# data.table is used inside the package without being attached; this tells it the
# package knows what it is doing, so [.data.table keeps its own semantics here.
.datatable.aware <- TRUE

# Set-based resolution: every query answered in a handful of joins rather than one pass
# per row. Same semantics as the row path in NSR_local.R - a place is judged by the
# polygons containing it, sub-polygons answer only when unanimous, endemism is the
# exception that may look outside - but it scales to the millions of taxon x polygon
# pairs an occurrence pipeline asks about.

#' Which taxa have a native range confined to one region or one country
#'
#' Internal.  Computed once per build, for the `Ie` rule: a taxon absent from the place
#' queried but confined elsewhere cannot be native there.  A taxon native in a single
#' region is confined to it; otherwise it is confined to a country if every region it is
#' native in lies inside that one country.
#' @keywords internal
#' @noRd
nsr_confined_ranges <- function(nat, db) {
  dt <- data.table::data.table
  cnt <- nat[, list(n_nat = .N), by = "taxon_id"]
  single <- merge(nat, cnt[n_nat == 1L], by = "taxon_id")
  nm <- function(k) {
    i <- match(k, db$regions$region_key)
    data.table::fifelse(is.na(i), k, db$regions$region_name[i])
  }
  out <- dt(taxon_id = single$taxon_id, confined_to = nm(single$region_key))
  multi <- cnt[n_nat > 1L]
  if (nrow(multi)) {
    lk <- db$links_dt[relation %in% c("same", "within") &
                        startsWith(to_region, "gadm0:"),
                      list(region_key = from_region, country = to_region)]
    if (!is.null(db$gadm_edges_dt) && nrow(db$gadm_edges_dt)) {
      lk <- unique(data.table::rbindlist(list(
        lk,
        db$gadm_edges_dt[relation == "within",
                         list(region_key = from_region, country = to_region)]),
        use.names = TRUE))
    }
    # a country row is its own country: a taxon native both to gadm0:CAN and to one of
    # Canada's states is confined to Canada, and without this it has no common container
    own <- unique(nat$region_key[startsWith(nat$region_key, "gadm0:")])
    if (length(own)) {
      lk <- unique(data.table::rbindlist(
        list(lk, dt(region_key = own, country = own)), use.names = TRUE))
    }
    m <- merge(nat[taxon_id %in% multi$taxon_id], lk, by = "region_key", allow.cartesian = TRUE)
    per <- m[, list(n_in = data.table::uniqueN(region_key)), by = c("taxon_id", "country")]
    per <- merge(per, multi, by = "taxon_id")
    conf <- per[n_in == n_nat]
    conf <- conf[!duplicated(conf$taxon_id)]
    if (nrow(conf)) out <- data.table::rbindlist(list(out,
      dt(taxon_id = conf$taxon_id, confined_to = nm(conf$country))))
  }
  data.table::setkeyv(out, "taxon_id")
  out
}

#' Native status for taxa and polygons you have already resolved
#'
#' A pipeline that has located its records itself - in the WGSRPD raster and the GADM
#' index, as an occurrence workflow does once for all of its coordinates - already knows
#' the polygons each record sits in.  This takes those directly, skipping name resolution
#' and point-in-polygon, and answers with the same rules as \code{\link{NSR_local}}.
#'
#' @param taxon_id WCVP accepted ids (character), one per query.
#' @param region_keys The polygons each record sits in, as a list of character vectors
#'   (\code{"wgsrpd3:BZL"}, \code{"gadm1:BRA.25_1"}), or a single character vector for
#'   one polygon each.
#' @param country_keys Optional country polygons (\code{"gadm0:BRA"}), same shape.
#' @param dir Cache directory.
#' @param min_overlap Ignore region links covering less than this share of a region.
#' @param exclude_extinct Ignore distribution records the source marks extinct?  See
#'   \code{\link{NSR_local}}.
#' @return A data.frame with \code{native_status} and the status columns
#'   \code{NSR_local()} returns, keyed on \code{taxon_id}.  It does not echo the input
#'   columns \code{NSR_local()} carries through (\code{species}, the division names,
#'   \code{user_id}), since this entry point is given ids and polygon keys rather than
#'   a record.
#' @export
NSR_local_by_region <- function(taxon_id, region_keys, country_keys = NULL,
                                dir = nsr_cache_dir(), min_overlap = 0.01,
                                exclude_extinct = TRUE) {
  if (!is.list(region_keys)) region_keys <- as.list(region_keys)
  if (is.null(country_keys)) country_keys <- vector("list", length(taxon_id))
  if (!is.list(country_keys)) country_keys <- as.list(country_keys)
  stopifnot(length(taxon_id) == length(region_keys),
            length(taxon_id) == length(country_keys))
  db <- nsr_local_db(dir)
  clean <- function(z) { z <- z[!is.na(z) & nzchar(z)]; if (!length(z)) character(0) else z }
  places <- list(fine = lapply(region_keys, clean), country = lapply(country_keys, clean))
  res <- if (!is.null(db$chk_dt))
           nsr_resolve_status_set(as.character(taxon_id), places, db, min_overlap, exclude_extinct)
         else nsr_resolve_status(as.character(taxon_id), places, db, min_overlap, exclude_extinct)
  data.frame(taxon_id = as.character(taxon_id),
             native_status_country = res$country_code,
             native_status_state_province = res$state_code,
             native_status = res$code, native_status_reason = res$reason,
             native_status_sources = res$sources, native_status_opinions = res$opinions,
             native_status_scope = res$scope,
             native_status_conflict = res$conflict,
             native_status_conflict_type = res$conflict_type,
             isIntroduced = as.integer(res$code %in% c("I", "Ie")),
             isEndemic = as.integer(res$code == "Ne"),
             isCultivatedNSR = res$cultivated,
             n_subpolygons_native = res$n_sub_native,
             n_subpolygons_introduced = res$n_sub_introduced,
             stringsAsFactors = FALSE)
}

#' Resolve every query at once, with joins
#'
#' Internal.  Needs \code{data.table}; \code{\link{NSR_local}} falls back to the row path
#' without it.  Returns the same list of per-row vectors as \code{nsr_resolve_status()},
#' \emph{including} the per-level codes: the reduction is run once for the answer and once
#' more against each political level on its own, which is what the row path does per row.
#' Endemism is deliberately not applied to the per-level codes - \code{Ne} and \code{Ie}
#' are claims about the taxon's whole range rather than about one level - so that the two
#' implementations agree column for column.
#' @keywords internal
#' @noRd
nsr_resolve_status_set <- function(taxon_id, places, db, min_overlap = 0.01,
                                   exclude_extinct = TRUE) {
  n <- length(taxon_id)
  out <- nsr_resolve_status_set1(taxon_id, places, db, min_overlap, endemism = TRUE,
                                 exclude_extinct = exclude_extinct)
  empty <- rep(list(character(0)), n)
  # a level reports a code only when it was asked about AND there is a taxon to ask
  # about: with no match in the backbone no lookup happened at any level, which is what
  # the row path records
  asked <- !is.na(taxon_id)
  nc <- lengths(places$country)
  nf <- lengths(places$fine)
  if (any(nc > 0)) {
    lv <- nsr_resolve_status_set1(taxon_id, list(fine = empty, country = places$country),
                                  db, min_overlap, endemism = FALSE,
                                  exclude_extinct = exclude_extinct)
    out$country_code <- ifelse(asked & nc > 0, lv$code, NA_character_)
  }
  if (any(nf > 0)) {
    lv <- nsr_resolve_status_set1(taxon_id, list(fine = places$fine, country = empty),
                                  db, min_overlap, endemism = FALSE,
                                  exclude_extinct = exclude_extinct)
    out$state_code <- ifelse(asked & nf > 0, lv$code, NA_character_)
  }
  out
}

#' One pass of the set resolver, against the polygons it is given
#' @keywords internal
#' @noRd
nsr_resolve_status_set1 <- function(taxon_id, places, db, min_overlap = 0.01,
                                    endemism = TRUE, exclude_extinct = TRUE) {
  dt <- function(...) data.table::data.table(...)
  n <- length(taxon_id)
  blank <- rep(NA_character_, n)
  out <- list(code = blank, reason = blank, sources = blank, opinions = blank,
              country_code = blank, state_code = blank, scope = blank,
              conflict = rep(FALSE, n), conflict_type = rep(NA_character_, n),
              cultivated = rep(NA_integer_, n), n_sub_native = rep(NA_integer_, n),
              n_sub_introduced = rep(NA_integer_, n))

  fine <- places$fine
  ctry <- places$country
  nf <- lengths(fine)
  nc <- lengths(ctry)
  has_fine <- nf > 0
  # the polygons the query names: its own ("same"), and its country, which contains it
  Q <- data.table::rbindlist(list(
    dt(qid = rep(seq_len(n), nf), region_key = unlist(fine), relation = "same"),
    dt(qid = rep(seq_len(n), nc), region_key = unlist(ctry),
       relation = "same")))                       # fixed below for rows that have a finer key
  has_place <- nf > 0 | nc > 0
  if (!nrow(Q)) return(nsr_set_finish(out, taxon_id, db, n, endemism = endemism,
                                      has_place = has_place))
  Q[, relation := data.table::fifelse(has_fine[qid] & region_key %in% unlist(ctry), "within", relation)]
  Q[, taxon_id := taxon_id[qid]]
  Q <- Q[!is.na(taxon_id) & !is.na(region_key)]

  # polygons related to the ones named: only the query's OWN polygons follow links, so a
  # country's other sub-polygons never reach a state-level query
  own <- Q[relation == "same"]
  L <- db$links_dt[own, on = c(from_region = "region_key"), nomatch = 0L,
                   allow.cartesian = TRUE]
  if (nrow(L)) {
    known <- own[, list(sys = unique(sub(":.*", "", region_key))), by = "qid"]
    L[, to_sys := sub(":.*", "", to_region)]
    L <- L[!paste(qid, to_sys) %in% paste(known$qid, known$sys)]
    L <- L[fraction >= min_overlap & to_region != from_region,
           .(qid, region_key = to_region, relation = relation, taxon_id)]
    Q <- data.table::rbindlist(list(Q, L), use.names = TRUE)
  }
  # GADM's exact parent/child containment, joined on the query's OWN polygons only so a
  # country's other states never reach a state-level query.  Deliberately after the
  # min_overlap filter: these edges carry no fraction because the containment is exact.
  if (!is.null(db$gadm_edges_dt) && nrow(db$gadm_edges_dt)) {
    H <- db$gadm_edges_dt[relation == "contains"][own, on = c(from_region = "region_key"),
                                                  nomatch = 0L, allow.cartesian = TRUE]
    if (nrow(H)) {
      Q <- data.table::rbindlist(
        list(Q, H[to_region != from_region, .(qid, region_key = to_region, relation, taxon_id)]),
        use.names = TRUE)
    }
  }
  # keep the strongest relation per (query, polygon)
  ord <- c(same = 1L, within = 2L, contains = 3L, overlaps = 4L)
  Q[, r_ord := ord[relation]]
  data.table::setorder(Q, qid, region_key, r_ord)
  Q <- unique(Q, by = c("qid", "region_key"))

  # the opinions themselves
  O <- db$chk_dt[Q, on = c("taxon_id", "region_key"), nomatch = 0L, allow.cartesian = TRUE]
  # after the join, so the checklist is never copied just to drop 0.14% of its rows
  if (exclude_extinct && nrow(O) && "is_extinct" %in% names(O)) O <- O[is_extinct == 0L]
  if (!nrow(O)) return(nsr_set_finish(out, taxon_id, db, n, Q, endemism = endemism,
                                      has_place = has_place))

  O[, `:=`(inc = relation %in% c("same", "within"), sub = relation == "contains",
           ovl = relation == "overlaps")]

  # --- verdict from the polygons the place lies in -------------------------------------
  inc <- O[inc == TRUE]
  agg <- inc[, .(
    has_nat = any(status == "native"), has_int = any(status == "introduced"),
    stated_nat = any(relation == "same" & status == "native"),
    stated_int = any(relation == "same" & status == "introduced"),
    stated_pre = any(relation == "same" & status == "present"),
    n_src_sets = data.table::uniqueN(.SD[, paste(sort(unique(status)), collapse = "+"), by = source_name]$V1),
    within_src = any(.SD[, data.table::uniqueN(status), by = source_name]$V1 > 1),
    srcs = paste(sort(unique(source_name)), collapse = ", "),
    cult = as.integer(any(is_cultivated %in% c(1, "1", TRUE)))
  ), by = qid]

  sub <- O[sub == TRUE]
  subagg <- sub[, .(n_sub_nat = sum(status == "native"), n_sub_int = sum(status == "introduced"),
                    sub_sets = data.table::uniqueN(status), sub_status = status[1],
                    sub_srcs = paste(sort(unique(source_name)), collapse = ", ")), by = qid]
  ovlagg <- O[ovl == TRUE, .(ovl_srcs = paste(sort(unique(source_name)), collapse = ", ")), by = qid]
  allop <- O[, .(opinions = paste(paste0(source_name, ":", status,
                                         data.table::fifelse(relation == "same", "",
                                                             paste0("(", relation, ")"))),
                                  collapse = "; "),
                 cult_any = as.integer(any(is_cultivated %in% c(1, "1", TRUE)))), by = qid]

  res <- dt(qid = seq_len(n))
  res <- merge(res, agg, by = "qid", all.x = TRUE)
  res <- merge(res, subagg, by = "qid", all.x = TRUE)
  res <- merge(res, ovlagg, by = "qid", all.x = TRUE)
  res <- merge(res, allop, by = "qid", all.x = TRUE)

  res[, conflict_type := data.table::fcase(
    is.na(has_nat), NA_character_,
    within_src & n_src_sets > 1, "both",
    n_src_sets > 1, "sources",
    within_src == TRUE, "within source",
    default = "none")]
  res[, code := data.table::fcase(
    !is.na(has_nat) & has_nat, "N",
    !is.na(has_int) & has_int, "I",
    !is.na(has_nat), "P",
    !is.na(sub_sets) & sub_sets == 1L,
      c(native = "N", introduced = "I", present = "P")[sub_status],
    !is.na(sub_sets), "P",
    !is.na(ovl_srcs), "UNK",
    default = NA_character_)]
  res[, scope := data.table::fcase(
    !is.na(has_nat) & ((code == "N" & stated_nat) | (code == "I" & stated_int) |
                         (code == "P" & stated_pre)), "polygon",
    !is.na(has_nat), "containing polygon",
    !is.na(sub_sets) & sub_sets == 1L, "sub-polygons agree",
    !is.na(sub_sets), "sub-polygons differ",
    !is.na(ovl_srcs), "overlapping only",
    default = NA_character_)]
  res[, reason := data.table::fcase(
    code == "N" & conflict_type != "none",
      paste0("Native to this polygon as per checklist (", conflict_type,
             " disagree; any native opinion is taken)"),
    code == "N" & scope == "polygon", "Native in this polygon, as per checklist",
    code == "N" & scope == "containing polygon",
      "Native in a polygon containing this place, as per checklist",
    code == "N", "Native in every listed polygon within this place, as per checklist",
    code == "I" & scope == "polygon", "Introduced in this polygon, as per checklist",
    code == "I" & scope == "containing polygon",
      "Introduced in a polygon containing this place, as per checklist",
    code == "I", "Introduced in every listed polygon within this place, as per checklist",
    code == "P" & scope == "sub-polygons differ",
      paste0("Status varies among the polygons within this place (", n_sub_nat, " native, ",
             n_sub_int, " introduced); give coordinates or a finer division"),
    code == "P" & scope == "polygon", "Present in this polygon, as per checklist",
    code == "P", "Present in a polygon containing this place, as per checklist",
    code == "UNK", "Only polygons partly overlapping this place have a status; none covers it",
    default = NA_character_)]
  res[, srcs_any := data.table::fcase(!is.na(srcs), srcs, !is.na(sub_srcs), sub_srcs,
                                      !is.na(ovl_srcs), ovl_srcs, default = NA_character_)]

  for (f in c("code", "reason", "scope", "conflict_type")) out[[f]] <- res[[f]]
  out$sources <- res$srcs_any
  out$opinions <- res$opinions
  out$cultivated <- res$cult_any
  out$n_sub_native <- data.table::fifelse(is.na(res$n_sub_nat), 0L, as.integer(res$n_sub_nat))
  out$n_sub_introduced <- data.table::fifelse(is.na(res$n_sub_int), 0L, as.integer(res$n_sub_int))
  out$conflict <- !is.na(res$conflict_type) & res$conflict_type != "none"
  # "none" is the contract value for a result nothing disagreed about; NA here would be
  # read as unknown, and the row path says "none" for the same query
  out$conflict_type[!is.na(out$code) & is.na(out$conflict_type)] <- "none"
  nsr_set_finish(out, taxon_id, db, n, Q, endemism = endemism, has_place = has_place)
}

#' Fill in the answers that need no opinions: absence, unknowns, and endemism
#' @keywords internal
#' @noRd
nsr_set_finish <- function(out, taxon_id, db, n, Q = NULL, endemism = TRUE,
                           has_place = TRUE) {
  dt <- function(...) data.table::data.table(...)
  evaluable <- !is.na(taxon_id) & taxon_id %in% db$evaluable
  # the regions each query consulted, for the coverage test
  covered <- rep(FALSE, n)
  if (!is.null(Q) && nrow(Q)) {
    cv <- Q[relation %in% c("same", "within", "contains") & region_key %in% db$covered,
            .(any = TRUE), by = qid]
    covered[cv$qid] <- TRUE
  }
  no_answer <- is.na(out$code)
  out$code[no_answer] <- data.table::fcase(
    !evaluable[no_answer], "UNK",
    covered[no_answer], "A",
    default = "UNK")
  out$reason[no_answer] <- data.table::fcase(
    !evaluable[no_answer], "No source holds native status information for this taxon",
    covered[no_answer], "Absent from the comprehensive checklists for this polygon",
    default = "No comprehensive checklist covers this polygon")
  out$scope[no_answer & out$code == "A"] <- "polygon"
  out$scope[no_answer & out$code == "UNK"] <- "none"
  out$conflict_type[no_answer] <- "none"
  out$n_sub_native[is.na(out$n_sub_native)] <- 0L
  out$n_sub_introduced[is.na(out$n_sub_introduced)] <- 0L
  # absence carries no opinion, so it carries no cultivation flag either: NA, matching
  # nsr_reduce().  A 0 here would read as a checked negative and would differ from the
  # row path for the same query, on nothing but batch size.

  # A taxon that was matched but has nowhere to look is not the same as a place no
  # source covers, and the row path distinguishes them; the generic absence logic above
  # would otherwise report this as uncovered.
  noplace <- !is.na(taxon_id) & !rep_len(has_place, n)
  if (any(noplace)) {
    out$code[noplace] <- "UNK"
    out$reason[noplace] <- "Place not matched to any region"
    out$scope[noplace] <- "none"
  }

  # rows with no place at all, or no taxon
  none <- is.na(taxon_id)
  if (any(none)) {
    out$code[none] <- "UNK"
    out$reason[none] <- "Taxon not matched to a species in the backbone"
    out$scope[none] <- "none"
  }

  # --- endemism, the one rule allowed to look outside the queried polygon --------------
  if (endemism && !is.null(Q) && nrow(Q)) {
    inside <- Q[relation %in% c("same", "contains"), .(qid, region_key)]
    nat <- db$native_dt
    tq <- dt(qid = seq_len(n), taxon_id = taxon_id)[!is.na(taxon_id)]
    # how many of the taxon's native regions fall inside the queried place
    m <- nat[tq, on = "taxon_id", nomatch = 0L, allow.cartesian = TRUE]
    if (nrow(m)) {
      m[, in_place := paste(qid, region_key) %in% paste(inside$qid, inside$region_key)]
      per <- m[, .(n_nat = .N, n_in = sum(in_place)), by = qid]
      endemic <- per[n_nat > 0 & n_in == n_nat, qid]
      i <- endemic[out$code[endemic] == "N"]
      out$code[i] <- "Ne"
      out$reason[i] <- sub("^Native", "Native and endemic", out$reason[i])
      # Ie: absent here, and the whole native range lies elsewhere and is confined
      consulted <- Q[relation %in% c("same", "within", "contains"), .(qid, region_key)]
      m[, consulted_here := paste(qid, region_key) %in% paste(consulted$qid, consulted$region_key)]
      elsewhere <- m[, .(n_nat = .N, n_here = sum(consulted_here)), by = qid][n_nat > 0 & n_here == 0]
      conf <- db$confined_dt
      elsewhere <- merge(elsewhere, tq, by = "qid")
      e <- merge(elsewhere, conf, by = "taxon_id", all.x = FALSE, sort = FALSE)
      j <- e$qid[out$code[e$qid] == "A" & !is.na(e$confined_to)]
      out$code[j] <- "Ie"
      out$reason[j] <- paste0("Absent from this region and endemic to ",
                              e$confined_to[match(j, e$qid)], ", so introduced here (inferred)")
    }
  }
  out
}

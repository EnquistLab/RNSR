# ===========================================================================
# What a species' genus or family can say about it
#
# Most of what is true of a genus is true of only SOME of its species: a genus native to
# a region means some species of it is native there, and the species in hand is as
# readily the introduced one (the ecological fallacy, in its division form).  So a genus
# never lends a species its native status.
#
# Two claims about a higher taxon are true of EVERY species in it, and those do carry
# down - for a species with no record of its own bearing on the place:
#
#   - the genus is introduced in a polygon that is, or contains, the place: then no
#     species of it is native there, so this one is introduced (I);
#   - the genus resolves to Ie at the place - absent from its comprehensive lists, with
#     its whole native range confined somewhere else: then so is every species's (Ie).
#
# In WCVP v15 the first holds without exception: of 51,473 genus x area records marked
# introduced, not one area has a native species of that genus.  See the design note,
# "Two sound downward inferences".
# ===========================================================================

#' The genus and family taxa above each taxon, where the cache holds them
#'
#' Internal.  Read off the taxa table: a taxon's \code{genus} and \code{family} names,
#' matched to taxa of that rank.  Where a taxon is not in the table (a name resolved at
#' query time), its genus is the first word of \code{name} - which the caller passes as
#' the ACCEPTED name, so a synonym that moved genus is read under its new genus - and its
#' family is that genus's, or else \code{family}, the accepted family from resolution.
#' A taxon is never its own higher taxon.
#' @return data.frame with \code{genus_id}, \code{genus}, \code{family_id}, \code{family}.
#' @keywords internal
#' @noRd
nsr_higher_ids <- function(taxon_id, db, name = NULL, family = NULL) {
  accepted_family <- family
  tx <- db$taxa
  rk <- tolower(tx$rank)
  i <- match(taxon_id, tx$taxon_id)
  own_rank <- rk[i]
  genus <- tx$genus[i]
  if (!is.null(name)) {
    miss <- (is.na(genus) | !nzchar(genus)) & !is.na(name) & nzchar(trimws(name))
    genus[miss] <- sub("[[:space:]].*$", "", trimws(name[miss]))
  }
  g <- rk == "genus"
  gi <- match(genus, tx$species_name[g])
  genus_id <- tx$taxon_id[g][gi]
  family <- tx$family[i]
  nofam <- is.na(family) | !nzchar(family)
  family[nofam] <- tx$family[g][gi][nofam]
  if (!is.null(accepted_family)) {
    nofam <- is.na(family) | !nzchar(family)
    family[nofam] <- accepted_family[nofam]
  }
  f <- rk == "family"
  family_id <- tx$taxon_id[f][match(family, tx$species_name[f])]
  self <- function(x) !is.na(x) & !is.na(taxon_id) & x == taxon_id
  genus_id[own_rank %in% c("genus", "family") | self(genus_id)] <- NA_character_
  family_id[own_rank %in% "family" | self(family_id)] <- NA_character_
  data.frame(genus_id = genus_id, genus = genus, family_id = family_id, family = family,
             stringsAsFactors = FALSE)
}

#' The answer for a set of places: the taxon's own records, then its higher taxa
#'
#' Internal.  Every answer \code{NSR_local()} gives goes through here - the main one and
#' both halves of a split place - so the higher-rank inferences cannot be applied to one
#' and missed on another.
#' @keywords internal
#' @noRd
nsr_answer <- function(taxon_id, places, resolve, db, name = NULL, min_overlap = 0.01,
                       family = NULL) {
  nsr_apply_higher(resolve(taxon_id, places), taxon_id, places, resolve, db, name, min_overlap,
                   family)
}

#' Apply the two sound downward inferences from genus, then family
#'
#' Internal.  For each row the resolver left at \code{A} or \code{UNK} with no opinion
#' of its own, asks the same resolver what the genus (then the family) is at the same
#' place, and carries down only an \code{I} from a polygon that is or contains the place
#' with no native sub-polygon, or an \code{Ie}.  Because it reuses whichever resolver is
#' running, the row and set paths agree by construction.  Touches \code{code},
#' \code{reason}, \code{scope}, \code{sources} and \code{opinions} only: the per-level codes
#' remain the species' own records, as they do for endemism.
#' @param resolve \code{function(taxon_id, places)} returning the resolver's list.
#' @keywords internal
#' @noRd
nsr_apply_higher <- function(res, taxon_id, places, resolve, db, name = NULL,
                             min_overlap = 0.01, family = NULL) {
  todo <- !is.na(taxon_id) & res$code %in% c("A", "UNK") & is.na(res$opinions)
  if (!any(todo)) return(res)
  # "No record of its own here" means no record at all - an extinct one included.  With
  # exclude_extinct, a lost population has no opinions, and is A; it is still a record of
  # the species here, and its genus must not turn "gone" into "introduced".  So ask exactly
  # what the resolver asks - nsr_opinions_for(), which reaches overlapping polygons too -
  # but with extinct records kept, plus the direct rows on the country the place was given
  # with, as the resolver takes them.
  own <- vapply(which(todo), function(i) {
    tid <- as.character(taxon_id[i])
    fine <- places$fine[[i]]
    ctry <- places$country[[i]]
    keys <- if (length(fine)) fine else ctry
    anc <- setdiff(ctry, keys)
    !is.null(nsr_opinions_for(tid, keys, db, min_overlap, exclude_extinct = FALSE)) ||
      length(unlist(mget(paste(tid, anc), db$chk_env, ifnotfound = list(NULL)))) > 0
  }, logical(1))
  todo[which(todo)[own]] <- FALSE
  if (!any(todo)) return(res)
  h <- nsr_higher_ids(taxon_id, db, name, family)
  for (lvl in c("genus", "family")) {
    hid <- h[[paste0(lvl, "_id")]]
    rows <- which(todo & !is.na(hid))
    if (!length(rows)) next
    g <- resolve(hid[rows], list(fine = places$fine[rows], country = places$country[rows]))
    # introduced in a polygon the place lies in, with no part of it native - a genus
    # introduced only in some sub-polygons is not introduced in the whole place
    intro <- g$code %in% "I" & g$scope %in% c("polygon", "containing polygon") &
      g$n_sub_native %in% 0L
    # A place made of several polygons in one geography - a division from another system,
    # spanning several GADM units - is introduced-for-the-genus only if every part is: one
    # unit's list says nothing about the others.
    intro[intro] <- nsr_every_part(rows[intro], hid, places, resolve, function(p) {
      p$code %in% "I" & p$scope %in% c("polygon", "containing polygon") & p$n_sub_native %in% 0L
    })
    endem <- g$code %in% "Ie"
    hit <- intro | endem
    if (!any(hit)) next
    r <- rows[hit]
    nm <- h[[lvl]][r]
    where <- ifelse(g$scope[hit] %in% "polygon", "this polygon", "a polygon containing this place")
    elsewhere <- sub("^Absent from this region and endemic to (.*), so introduced here \\(inferred\\)$",
                     "\\1", g$reason[hit])
    res$code[r] <- g$code[hit]
    res$reason[r] <- ifelse(
      intro[hit],
      paste0("Introduced: its ", lvl, " ", nm, " is introduced in ", where, ", as per checklist"),
      paste0("Absent from this region, and its ", lvl, " ", nm, " is endemic to ", elsewhere,
             ", so introduced here (inferred)"))
    res$scope[r] <- lvl
    res$sources[r] <- g$sources[hit]
    # the evidence is the higher taxon's, so is any disagreement within it
    res$conflict[r] <- g$conflict[hit]
    res$conflict_type[r] <- g$conflict_type[hit]
    res$opinions[r] <- paste0(lvl, " ", nm, ": ",
                              ifelse(is.na(g$opinions[hit]), "none here", g$opinions[hit]))
    todo[r] <- FALSE
  }
  res
}

#' Does a test hold for every part of a place made of several polygons?
#'
#' Internal.  A place's keys in the same geography are its parts.  For rows whose place
#' has such parts, resolves the taxon at each part alone and asks \code{test} of each;
#' rows with a single polygon per geography pass through as TRUE.
#' @return logical, one per element of \code{rows}.
#' @keywords internal
#' @noRd
nsr_every_part <- function(rows, ids, places, resolve, test) {
  ok <- rep(TRUE, length(rows))
  if (!length(rows)) return(ok)
  parts <- lapply(rows, function(i) {
    fine <- places$fine[[i]]
    k <- if (length(fine)) fine else places$country[[i]]
    s <- sub(":.*", "", k)
    k[s %in% s[duplicated(s)]]
  })
  multi <- which(lengths(parts) > 0)
  if (!length(multi)) return(ok)
  rr <- rep(rows[multi], lengths(parts[multi]))
  pk <- unlist(parts[multi], use.names = FALSE)
  in_fine <- vapply(seq_along(rr), function(j) length(places$fine[[rr[j]]]) > 0, logical(1))
  pl <- list(fine = ifelse(in_fine, as.list(pk), list(character(0))),
             country = ifelse(in_fine, places$country[rr], as.list(pk)))
  pass <- test(resolve(ids[rr], pl))
  each <- tapply(pass, rep(multi, lengths(parts[multi])), all)
  ok[as.integer(names(each))] <- as.logical(each)
  ok
}

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
#' query time), its genus is the first word of the submitted name, and its family that
#' genus's.  A taxon is never its own higher taxon.
#' @return data.frame with \code{genus_id}, \code{genus}, \code{family_id}, \code{family}.
#' @keywords internal
#' @noRd
nsr_higher_ids <- function(taxon_id, db, name = NULL) {
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
  f <- rk == "family"
  family_id <- tx$taxon_id[f][match(family, tx$species_name[f])]
  self <- function(x) !is.na(x) & !is.na(taxon_id) & x == taxon_id
  genus_id[own_rank %in% c("genus", "family") | self(genus_id)] <- NA_character_
  family_id[own_rank %in% "family" | self(family_id)] <- NA_character_
  data.frame(genus_id = genus_id, genus = genus, family_id = family_id, family = family,
             stringsAsFactors = FALSE)
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
nsr_apply_higher <- function(res, taxon_id, places, resolve, db, name = NULL) {
  todo <- !is.na(taxon_id) & res$code %in% c("A", "UNK") & is.na(res$opinions)
  if (!any(todo)) return(res)
  h <- nsr_higher_ids(taxon_id, db, name)
  for (lvl in c("genus", "family")) {
    hid <- h[[paste0(lvl, "_id")]]
    rows <- which(todo & !is.na(hid))
    if (!length(rows)) next
    g <- resolve(hid[rows], list(fine = places$fine[rows], country = places$country[rows]))
    # introduced in a polygon the place lies in, with no part of it native - a genus
    # introduced only in some sub-polygons is not introduced in the whole place
    intro <- g$code %in% "I" & g$scope %in% c("polygon", "containing polygon") &
      g$n_sub_native %in% 0L
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
    res$opinions[r] <- paste0(lvl, " ", nm, ": ",
                              ifelse(is.na(g$opinions[hit]), "none here", g$opinions[hit]))
    todo[r] <- FALSE
  }
  res
}

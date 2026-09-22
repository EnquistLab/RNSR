# Source importers for the local NSR.
#
# Each returns a data.frame with one row per taxon x division x opinion:
#   taxon_name        the source's own name string (resolved later against WCVP)
#   taxon_id          the WCVP accepted id, where the source supplies one directly
#   status            "native", "introduced" or "present"
#   is_cultivated     1 where the source says the taxon is cultivated in that division
#   region_key        "<system>:<code>", the source's OWN geography
#   region_name       the source's name for it
#
# Each source keeps the regions it publishes against: WCVP records WGSRPD level-3 areas
# (biogeographic, e.g. "Borneo"), VASCAN and Flora do Brasil record provinces and states
# (GADM level 1). Queries are resolved against those polygons, not against names; see
# local_regions.R.

#' Read a delimited file quickly when data.table is installed
#' @keywords internal
#' @noRd
nsr_read_delim <- function(path_or_conn, sep = "\t", quote = "\"", select = NULL) {
  if (requireNamespace("data.table", quietly = TRUE) && is.character(path_or_conn)) {
    return(as.data.frame(data.table::fread(path_or_conn, sep = sep, quote = quote,
                                           showProgress = FALSE, data.table = FALSE,
                                           select = select, colClasses = "character")))
  }
  x <- utils::read.delim(path_or_conn, sep = sep, quote = quote, colClasses = "character",
                         stringsAsFactors = FALSE, na.strings = "")
  if (!is.null(select)) x <- x[, intersect(select, names(x)), drop = FALSE]
  x
}

#' Extract one member of a zip to a temporary file
#' @keywords internal
#' @noRd
nsr_unzip_member <- function(zip, member) {
  td <- file.path(tempdir(), paste0("nsr-", basename(zip)))
  dir.create(td, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(zip, files = member, exdir = td, overwrite = TRUE)
  file.path(td, member)
}

#' POWO / WCVP: global native and introduced ranges at WGSRPD level 3
#'
#' Internal.  Distributions are kept against the WGSRPD level-3 areas WCVP publishes
#' them against; the polygons do the rest.  Matching those areas to political divisions
#' by NAME loses a quarter of them (Borneo, Sulawesi, Maluku, New Guinea, Lesser Sunda
#' Is. are not country or state names) and cannot express "Brazil Southeast" containing
#' four states, so it is not done.
#'
#' Names need no resolver: WCVP is the backbone, so \code{accepted_plant_name_id} is the
#' taxon id directly.
#' @keywords internal
#' @noRd
nsr_import_wcvp <- function(zip = NULL, bb, quiet = FALSE) {
  if (is.null(zip)) zip <- nsr_find_wcvp_zip()
  if (is.null(zip) || !file.exists(zip)) {
    stop("Could not find the WCVP archive. Supply it as files = list(wcvp = \"wcvp-v15.zip\"), ",
         "or build the TNRS 'wcvp' source, whose cache holds one.", call. = FALSE)
  }
  dist_f <- nsr_unzip_member(zip, "wcvp_distribution.csv")
  names_f <- nsr_unzip_member(zip, "wcvp_names.csv")
  if (!quiet) message("  reading WCVP ...")
  dist <- nsr_read_delim(dist_f, sep = "|",
                         select = c("plant_name_id", "area_code_l3", "area", "introduced",
                                    "extinct", "location_doubtful"))
  nm <- nsr_read_delim(names_f, sep = "|",
                       select = c("plant_name_id", "accepted_plant_name_id", "parent_plant_name_id",
                                  "taxon_name", "taxon_rank", "taxon_status", "family", "genus"))

  # distributions are recorded against a name; carry them to its accepted taxon
  i <- match(dist$plant_name_id, nm$plant_name_id)
  acc <- nm$accepted_plant_name_id[i]
  acc[is.na(acc) | !nzchar(acc)] <- nm$plant_name_id[i][is.na(acc) | !nzchar(acc)]
  # WCVP records distributions against accepted infraspecific taxa too (60k of them), so a
  # subspecies' range hangs off the subspecies. Roll those up to the species: it is NSR's
  # own rule (native propagates upward) and it is what a species-level query needs.
  sp <- nsr_species_of(acc, nm)
  j <- match(sp, nm$plant_name_id)
  dist$taxon_id <- sp
  dist$taxon_name <- nm$taxon_name[j]
  dist$rank_published <- nm$taxon_rank[match(acc, nm$plant_name_id)]

  flag <- function(x) !is.na(x) & x %in% c("1", 1, TRUE, "TRUE")
  # Extinct records are KEPT and flagged rather than dropped here.  WCVP's `extinct` is a
  # bare 0/1 with no date - it means "considered no longer present as of this release" -
  # so whether it should count is a property of the question, not of the build: present-day
  # status wants it out, a historical distribution wants it in.  NSR_local(exclude_extinct)
  # decides at query time, which is only possible if the build keeps the row.  Dropping it
  # here would also lose the fact that the taxon was ONCE native there, and the Ie rule
  # needs that fact or it will call a natively extirpated record introduced.
  # A DOUBTFUL record is kept as "present": too weak to support native or introduced, and
  # dropping it would let the region read as a confident absence when the source in fact
  # records a maybe.
  keep <- !is.na(dist$taxon_id) & !is.na(dist$taxon_name)
  dist <- dist[keep, , drop = FALSE]
  j <- j[keep]                      # j indexes nm per dist row, so it is subset with it

  out <- data.frame(
    taxon_name = dist$taxon_name, taxon_id = dist$taxon_id,
    species_name = dist$taxon_name, family = nm$family[j], genus = nm$genus[j],
    rank = nm$taxon_rank[j],
    status = ifelse(flag(dist$location_doubtful), "present",
                    ifelse(flag(dist$introduced), "introduced", "native")),
    is_cultivated = 0L, is_extinct = as.integer(flag(dist$extinct)),
    region_key = paste0("wgsrpd3:", dist$area_code_l3), region_name = dist$area,
    stringsAsFactors = FALSE
  )
  out[!is.na(out$region_key), , drop = FALSE]
}

#' Walk accepted infraspecific taxa up to their species
#'
#' Internal.  Up to three steps (a variety under a subspecies), stopping at the first
#' ancestor of species rank; anything already at species rank or above is left alone.
#' @keywords internal
#' @noRd
nsr_species_of <- function(ids, nm, max_steps = 3L) {
  rank <- tolower(nm$taxon_rank)
  is_infra <- !rank %in% c("species", "genus", "family", "")
  out <- ids
  for (step in seq_len(max_steps)) {
    i <- match(out, nm$plant_name_id)
    up <- !is.na(i) & is_infra[i]
    if (!any(up)) break
    par <- nm$parent_plant_name_id[i[up]]
    par[is.na(par) | !nzchar(par)] <- out[up][is.na(par) | !nzchar(par)]
    out[up] <- par
  }
  out
}

#' Where TNRS keeps its copy of WCVP, if it has one
#' @keywords internal
#' @noRd
nsr_find_wcvp_zip <- function() {
  if (!requireNamespace("TNRS", quietly = TRUE)) return(NULL)
  d <- try(TNRS:::tnrs_cache_dir(), silent = TRUE)
  if (inherits(d, "try-error") || !dir.exists(d)) return(NULL)
  f <- list.files(d, pattern = "^wcvp.*\\.zip$", full.names = TRUE)
  if (!length(f)) NULL else f[order(file.mtime(f), decreasing = TRUE)][1]
}

#' VASCAN: Canada, by province and territory
#' @keywords internal
#' @noRd
nsr_import_vascan <- function(zip, bb, quiet = FALSE) {
  if (is.null(zip) || !file.exists(zip)) {
    stop("Supply the VASCAN archive as files = list(vascan = \"vascan_dwca.zip\"); ",
         "download it from https://data.canadensys.net/ipt/archive.do?r=vascan", call. = FALSE)
  }
  tx <- nsr_read_delim(nsr_unzip_member(zip, "taxon.txt"))
  di <- nsr_read_delim(nsr_unzip_member(zip, "distribution.txt"))
  # carry each distribution to the accepted name of its taxon
  acc <- tx$acceptedNameUsageID
  acc[is.na(acc) | !nzchar(acc)] <- tx$id[is.na(acc) | !nzchar(acc)]
  i <- match(di$id, tx$id)
  j <- match(acc[i], tx$id)
  di$taxon_name <- tx$scientificName[j]
  # VASCAN splits the two questions: occurrenceStatus is present / excluded / doubtful /
  # irregular / absent, establishmentMeans is native / introduced. Nativeness comes from
  # the latter; the former only says whether the record counts at all.
  occ <- tolower(ifelse(is.na(di$occurrenceStatus), "", di$occurrenceStatus))
  est <- tolower(ifelse(is.na(di$establishmentMeans), "", di$establishmentMeans))
  status <- ifelse(est %in% c("native", "endemic"), "native",
                   ifelse(est %in% c("introduced", "naturalized", "naturalised"), "introduced",
                          "present"))
  status[occ %in% c("absent", "excluded")] <- NA_character_
  # locationID is "ISO3166-2:CA-NB"; the code beats the name, since VASCAN's spellings and
  # GADM's differ and the list also covers places outside Canada
  code <- sub("^ISO3166-2:", "", ifelse(is.na(di$locationID), "", di$locationID))
  gid1 <- bb$state$gid_1[match(ifelse(nzchar(code), sub("-", ".", toupper(code)), NA_character_),
                               bb$state$hasc_full)]
  # the three VASCAN entries carrying no code: two are one GADM province, one another country
  loc <- tolower(ifelse(is.na(di$locality), "", di$locality))
  gid1[is.na(gid1) & loc %in% c("newfoundland", "labrador")] <-
    bb$state$gid_1[match("CA.NF", bb$state$hasc_full)]
  gid0 <- rep(NA_character_, nrow(di))
  gid0[is.na(gid1) & loc == "greenland"] <- "GRL"
  out <- data.frame(taxon_name = di$taxon_name, taxon_id = NA_character_, status = status,
                    is_cultivated = 0L, is_extinct = 0L,
                    region_key = ifelse(!is.na(gid1), paste0("gadm1:", gid1),
                                        ifelse(!is.na(gid0), paste0("gadm0:", gid0), NA_character_)),
                    region_name = di$locality, stringsAsFactors = FALSE)
  if (!quiet) {
    miss <- unique(di$locality[is.na(out$region_key)])
    if (length(miss)) message("  ", length(miss), " VASCAN divisions unmatched: ",
                              paste(utils::head(miss, 6), collapse = ", "))
  }
  out[!is.na(out$status) & !is.na(out$region_key) & !is.na(out$taxon_name), , drop = FALSE]
}

#' Flora e Funga do Brasil: Brazil, by state
#'
#' Internal.  \code{establishmentMeans} is Portuguese: NATIVA, NATURALIZADA, CULTIVADA.
#' Cultivated rows are kept as "present" and flagged, which is what the service's
#' \code{isCultivatedNSR} reports.  Divisions come from the ISO 3166-2 code in
#' \code{locationID} (BR-SP ...), matched to the GNRS backbone's HASC code.
#' @keywords internal
#' @noRd
nsr_import_flbr <- function(zip, bb, quiet = FALSE) {
  if (is.null(zip) || !file.exists(zip)) {
    stop("Supply the Flora do Brasil archive as files = list(flbr = \"flbr_dwca.zip\"); ",
         "download it from https://api.checklistbank.org/dataset/2031/archive", call. = FALSE)
  }
  tx <- nsr_read_delim(nsr_unzip_member(zip, "taxon.txt"))
  di <- nsr_read_delim(nsr_unzip_member(zip, "distribution.txt"))
  acc <- tx$acceptedNameUsageID
  acc[is.na(acc) | !nzchar(acc)] <- tx$id[is.na(acc) | !nzchar(acc)]
  i <- match(di$id, tx$id)
  j <- match(acc[i], tx$id)
  di$taxon_name <- tx$scientificName[j]

  em <- toupper(ifelse(is.na(di$establishmentMeans), "", di$establishmentMeans))
  status <- ifelse(em == "NATIVA", "native",
                   ifelse(em == "NATURALIZADA", "introduced", "present"))
  cult <- as.integer(em == "CULTIVADA")

  # BR-SP -> GNRS hasc_full "BR.SP" -> GADM gid_1
  hasc <- sub("-", ".", toupper(ifelse(is.na(di$locationID), "", di$locationID)))
  k <- match(hasc, bb$state$hasc_full)
  out <- data.frame(
    taxon_name = di$taxon_name, taxon_id = NA_character_, status = status, is_cultivated = cult,
    is_extinct = 0L,
    region_key = ifelse(is.na(bb$state$gid_1[k]), NA_character_, paste0("gadm1:", bb$state$gid_1[k])),
    region_name = bb$state$state_province[k],
    stringsAsFactors = FALSE
  )
  if (!quiet) {
    miss <- unique(di$locationID[is.na(k)])
    if (length(miss)) message("  ", length(miss), " Brazilian location codes unmatched: ",
                              paste(utils::head(miss, 8), collapse = ", "))
  }
  out[!is.na(out$region_key) & !is.na(out$taxon_name), , drop = FALSE]
}

#' Cache directory for the offline NSR
#'
#' Internal.  Shared with GNRS and GVS, so the three services build one cache: the
#' political divisions NSR keys on are GNRS's, and a user who has built either of the
#' others already has them.  \code{options(NSR.cache_dir=)} overrides, then
#' \code{options(GNRS.cache_dir=)}, then \code{tools::R_user_dir("GNRS", "cache")}.
#'
#' \code{tools::R_user_dir()} arrived in R 4.0.0, and the package supports R 3.5 for the
#' API functions.  Rather than invent a different default on old R - which would put the
#' cache somewhere GNRS and GVS do not look, and silently give each package its own copy -
#' this says what is missing and how to set it.  The API functions never reach here.
#' @keywords internal
#' @noRd
nsr_cache_dir <- function(create = FALSE) {
  fallback <- function() {
    if (!is.null(getNamespace("tools")[["R_user_dir"]])) {
      return(tools::R_user_dir("GNRS", which = "cache"))
    }
    stop("The local NSR needs R >= 4.0.0 for the shared cache location, or an explicit ",
         "one: set options(GNRS.cache_dir = ) to the directory GNRS and GVS use, or pass ",
         "dir = to this function.", call. = FALSE)
  }
  dir <- getOption("NSR.cache_dir", getOption("GNRS.cache_dir", fallback()))
  if (create && !dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Paths of the local NSR tables
#' @keywords internal
#' @noRd
nsr_table_path <- function(table, dir = nsr_cache_dir()) {
  file.path(dir, paste0("nsr-", table, ".gz.parquet"))
}

#' @keywords internal
#' @noRd
nsr_provenance_path <- function(component, dir = nsr_cache_dir()) {
  file.path(dir, paste0("nsr-", component, "-provenance.rds"))
}

#' The tables the local NSR is made of
#' @keywords internal
#' @noRd
nsr_tables <- function() c("sources", "taxa", "regions", "checklist")

#' Resolve a source's accepted synonyms to its canonical name
#'
#' Internal.  The Kew dataset is read as WCVP and reported as \code{wcvp}, which is what
#' it is and what \code{TNRS_local()} calls the same data.  \code{powo}, the portal the
#' live service names it after, is accepted so existing calls and notes keep working.
#' @keywords internal
#' @noRd
nsr_canonical_source <- function(x) {
  if (!length(x)) return(x)
  alias <- c(powo = "wcvp")
  i <- match(x, names(alias))
  unname(ifelse(is.na(i), x, alias[i]))
}

#' Require the optional packages a local-NSR path needs
#'
#' Internal.  The offline implementation is all Suggests: the API functions must keep
#' working on an install that has none of it, so every entry point says what is missing
#' rather than failing inside a \code{::} call.
#' @keywords internal
#' @noRd
nsr_need <- function(..., what = "The local NSR") {
  for (pkg in c(...)) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(what, " needs the '", pkg, "' package.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Checklist sources the local NSR can build
#'
#' Internal.  One entry per source: what it covers, whether it is comprehensive for its
#' area (so that absence there is informative), where it comes from and under what
#' licence.  Sources are fetched on the user's machine at build time; nothing derived is
#' shipped with the package.
#'
#' \code{region_system} is the geography the source publishes against, kept as it is.
#' \code{is_comprehensive} says the source aims to list everything in its area; it does
#' not say which taxa it covers, which is measured from the data instead.
#' @keywords internal
#' @noRd
nsr_builtin_registry <- function() {
  list(
    wcvp = list(
      source_name = "wcvp",
      source_name_full = "Plants of the World Online / World Checklist of Vascular Plants (WCVP)",
      version = "v15",
      url = "https://sftp.kew.org/pub/data-repositories/WCVP/",
      licence = "CC BY 4.0",
      citation = "Govaerts, R. et al. The World Checklist of Vascular Plants (WCVP). Royal Botanic Gardens, Kew.",
      is_comprehensive = TRUE,
      region_system = "wgsrpd3",
      coverage = "global, WGSRPD level 3 (the geography WCVP publishes against)",
      local_file = "wcvp-v15.zip (from the TNRS cache, or supplied)"
    ),
    vascan = list(
      source_name = "vascan",
      source_name_full = "Database of Vascular Plants of Canada (VASCAN)",
      version = NA_character_,
      url = "https://data.canadensys.net/ipt/archive.do?r=vascan",
      licence = "CC0 1.0",
      citation = "Brouillet, L. et al. VASCAN, the Database of Vascular Plants of Canada.",
      is_comprehensive = TRUE,
      region_system = "gadm1",
      coverage = "Canada, by province and territory",
      local_file = "vascan_dwca.zip"
    ),
    # usda is deliberately absent: the USDA no longer serves the per-state native-status
    # export the service was built from. The GBIF copy of PLANTS is names only, and the
    # current API gives status by region (CAN / L48 / AK / HI), not by state, which adds
    # nothing over WCVP. See dev_notes/01-offline-nsr-design.md.
    flbr = list(
      source_name = "flbr",
      source_name_full = "Flora e Funga do Brasil (Lista Oficial)",
      version = NA_character_,
      url = "https://api.checklistbank.org/dataset/2031/archive",
      licence = "CC BY 4.0",
      citation = "Flora e Funga do Brasil. Jardim Botanico do Rio de Janeiro.",
      is_comprehensive = TRUE,
      region_system = "gadm1",
      coverage = "Brazil, by state; the level WGSRPD cannot reach (five level-3 regions)",
      local_file = "flbr_dwca.zip"
    )
  )
}

#' Which parts of the local NSR are built
#'
#' @param dir Cache directory.  Defaults to the one shared with GNRS and GVS.
#' @return A data.frame with one row per source: whether it is built, how many checklist
#'   records and taxa it contributed, and when it was built.
#' @export
NSR_local_status <- function(dir = nsr_cache_dir()) {
  nsr_need("nanoparquet")
  reg <- nsr_builtin_registry()
  built_tables <- vapply(nsr_tables(), function(t) file.exists(nsr_table_path(t, dir)), logical(1))
  src <- if (built_tables[["sources"]]) {
    as.data.frame(nanoparquet::read_parquet(nsr_table_path("sources", dir)))
  } else NULL
  out <- do.call(rbind, lapply(reg, function(s) {
    row <- if (!is.null(src) && s$source_name %in% src$source_name) src[src$source_name == s$source_name, ] else NULL
    data.frame(
      source = s$source_name, coverage = s$coverage, licence = s$licence,
      built = !is.null(row),
      n_records = if (is.null(row)) NA_integer_ else as.integer(row$n_records),
      n_taxa = if (is.null(row)) NA_integer_ else as.integer(row$n_taxa),
      date_built = if (is.null(row)) NA_character_ else as.character(row$date_accessed),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  attr(out, "cache_dir") <- dir
  out
}

#' Remove the local NSR tables
#'
#' @param dir Cache directory.
#' @param quiet Suppress messages?
#' @return Invisibly, the files removed.
#' @export
NSR_local_remove <- function(dir = nsr_cache_dir(), quiet = FALSE) {
  f <- c(vapply(nsr_tables(), nsr_table_path, character(1), dir = dir),
         nsr_table_path("region-links", dir), nsr_raster_path("wgsrpd3", dir),
         nsr_raster_path("wgsrpd3_index", dir), nsr_provenance_path("nsr", dir))
  f <- f[file.exists(f)]
  unlink(f)
  if (!quiet) message("Removed ", length(f), " file(s) from ", dir)
  invisible(f)
}

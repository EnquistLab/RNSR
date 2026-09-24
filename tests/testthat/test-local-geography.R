# A cache can be built and still unable to reach a source.  WCVP publishes against WGSRPD,
# so division names reach its opinions only through the region link table and coordinates
# only through the WGSRPD raster; if either is missing, every answer that depends on WCVP
# comes back UNK and looks like an honest "no data".  These tests build exactly that cache -
# small, on disk, with neither - and check that it says so.

skip_if_not_installed("nanoparquet")

broken_cache <- function() {
  dir <- file.path(tempdir(), paste0("nsr-geo-", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(dir, recursive = TRUE)
  w <- function(x, t) nanoparquet::write_parquet(x, file.path(dir, paste0("nsr-", t, ".gz.parquet")),
                                                 compression = "gzip")
  w(data.frame(source_name = "wcvp", is_comprehensive = TRUE, n_records = 1L, n_taxa = 1L,
               date_accessed = "2026-09-24", stringsAsFactors = FALSE), "sources")
  w(data.frame(taxon_id = "t1", species_name = "Sp one", family = "Fam", genus = "Gen",
               rank = "species", stringsAsFactors = FALSE), "taxa")
  w(data.frame(region_key = "wgsrpd3:QUE", region_name = "Quebec", system = "wgsrpd3",
               code = "QUE", stringsAsFactors = FALSE), "regions")
  w(data.frame(taxon_id = "t1", region_key = "wgsrpd3:QUE", status = "native",
               is_cultivated = 0L, is_extinct = 0L, source_name = "wcvp",
               stringsAsFactors = FALSE), "checklist")
  dir
}
# the once-per-session memory lives in an environment, so it is reset through a reference
forget_warnings <- function() { e <- NSR:::nsr_session; e$warned <- NULL }
query <- data.frame(species = "Sp one", country = "Canada", state_province = "Quebec",
                    stringsAsFactors = FALSE)

test_that("status separates built from reachable", {
  dir <- broken_cache()
  s <- NSR_local_status(dir)
  w <- s[s$source == "wcvp", ]
  expect_true(w$built)
  expect_false(w$answers_by_name)
  expect_false(w$answers_by_coordinates)
  # unbuilt sources say nothing either way
  expect_true(is.na(s$answers_by_name[s$source == "vascan"]))
})

test_that("a query against it is told, once", {
  dir <- broken_cache()
  forget_warnings()
  expect_warning(NSR_local(query, dir = dir, resolve_names = FALSE, quiet = TRUE),
                 "region link table is missing")
  # not again on the next call: a loop over batches should not repeat it every time
  expect_no_warning(NSR_local(query, dir = dir, resolve_names = FALSE, quiet = TRUE))
})

test_that("asking for coordinates adds the raster to what is reported missing", {
  dir <- broken_cache()
  forget_warnings()
  expect_warning(NSR_local(query, dir = dir, resolve_names = FALSE, quiet = TRUE,
                           use_coordinates = TRUE),
                 "WGSRPD raster is missing")
})

test_that("a cache with its link table is not warned about", {
  dir <- broken_cache()
  nanoparquet::write_parquet(
    data.frame(from_region = "gadm1:CAN.11_1", to_region = "wgsrpd3:QUE", relation = "same",
               fraction = 0.99, stringsAsFactors = FALSE),
    file.path(dir, "nsr-region-links.gz.parquet"), compression = "gzip")
  forget_warnings()
  expect_no_warning(NSR_local(query, dir = dir, resolve_names = FALSE, quiet = TRUE))
  expect_true(NSR_local_status(dir)$answers_by_name[1])
})

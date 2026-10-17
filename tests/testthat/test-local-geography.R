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
  # the GNRS backbone, as GNRS writes it to the shared cache: Canada, and Quebec
  g <- function(x, t) nanoparquet::write_parquet(x, file.path(dir, paste0("gnrs-", t, ".gz.parquet")),
                                                 compression = "gzip")
  g(data.frame(country_id = 1L, country = "Canada", gid_0 = "CAN", iso = "CA",
               stringsAsFactors = FALSE), "country")
  g(data.frame(state_province_id = 10L, country_id = 1L, country = "Canada",
               state_province = "Quebec", state_province_std = "Quebec", hasc_full = "CA.QC",
               gid_1 = "CAN.11_1", stringsAsFactors = FALSE), "state_province")
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
  r <- expect_no_warning(NSR_local(query, dir = dir, resolve_names = FALSE, quiet = TRUE))
  expect_true(NSR_local_status(dir)$answers_by_name[1])
  # and does answer: Quebec by name -> GADM -> the link -> WCVP's QUE, t1's only native area
  expect_equal(r$native_status, "Ne")
  expect_equal(r$regions_matched, "names")
})

test_that("without the GNRS backbone, names reach nothing, and status says so", {
  dir <- broken_cache()
  nanoparquet::write_parquet(
    data.frame(from_region = "gadm1:CAN.11_1", to_region = "wgsrpd3:QUE", relation = "same",
               fraction = 0.99, stringsAsFactors = FALSE),
    file.path(dir, "nsr-region-links.gz.parquet"), compression = "gzip")
  file.remove(file.path(dir, c("gnrs-country.gz.parquet", "gnrs-state_province.gz.parquet")))
  expect_false(NSR_local_status(dir)$answers_by_name[1])
  forget_warnings()
  expect_warning(r <- NSR_local(query, dir = dir, resolve_names = FALSE, quiet = TRUE),
                 "GNRS backbone is missing")
  expect_equal(r$native_status, "UNK")
})

# GVS's GADM index is one thing in two files: the raster of unit numbers and the table
# that decodes them.  A shared cache can hold one without the other.
test_that("the GADM index counts only when both of its files are present", {
  dir <- broken_cache()
  file.create(file.path(dir, "gadmindex-units-30s.tif"))
  expect_false(NSR:::nsr_geography(dir)$gadm_index)
  file.create(file.path(dir, "gadmindex-units.gz.parquet"))
  expect_true(NSR:::nsr_geography(dir)$gadm_index)
})

test_that("coordinates reach a geography directly, by crosswalk, or not at all", {
  geo <- function(raster, gadm, links) list(raster = raster, gadm_index = gadm, links = links)
  reach <- NSR:::nsr_coordinate_reach
  expect_equal(reach("wgsrpd3", geo(TRUE,  FALSE, FALSE)), "direct")
  expect_equal(reach("wgsrpd3", geo(FALSE, TRUE,  TRUE)),  "crosswalk")
  expect_equal(reach("wgsrpd3", geo(FALSE, TRUE,  FALSE)), "none")
  expect_equal(reach("gadm1",   geo(FALSE, TRUE,  FALSE)), "direct")
  expect_equal(reach("gadm1",   geo(TRUE,  FALSE, TRUE)),  "crosswalk")
  expect_equal(reach("gadm1",   geo(FALSE, FALSE, TRUE)),  "none")
})

# Without the WGSRPD raster, a point still lands in a GADM unit and reaches WCVP through
# the link table, exactly as a division name would.  Status and the warning must say that,
# rather than claiming WCVP is unreachable when it is answering.
test_that("a crosswalked route is reported as one, not as unreachable", {
  dir <- broken_cache()
  nanoparquet::write_parquet(
    data.frame(from_region = "gadm1:CAN.11_1", to_region = "wgsrpd3:QUE", relation = "same",
               fraction = 0.99, stringsAsFactors = FALSE),
    file.path(dir, "nsr-region-links.gz.parquet"), compression = "gzip")
  # Placeholders: only their existence is read here.  The query below carries no
  # coordinates, so nsr_locate_regions() never opens them - this test is about what status
  # and the warning SAY.  A real point through a real raster is tested further down.
  file.create(file.path(dir, c("gadmindex-units-30s.tif", "gadmindex-units.gz.parquet")))
  expect_false(any(c("latitude", "longitude") %in% names(query)))
  w <- NSR_local_status(dir)[1, ]
  expect_true(w$answers_by_coordinates)
  expect_equal(w$coordinate_route, "crosswalk")
  forget_warnings()
  expect_warning(
    NSR_local(query, dir = dir, resolve_names = FALSE, quiet = TRUE, use_coordinates = TRUE),
    "only through the crosswalk division names use")
  forget_warnings()
  msg <- tryCatch(
    NSR_local(query, dir = dir, resolve_names = FALSE, quiet = TRUE, use_coordinates = TRUE),
    warning = conditionMessage)
  expect_false(grepl("will be UNK", msg))
})

# The WGSRPD raster is a pair as well: nsr_build_wgsrpd_raster() writes the index, then the
# raster, so an interrupted build can leave either alone.
test_that("the WGSRPD raster counts only with its index", {
  dir <- broken_cache()
  file.create(NSR:::nsr_raster_path("wgsrpd3", dir))
  expect_false(NSR:::nsr_has_wgsrpd_raster(dir))
  expect_false(NSR:::nsr_geography(dir)$raster)
  file.create(NSR:::nsr_raster_path("wgsrpd3_index", dir))
  expect_true(NSR:::nsr_has_wgsrpd_raster(dir))
})

test_that("a raster without its index places nothing, rather than failing the query", {
  skip_if_not_installed("terra")
  dir <- broken_cache()
  file.create(NSR:::nsr_raster_path("wgsrpd3", dir))   # no index beside it
  loc <- NSR:::nsr_locate_regions(-71.2, 46.8, dir)
  expect_true(is.na(loc$wgsrpd3))
})

test_that("a source is built only if every table the query path loads is there", {
  dir <- broken_cache()
  expect_true(NSR_local_status(dir)$built[1])
  file.remove(file.path(dir, "nsr-checklist.gz.parquet"))
  expect_false(NSR_local_status(dir)$built[1])
})

test_that("a point placed only by the WGSRPD raster is matched on coordinates", {
  skip_if_not_installed("terra")
  dir <- broken_cache()
  r <- terra::rast(terra::ext(-72, -70, 46, 48), nrows = 1, ncols = 1, crs = "EPSG:4326")
  terra::values(r) <- 1L
  terra::writeRaster(r, NSR:::nsr_raster_path("wgsrpd3", dir), overwrite = TRUE)
  nanoparquet::write_parquet(
    data.frame(unit = 1L, area_code_l3 = "QUE", area_name = "Quebec", stringsAsFactors = FALSE),
    NSR:::nsr_raster_path("wgsrpd3_index", dir), compression = "gzip")
  expect_false(NSR:::nsr_has_gadm_index(dir))
  pt <- data.frame(species = "Sp one", latitude = 46.8, longitude = -71.2,
                   stringsAsFactors = FALSE)
  forget_warnings()
  r <- suppressWarnings(   # the cache has no link table, which is warned about elsewhere
    NSR_local(pt, dir = dir, resolve_names = FALSE, quiet = TRUE, use_coordinates = TRUE))
  expect_equal(r$regions_matched, "coordinates")
  # and the point does reach WCVP directly: t1 is native in QUE and nowhere else
  expect_equal(r$native_status, "Ne")
})

test_that("a raster unit the index cannot decode leaves the point unplaced", {
  skip_if_not_installed("terra")
  dir <- broken_cache()
  r <- terra::rast(terra::ext(-72, -70, 46, 48), nrows = 1, ncols = 1, crs = "EPSG:4326")
  terra::values(r) <- 2L                         # the index below only knows unit 1
  terra::writeRaster(r, NSR:::nsr_raster_path("wgsrpd3", dir), overwrite = TRUE)
  nanoparquet::write_parquet(
    data.frame(unit = 1L, area_code_l3 = "QUE", area_name = "Quebec", stringsAsFactors = FALSE),
    NSR:::nsr_raster_path("wgsrpd3_index", dir), compression = "gzip")
  loc <- NSR:::nsr_locate_regions(-71.2, 46.8, dir)
  expect_true(is.na(loc$wgsrpd3))                # not "wgsrpd3:NA"
})

test_that("an incomplete cache reports no reach either way", {
  dir <- broken_cache()
  file.remove(file.path(dir, "nsr-checklist.gz.parquet"))
  s <- NSR_local_status(dir)[1, ]
  expect_false(s$built)
  expect_true(is.na(s$answers_by_name))
  expect_true(is.na(s$answers_by_coordinates))
  expect_true(is.na(s$coordinate_route))
})

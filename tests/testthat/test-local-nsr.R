# Offline NSR. The pure helpers run anywhere; the rest need a built cache and are skipped
# unless the option NSR.test_cache names one.

test_that("division names match through bilingual and standard spellings", {
  bb <- list(
    country = data.frame(country_id = 1L, country = "Canada", gid_0 = "CAN",
                         stringsAsFactors = FALSE),
    state = data.frame(state_province_id = c(10L, 11L), country_id = 1L, country = "Canada",
                       state_province = c("New Brunswick/Nouveau-Brunswick", "Québec"),
                       state_province_std = c("New Brunswick/Nouveau-Brunswick", "Quebec"),
                       hasc_full = c("CA.NB", "CA.QC"), gid_1 = c("CAN.4_1", "CAN.11_1"),
                       stringsAsFactors = FALSE))
  r <- NSR:::nsr_match_poldiv(rep("Canada", 4),
                              c("New Brunswick", "Nouveau-Brunswick", "Quebec", "Atlantis"), bb)
  expect_equal(r$state_province_id, c(10L, 10L, 11L, NA_integer_))
  expect_equal(r$poldiv_level, c("state_province", "state_province", "state_province", "country"))
})

test_that("a place is judged by the polygons containing it", {
  db <- list(evaluable = "t1", covered = c("a", "b"))
  op <- function(status, relation, source = "powo") {
    data.frame(status = status, relation = relation, source_name = source,
               is_cultivated = 0L, stringsAsFactors = FALSE)
  }
  # the polygon itself, and any polygon containing it, answer directly
  expect_equal(NSR:::nsr_reduce(op("native", "same"), "a", TRUE, db)$code, "N")
  expect_equal(NSR:::nsr_reduce(op("introduced", "within"), "a", TRUE, db)$code, "I")
  r <- NSR:::nsr_reduce(op("native", "within"), "a", TRUE, db)
  expect_equal(r$code, "N")
  expect_equal(r$scope, "containing polygon")
  # among containing polygons, any native opinion wins, and disagreement is recorded
  r <- NSR:::nsr_reduce(rbind(op("native", "same"), op("introduced", "same", "usda")),
                        "a", TRUE, db)
  expect_equal(r$code, "N")
  expect_true(r$conflict)
  # polygons INSIDE the place describe parts of it: they answer only when unanimous
  r <- NSR:::nsr_reduce(rbind(op("native", "contains"), op("native", "contains")),
                        "a", TRUE, db)
  expect_equal(r$code, "N")
  expect_equal(r$scope, "sub-polygons agree")
  r <- NSR:::nsr_reduce(rbind(op("native", "contains"), op("introduced", "contains")),
                        "a", TRUE, db)
  expect_equal(r$code, "P")
  expect_match(r$reason, "varies among the polygons")
  expect_equal(r$n_sub_native, 1L)
  expect_equal(r$n_sub_introduced, 1L)
  # a containing polygon outranks the sub-polygons
  r <- NSR:::nsr_reduce(rbind(op("introduced", "within"), op("native", "contains")),
                        "a", TRUE, db)
  expect_equal(r$code, "I")
  # merely overlapping polygons describe neither the place nor its parts
  r <- NSR:::nsr_reduce(op("native", "overlaps"), "a", TRUE, db)
  expect_equal(r$code, "UNK")
  expect_match(r$reason, "partly overlapping")
})

test_that("absence is only read for taxa the sources know about", {
  db <- list(evaluable = "t1", covered = "a")
  # evaluable taxon, comprehensively listed region, no opinion -> absent
  r <- NSR:::nsr_reduce(NULL, "a", TRUE, db)
  expect_equal(r$code, "A")
  # a taxon no source holds information about -> unknown, never absent
  r <- NSR:::nsr_reduce(NULL, "a", FALSE, db)
  expect_equal(r$code, "UNK")
  expect_match(r$reason, "No source holds native status")
  # evaluable taxon, but no comprehensive source covers the region -> unknown
  expect_equal(NSR:::nsr_reduce(NULL, "z", TRUE, db)$code, "UNK")
})

cache <- getOption("NSR.test_cache", "")

test_that("NSR_local answers the reference cases (needs a built cache)", {
  skip_if(!nzchar(cache) || !file.exists(file.path(cache, "nsr-checklist.gz.parquet")),
          "no built NSR cache (option NSR.test_cache)")
  x <- data.frame(
    species = c("Pinus ponderosa", "Araucaria angustifolia", "Araucaria angustifolia",
                "Acer saccharum", "Sphagnum palustre", "Araucaria angustifolia"),
    country = c("United States", "Brazil", "Brazil", "Canada", "United States", NA),
    state_province = c("California", "Sao Paulo", "Amazonas", "New Brunswick", "California", NA),
    county_parish = "",
    latitude = c(NA, NA, NA, NA, NA, -23.5),
    longitude = c(NA, NA, NA, NA, NA, -47.5),
    stringsAsFactors = FALSE)
  r <- NSR_local(x, dir = cache, quiet = TRUE)
  expect_equal(r$native_status, c("N", "N", "A", "N", "UNK", "N"))
  # Sao Paulo is answered by the Brazilian flora, which WGSRPD cannot reach
  expect_match(r$native_status_sources[2], "flbr")
  # a moss is unknown, not absent: POWO holds no information about it
  expect_false(r$taxon_evaluable[5])
  # coordinates are resolved in each source's own geography
  expect_equal(r$regions_matched[6], "coordinates")
})

test_that("region links relate the geographies sensibly (needs a built cache)", {
  skip_if(!nzchar(cache) || !file.exists(file.path(cache, "nsr-region-links.gz.parquet")),
          "no built NSR cache")
  lk <- as.data.frame(nanoparquet::read_parquet(file.path(cache, "nsr-region-links.gz.parquet")))
  expect_true(all(lk$relation %in% c("same", "within", "contains", "overlaps")))
  expect_true(all(lk$fraction >= 0 & lk$fraction <= 1 + 1e-9))
  # Sao Paulo lies inside WGSRPD's Brazil Southeast
  sp <- lk[lk$from_region == "gadm1:BRA.25_1" & lk$to_region == "wgsrpd3:BZL", ]
  expect_equal(nrow(sp), 1)
  expect_true(sp$relation %in% c("within", "same"))
  # and the relation is recorded from both sides
  rev <- lk[lk$from_region == "wgsrpd3:BZL" & lk$to_region == "gadm1:BRA.25_1", ]
  expect_equal(nrow(rev), 1)
  expect_true(rev$relation %in% c("contains", "same"))
})

test_that("absence becomes introduction only for taxa confined elsewhere", {
  skip_if(!nzchar(cache) || !file.exists(file.path(cache, "nsr-checklist.gz.parquet")),
          "no built NSR cache (option NSR.test_cache)")
  db <- NSR:::nsr_local_db(cache)
  nat <- db$checklist[db$checklist$status == "native", ]
  per <- table(nat$taxon_id)
  # a species POWO gives a single native area, queried far away
  cal <- intersect(names(per)[per == 1], nat$taxon_id[nat$region_key == "wgsrpd3:CAL"])
  skip_if(!length(cal), "no single-area Californian native in this build")
  sp <- db$taxa$species_name[match(cal[1], db$taxa$taxon_id)]
  r <- NSR_local(data.frame(species = c(sp, sp), country = "United States",
                            state_province = c("Michigan", "California"),
                            county_parish = "", stringsAsFactors = FALSE),
                 dir = cache, quiet = TRUE)
  expect_equal(r$native_status, c("Ie", "Ne"))
  expect_equal(r$isIntroduced, c(1L, 0L))
  expect_equal(r$isEndemic, c(0L, 1L))
  expect_match(r$native_status_reason[1], "endemic to")
  # a widely native species merely unrecorded there stays absent
  wide <- names(per)[per > 20]
  wide <- setdiff(wide, db$checklist$taxon_id[db$checklist$region_key == "wgsrpd3:MIC"])
  skip_if(!length(wide), "no widespread species absent from Michigan")
  w <- db$taxa$species_name[match(wide[1], db$taxa$taxon_id)]
  r2 <- NSR_local(data.frame(species = w, country = "United States", state_province = "Michigan",
                             county_parish = "", stringsAsFactors = FALSE),
                  dir = cache, quiet = TRUE)
  expect_equal(r2$native_status, "A")
  expect_equal(r2$isIntroduced, 0L)
})

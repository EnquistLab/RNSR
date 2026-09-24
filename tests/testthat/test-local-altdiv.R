context("divisions that belong to another division system")

# Self-contained: the tables GNRS writes to the shared cache, in a temp directory.

dir <- file.path(tempdir(), "nsr-altdiv")
unlink(dir, recursive = TRUE)
dir.create(dir, recursive = TRUE, showWarnings = FALSE)
w <- function(x, table) nanoparquet::write_parquet(x, file.path(dir, paste0("altdiv-", table, ".gz.parquet")),
                                                   compression = "gzip")
w(data.frame(system = c("no-fylke", "se-landskap", "gb-vice-county"),
             entity_key = c("NO-VIKEN", "SE-LS-Uppland", "GB-VC"),
             country_iso = c("NO", "SE", "GB"),
             name = c("Viken", "Uppland", "Watsonian vice-county"),
             kind = c("superseded", "parallel", "parallel"),
             extent_known = c(TRUE, FALSE, FALSE), stringsAsFactors = FALSE), "units")
w(data.frame(entity_key = c("NO-VIKEN", "SE-LS-Uppland", "GB-VC"),
             name = c("Viken", "Uppland", "^VC ?[0-9]+"),
             match = c("exact", "exact", "regex"), stringsAsFactors = FALSE), "names")
w(data.frame(entity_key = "NO-VIKEN", level = 1L,
             gid = c("NOR.1_1", "NOR.4_1", "NOR.2_1"), stringsAsFactors = FALSE), "extent")

test_that("a division with a known extent contributes the units it covers", {
  k <- nsr_altdiv_keys("NO", "Viken", dir)
  expect_equal(k[[1]], c("gadm1:NOR.1_1", "gadm1:NOR.4_1", "gadm1:NOR.2_1"))
})

test_that("a division whose extent is unknown contributes nothing", {
  # recognised by GNRS, but there is no set of GADM units to ask about yet, so the
  # query stays where it was rather than being answered at the wrong level
  expect_equal(nsr_altdiv_keys("SE", "Uppland", dir)[[1]], character(0))
  expect_equal(nsr_altdiv_keys("GB", "VC57 Derbyshire", dir)[[1]], character(0))
})

test_that("an ordinary GADM division is left alone, and matching is per country", {
  expect_equal(nsr_altdiv_keys("NO", "Akershus", dir)[[1]], character(0))
  # "Viken" is a Norwegian entity: the same string under another country is not it
  expect_equal(nsr_altdiv_keys("SE", "Viken", dir)[[1]], character(0))
  expect_equal(nsr_altdiv_keys(NA_character_, NA_character_, dir)[[1]], character(0))
})

test_that("without the component built, nothing is claimed", {
  empty <- file.path(tempdir(), "nsr-altdiv-empty")
  unlink(empty, recursive = TRUE)
  dir.create(empty, recursive = TRUE, showWarnings = FALSE)
  expect_null(nsr_altdiv(empty))
  expect_equal(nsr_altdiv_keys("NO", "Viken", empty)[[1]], character(0))
})

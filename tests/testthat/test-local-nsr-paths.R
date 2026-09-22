# NSR_local() switches from the row resolver to the data.table set resolver at 200 rows
# (option NSR.set_min).  That switch must be invisible: the same query has to come back
# the same either way.  It has not been, twice - the set path never filled the per-level
# columns, and it reported a cultivated flag of 0 where the row path reports NA - so this
# compares the two directly, on tables held in memory rather than a built cache.

# a small world: Quebec in two geographies, inside Canada, plus a sliver of a region the
# query only just touches
fixture_db <- function() {
  checklist <- data.frame(
    taxon_id   = c("t1",          "t2",        "t3",          "t3",          "t4",          "t4"),
    region_key = c("wgsrpd3:QUE", "gadm0:CAN", "wgsrpd3:ONT", "wgsrpd3:BZL", "wgsrpd3:QUE", "gadm1:CAN.11_1"),
    status     = c("native",      "introduced", "native",     "native",      "present",     "native"),
    source_name = c("wcvp",       "vascan",    "wcvp",        "wcvp",        "wcvp",        "vascan"),
    is_cultivated = c(0L,          0L,          0L,            0L,            1L,            0L),
    stringsAsFactors = FALSE)
  links <- data.frame(
    from_region = c("gadm1:CAN.11_1", "wgsrpd3:QUE",    "gadm1:CAN.11_1"),
    to_region   = c("wgsrpd3:QUE",    "gadm1:CAN.11_1", "wgsrpd3:SLIVER"),
    relation    = c("same",           "same",           "within"),
    fraction    = c(0.99,             0.99,             0.001),
    stringsAsFactors = FALSE)
  taxa <- data.frame(
    taxon_id = c("t1", "t2", "t3", "t4"),
    species_name = c("Sp one", "Sp two", "Sp three", "Sp four"),
    family = "Fam", genus = "Gen", rank = "species", stringsAsFactors = FALSE)
  sources <- data.frame(source_name = c("wcvp", "vascan"), is_comprehensive = TRUE,
                        stringsAsFactors = FALSE)
  regions <- data.frame(
    region_key = c("wgsrpd3:QUE", "wgsrpd3:ONT", "wgsrpd3:BZL", "wgsrpd3:SLIVER",
                   "gadm1:CAN.11_1", "gadm0:CAN"),
    region_name = c("Quebec", "Ontario", "Brazil S", "Sliver", "Quebec", "Canada"),
    level = c("state_province", "state_province", "state_province", "state_province",
              "state_province", "country"),
    stringsAsFactors = FALSE)
  regions$system <- sub(":.*", "", regions$region_key)
  NSR:::nsr_index_db(list(sources = sources, taxa = taxa, checklist = checklist,
                          regions = regions, links = links))
}

# every query names Quebec, inside Canada
fixture_places <- function(n_taxa) {
  list(fine = rep(list("gadm1:CAN.11_1"), n_taxa),
       country = rep(list("gadm0:CAN"), n_taxa))
}

test_that("the row path and the set path agree, column for column", {
  skip_if_not_installed("data.table")
  db <- fixture_db()
  tid <- c("t1", "t2", "t3", "t4", NA_character_)
  places <- fixture_places(length(tid))

  row <- NSR:::nsr_resolve_status(tid, places, db)
  set <- NSR:::nsr_resolve_status_set(tid, places, db)

  for (f in c("code", "country_code", "state_code", "scope", "conflict", "conflict_type",
              "cultivated", "n_sub_native", "n_sub_introduced", "sources", "reason")) {
    expect_equal(set[[f]], row[[f]], info = f)
  }
})

test_that("the set path fills the per-level codes", {
  skip_if_not_installed("data.table")
  db <- fixture_db()
  tid <- c("t1", "t2")
  set <- NSR:::nsr_resolve_status_set(tid, fixture_places(2), db)
  # native in Quebec via the WGSRPD twin; nothing said about Canada as a whole
  expect_equal(set$state_code[1], "N")
  expect_false(is.na(set$country_code[1]))
  # introduced at country level propagates down to the state answer
  expect_equal(set$country_code[2], "I")
})

test_that("a level that was not asked about has no code", {
  skip_if_not_installed("data.table")
  db <- fixture_db()
  places <- list(fine = list(character(0)), country = list("gadm0:CAN"))
  for (res in list(NSR:::nsr_resolve_status("t2", places, db),
                   NSR:::nsr_resolve_status_set("t2", places, db))) {
    expect_true(is.na(res$state_code[1]))
    expect_equal(res$country_code[1], "I")
  }
})

test_that("absence leaves the cultivated flag unknown in both paths", {
  skip_if_not_installed("data.table")
  db <- fixture_db()
  # t3 is evaluable (it is native in Ontario and in Brazil, so its range is not confined
  # and the Ie rule does not fire) and Quebec is comprehensively listed, so a missing
  # opinion here is a real absence rather than a gap
  places <- fixture_places(1)
  row <- NSR:::nsr_resolve_status("t3", places, db)
  set <- NSR:::nsr_resolve_status_set("t3", places, db)
  expect_equal(row$code, "A")
  expect_equal(set$code, "A")
  expect_true(is.na(row$cultivated))
  expect_true(is.na(set$cultivated))
})

test_that("min_overlap keeps slivers out of the consulted set", {
  db <- fixture_db()
  keys <- "gadm1:CAN.11_1"
  # the default threshold drops the 0.1% link but keeps the twin
  expect_true("wgsrpd3:QUE" %in% NSR:::nsr_consulted_regions(keys, db))
  expect_false("wgsrpd3:SLIVER" %in% NSR:::nsr_consulted_regions(keys, db))
  # lower the threshold and it comes back, so it is the threshold doing the work
  expect_true("wgsrpd3:SLIVER" %in% NSR:::nsr_consulted_regions(keys, db, min_overlap = 0))
})

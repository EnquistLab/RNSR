# NSR_local() switches from the row resolver to the data.table set resolver at 200 rows
# (option NSR.set_min).  That switch must be invisible: the same query has to come back
# the same either way.  It has not been, twice - the set path never filled the per-level
# columns, and it reported a cultivated flag of 0 where the row path reports NA - so this
# compares the two directly, on tables held in memory rather than a built cache.

# a small world: Quebec in two geographies, inside Canada, plus a sliver of a region the
# query only just touches
fixture_db <- function() {
  # one row per taxon x region x source; is_extinct only ever set by WCVP
  spec <- c(
    "t1 wgsrpd3:QUE    native     wcvp   0 0",
    "t2 gadm0:CAN      introduced vascan 0 0",
    "t3 wgsrpd3:ONT    native     wcvp   0 0",
    "t3 wgsrpd3:BZL    native     wcvp   0 0",
    "t4 wgsrpd3:QUE    present    wcvp   1 0",
    "t4 gadm1:CAN.11_1 native     vascan 0 0",
    "t5 gadm1:CAN.11_1 native     vascan 0 0",
    "t5 gadm1:CAN.4_1  native     vascan 0 0",
    # t6 was native in Quebec and has been lost from it; it survives only in Brazil
    "t6 wgsrpd3:QUE    native     wcvp   0 1",
    "t6 wgsrpd3:BZL    native     wcvp   0 0",
    # t7 is the contrast: native ONLY in Brazil, never recorded in Quebec at all
    "t7 wgsrpd3:BZL    native     wcvp   0 0")
  f <- do.call(rbind, strsplit(trimws(spec), "[[:space:]]+"))
  checklist <- data.frame(
    taxon_id = f[, 1], region_key = f[, 2], status = f[, 3], source_name = f[, 4],
    is_cultivated = as.integer(f[, 5]), is_extinct = as.integer(f[, 6]),
    stringsAsFactors = FALSE)
  links <- data.frame(
    from_region = c("gadm1:CAN.11_1", "wgsrpd3:QUE",    "gadm1:CAN.11_1"),
    to_region   = c("wgsrpd3:QUE",    "gadm1:CAN.11_1", "wgsrpd3:SLIVER"),
    relation    = c("same",           "same",           "within"),
    fraction    = c(0.99,             0.99,             0.001),
    stringsAsFactors = FALSE)
  taxa <- data.frame(
    taxon_id = paste0("t", 1:7),
    species_name = paste("Sp", 1:7),
    family = "Fam", genus = "Gen", rank = "species", stringsAsFactors = FALSE)
  sources <- data.frame(source_name = c("wcvp", "vascan"), is_comprehensive = TRUE,
                        stringsAsFactors = FALSE)
  regions <- data.frame(
    region_key = c("wgsrpd3:QUE", "wgsrpd3:ONT", "wgsrpd3:BZL", "wgsrpd3:SLIVER",
                   "gadm1:CAN.11_1", "gadm1:CAN.4_1", "gadm0:CAN"),
    region_name = c("Quebec", "Ontario", "Brazil S", "Sliver", "Quebec",
                    "New Brunswick", "Canada"),
    level = c("state_province", "state_province", "state_province", "state_province",
              "state_province", "state_province", "country"),
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

# GADM keys carry their own hierarchy (BRA.25_1 sits in BRA).  Without it, a country
# query cannot see checklist rows published against that country's states, which is how
# VASCAN and Flora do Brasil publish - so native-up propagation and confined-country Ie
# both fail at country level.
test_that("a country query sees the states inside it", {
  skip_if_not_installed("data.table")
  db <- fixture_db()
  places <- list(fine = list(character(0)), country = list("gadm0:CAN"))
  row <- NSR:::nsr_resolve_status("t4", places, db)
  set <- NSR:::nsr_resolve_status_set("t4", places, db)
  # t4's only Canadian opinion is VASCAN's, recorded against Quebec the GADM state.
  # Without the hierarchy the country query cannot see it at all and answers A; with it
  # the state answers, and since Quebec is t4's only native region it is endemic there.
  expect_equal(row$code, "Ne")
  expect_equal(set$code, "Ne")
  expect_equal(row$scope, "sub-polygons agree")
  expect_equal(set$scope, "sub-polygons agree")
  expect_equal(set$sources, row$sources)
})

test_that("the hierarchy is exact, so min_overlap cannot filter it out", {
  db <- fixture_db()
  kids <- NSR:::nsr_gadm_children("gadm0:CAN", db)
  expect_setequal(kids, c("gadm1:CAN.11_1", "gadm1:CAN.4_1"))
  expect_equal(NSR:::nsr_gadm_parents("gadm1:CAN.4_1", db), "gadm0:CAN")
  # a state is a tiny share of its country, so as an overlap fraction it would be
  # dropped; the parent/child edges carry no fraction and are not filtered
  expect_setequal(NSR:::nsr_gadm_children("gadm0:CAN", db), kids)
  # regions with no checklist rows are not children of anything
  expect_equal(NSR:::nsr_gadm_children("gadm0:BRA", db), character(0))
})

test_that("a range confined to one country's states is confined to that country", {
  skip_if_not_installed("data.table")
  db <- fixture_db()
  # t5 is native in Quebec and New Brunswick and nowhere else; asked about a Brazilian
  # region that is comprehensively listed, it is absent there and endemic to Canada
  places <- list(fine = list("wgsrpd3:BZL"), country = list(character(0)))
  row <- NSR:::nsr_resolve_status("t5", places, db)
  set <- NSR:::nsr_resolve_status_set("t5", places, db)
  expect_equal(row$code, "Ie")
  expect_equal(set$code, "Ie")
  expect_match(row$reason, "Canada")
  expect_match(set$reason, "Canada")
})

# WCVP's extinct flag carries no date, so whether it counts belongs to the question
# rather than to the build: the cache keeps the row and the query decides.
test_that("exclude_extinct decides whether a lost population still answers", {
  skip_if_not_installed("data.table")
  db <- fixture_db()
  places <- list(fine = list("gadm1:CAN.11_1"), country = list(character(0)))
  for (f in list(NSR:::nsr_resolve_status, NSR:::nsr_resolve_status_set)) {
    gone <- f("t6", places, db, 0.01, exclude_extinct = TRUE)
    past <- f("t6", places, db, 0.01, exclude_extinct = FALSE)
    # present day: it is not there any more
    expect_equal(gone$code, "A")
    # as a historical distribution: it was native there
    expect_equal(past$code, "N")
  }
})

test_that("a lost population is absent, never introduced", {
  skip_if_not_installed("data.table")
  db <- fixture_db()
  places <- list(fine = list("gadm1:CAN.11_1"), country = list(character(0)))
  # t7 is native only in Brazil and was never in Quebec, so Quebec is an inferred
  # introduction - this is the machinery that must NOT fire for t6
  expect_equal(NSR:::nsr_resolve_status("t7", places, db)$code, "Ie")
  expect_equal(NSR:::nsr_resolve_status_set("t7", places, db)$code, "Ie")
  # t6's surviving range is equally confined to Brazil, but Quebec is part of its native
  # range whether or not the extinct record answers, so absence there is loss, not arrival
  expect_equal(NSR:::nsr_resolve_status("t6", places, db)$code, "A")
  expect_equal(NSR:::nsr_resolve_status_set("t6", places, db)$code, "A")
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

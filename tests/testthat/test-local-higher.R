# A genus speaks for a species only through claims true of every species in it, and only
# where the species has no record of its own here.  See R/local_higher.R and the design
# note, "Two sound downward inferences".

higher_db <- function() {
  spec <- c(
    # taxon      region_key        status      source   (comprehensive sources)
    "g_aca   wgsrpd3:QUE      introduced  wcvp",   # Acacia: introduced in Quebec
    "g_end   wgsrpd3:BZL      native      wcvp",   # Endemica: native only in Brazil S
    "g_nat   wgsrpd3:QUE      native      wcvp",   # Nativa: native in Quebec
    "s_own   wgsrpd3:QUE      native      wcvp",   # an Acacia with its own native record
    "g_par   gadm1:CAN.11_1   introduced  vascan", # Partia: introduced in one province only
    "g_mix   wgsrpd3:QUE      introduced  wcvp",   # Mixta: introduced per WCVP...
    "g_mix   gadm1:CAN.11_1   present     vascan") # ...merely present per VASCAN
  f <- do.call(rbind, strsplit(trimws(sub("#.*$", "", spec)), "[[:space:]]+"))
  checklist <- data.frame(taxon_id = f[, 1], region_key = f[, 2], status = f[, 3],
                          source_name = f[, 4], is_cultivated = 0L, is_extinct = 0L,
                          stringsAsFactors = FALSE)
  taxa <- data.frame(
    taxon_id     = c("g_aca", "s_aca", "s_own", "g_end", "s_end", "g_nat", "s_nat", "g_par", "s_par",
                     "g_mix", "s_mix"),
    species_name = c("Acacia", "Acacia one", "Acacia two", "Endemica", "Endemica one",
                     "Nativa", "Nativa one", "Partia", "Partia one", "Mixta", "Mixta one"),
    genus        = c("Acacia", "Acacia", "Acacia", "Endemica", "Endemica",
                     "Nativa", "Nativa", "Partia", "Partia", "Mixta", "Mixta"),
    family       = "Fam",
    rank         = c("Genus", "Species", "Species", "Genus", "Species",
                     "Genus", "Species", "Genus", "Species", "Genus", "Species"),
    stringsAsFactors = FALSE)
  NSR:::nsr_index_db(list(
    sources = data.frame(source_name = c("wcvp", "vascan"), is_comprehensive = TRUE,
                         stringsAsFactors = FALSE),
    taxa = taxa, checklist = checklist,
    regions = data.frame(region_key = c("wgsrpd3:QUE", "wgsrpd3:BZL", "gadm1:CAN.11_1", "gadm0:CAN"),
                         region_name = c("Quebec", "Brazil S", "Quebec", "Canada"),
                         system = c("wgsrpd3", "wgsrpd3", "gadm1", "gadm0"),
                         stringsAsFactors = FALSE),
    links = data.frame(from_region = c("gadm1:CAN.11_1", "wgsrpd3:QUE"),
                       to_region = c("wgsrpd3:QUE", "gadm1:CAN.11_1"),
                       relation = "same", fraction = 0.99, stringsAsFactors = FALSE)))
}

# both resolvers, through the same apply step NSR_local() and NSR_local_by_region() use
answer <- function(tid, places, db) {
  out <- list()
  for (path in c("row", "set")) {
    f <- if (path == "row") NSR:::nsr_resolve_status else NSR:::nsr_resolve_status_set
    resolve <- function(t, p) f(t, p, db)
    out[[path]] <- NSR:::nsr_apply_higher(resolve(tid, places), tid, places, resolve, db)
  }
  out
}
quebec <- function(n = 1) list(fine = rep(list("gadm1:CAN.11_1"), n), country = rep(list(character(0)), n))
canada <- function(n = 1) list(fine = rep(list(character(0)), n), country = rep(list("gadm0:CAN"), n))

test_that("a genus introduced here makes its unrecorded species introduced", {
  skip_if_not_installed("data.table")
  for (r in answer("s_aca", quebec(), higher_db())) {
    expect_equal(r$code, "I")
    expect_equal(r$scope, "genus")
    expect_match(r$reason, "its genus Acacia is introduced")
    expect_equal(r$sources, "wcvp")
  }
})

test_that("a genus endemic elsewhere makes its unrecorded species Ie", {
  skip_if_not_installed("data.table")
  for (r in answer("s_end", quebec(), higher_db())) {
    expect_equal(r$code, "Ie")
    expect_equal(r$scope, "genus")
    expect_match(r$reason, "its genus Endemica is endemic to Brazil S")
  }
})

test_that("a genus native here lends its species nothing", {
  skip_if_not_installed("data.table")
  # existential: SOME species of Nativa is native in Quebec, not necessarily this one
  for (r in answer("s_nat", quebec(), higher_db())) {
    expect_equal(r$code, "UNK")
    expect_true(is.na(r$scope) || r$scope != "genus")
  }
})

test_that("a species' own record is never overridden by its genus", {
  skip_if_not_installed("data.table")
  for (r in answer("s_own", quebec(), higher_db())) expect_equal(r$code, "Ne")
})

test_that("a genus introduced in part of the place is not introduced in all of it", {
  skip_if_not_installed("data.table")
  db <- higher_db()
  # Partia is introduced in Quebec only: asked about Canada, that is one sub-polygon
  for (r in answer("s_par", canada(), db)) expect_false(r$code %in% c("I", "Ie"))
  # asked about Quebec itself, it is the place
  for (r in answer("s_par", quebec(), db)) expect_equal(r$code, "I")
})

test_that("the paths agree on a mixed batch, column for column", {
  skip_if_not_installed("data.table")
  tid <- c("s_aca", "s_end", "s_nat", "s_own", "s_par", NA_character_)
  a <- answer(tid, quebec(length(tid)), higher_db())
  for (f in c("code", "reason", "scope", "sources", "opinions")) {
    expect_equal(a$set[[f]], a$row[[f]], info = f)
  }
})

test_that("higher taxa are found from the taxa table, or from a submitted name", {
  db <- higher_db()
  h <- NSR:::nsr_higher_ids(c("s_aca", "g_aca", "unknown"), db, name = c(NA, NA, "Acacia novel"))
  expect_equal(h$genus_id, c("g_aca", NA, "g_aca"))   # a genus is not its own genus
})

test_that("an inferred answer carries the higher taxon's disagreement with it", {
  skip_if_not_installed("data.table")
  # WCVP calls Mixta introduced in Quebec, VASCAN merely present: still no native opinion,
  # so the species is I - but the sources did not agree, and that must show
  for (r in answer("s_mix", quebec(), higher_db())) {
    expect_equal(r$code, "I")
    expect_true(r$conflict)
    expect_equal(r$conflict_type, "sources")
  }
})

test_that("each half of a split place gets the same inferences as any answer", {
  skip_if_not_installed("data.table")
  db <- higher_db()
  # a record whose point is in Quebec and whose names say Brazil S
  places <- list(xy = list("wgsrpd3:QUE"), nm = list("wgsrpd3:BZL"))
  for (f in list(NSR:::nsr_resolve_status, NSR:::nsr_resolve_status_set)) {
    resolve <- function(t, p) f(t, p, db)
    s <- NSR:::nsr_split_answers("s_aca", places, TRUE, resolve, db)
    # Acacia is introduced in Quebec: the coordinates' answer is I, not UNK
    expect_equal(s$xy, "I")
    expect_equal(s$nm, "UNK")
  }
})

# A genus speaks for a species only through claims true of every species in it, and only
# where the species has no record of its own here.  See R/local_higher.R and the design
# note, "Two sound downward inferences".

higher_db <- function() {
  spec <- c(
    # taxon      region_key        status      source  extinct   (comprehensive sources)
    "g_aca   wgsrpd3:QUE      introduced  wcvp    0",   # Acacia: introduced in Quebec
    "g_end   wgsrpd3:BZL      native      wcvp    0",   # Endemica: native only in Brazil S
    "g_nat   wgsrpd3:QUE      native      wcvp    0",   # Nativa: native in Quebec
    "s_own   wgsrpd3:QUE      native      wcvp    0",   # an Acacia with its own native record
    "g_par   gadm1:CAN.11_1   introduced  vascan  0",   # Partia: introduced in one province only
    "g_mix   wgsrpd3:QUE      introduced  wcvp    0",   # Mixta: introduced per WCVP...
    "g_mix   gadm1:CAN.11_1   present     vascan  0",   # ...merely present per VASCAN
    "g_duo   gadm1:CAN.11_1   introduced  vascan  0",   # Duo: introduced in Quebec...
    "g_duo   gadm1:CAN.4_1    introduced  vascan  0",   # ...and in New Brunswick
    "s_lost  wgsrpd3:QUE      native      wcvp    1",   # an Endemica once native in Quebec
    "f_int   wgsrpd3:QUE      introduced  wcvp    0",   # family Introfam: introduced in Quebec
    "f_end   wgsrpd3:BZL      native      wcvp    0",   # family Endfam: native only in Brazil S
    "s_ovl   wgsrpd3:ONT      introduced  wcvp    1")   # an Acacia, extinct in an overlapping area
  f <- do.call(rbind, strsplit(trimws(sub("#.*$", "", spec)), "[[:space:]]+"))
  checklist <- data.frame(taxon_id = f[, 1], region_key = f[, 2], status = f[, 3],
                          source_name = f[, 4], is_cultivated = 0L,
                          is_extinct = as.integer(f[, 5]), stringsAsFactors = FALSE)
  taxa <- data.frame(
    taxon_id     = c("g_aca", "s_aca", "s_own", "g_end", "s_end", "g_nat", "s_nat", "g_par", "s_par",
                     "g_mix", "s_mix", "g_duo", "s_duo", "s_lost"),
    species_name = c("Acacia", "Acacia one", "Acacia two", "Endemica", "Endemica one",
                     "Nativa", "Nativa one", "Partia", "Partia one", "Mixta", "Mixta one",
                     "Duo", "Duo one", "Endemica lost"),
    genus        = c("Acacia", "Acacia", "Acacia", "Endemica", "Endemica",
                     "Nativa", "Nativa", "Partia", "Partia", "Mixta", "Mixta",
                     "Duo", "Duo", "Endemica"),
    family       = "Fam",
    rank         = c("Genus", "Species", "Species", "Genus", "Species",
                     "Genus", "Species", "Genus", "Species", "Genus", "Species",
                     "Genus", "Species", "Species"),
    stringsAsFactors = FALSE)
  # family-rank taxa, and species that reach them: two through genera the cache does not
  # hold, one through Acacia, whose genus answers first
  taxa <- rbind(taxa, data.frame(
    taxon_id     = c("f_int", "f_end", "s_fi", "s_fe", "s_both", "s_ovl"),
    species_name = c("Introfam", "Endfam", "Plainus one", "Plainia one", "Acacia three",
                     "Acacia four"),
    genus        = c(NA, NA, "Plainus", "Plainia", "Acacia", "Acacia"),
    family       = c("Introfam", "Endfam", "Introfam", "Endfam", "Endfam", "Fam"),
    rank         = c("Family", "Family", "Species", "Species", "Species", "Species"),
    stringsAsFactors = FALSE))
  NSR:::nsr_index_db(list(
    sources = data.frame(source_name = c("wcvp", "vascan"), is_comprehensive = TRUE,
                         stringsAsFactors = FALSE),
    taxa = taxa, checklist = checklist,
    regions = data.frame(
      region_key = c("wgsrpd3:QUE", "wgsrpd3:BZL", "gadm1:CAN.11_1", "gadm1:CAN.4_1",
                     "gadm1:CAN.9_1", "gadm0:CAN", "wgsrpd3:ONT"),
      region_name = c("Quebec", "Brazil S", "Quebec", "New Brunswick", "Ontario", "Canada",
                      "Ontario"),
      system = c("wgsrpd3", "wgsrpd3", "gadm1", "gadm1", "gadm1", "gadm0", "wgsrpd3"),
      stringsAsFactors = FALSE),
    # Quebec-the-state is QUE, and also overlaps a little of WGSRPD's ONT
    links = data.frame(from_region = c("gadm1:CAN.11_1", "wgsrpd3:QUE", "gadm1:CAN.11_1"),
                       to_region = c("wgsrpd3:QUE", "gadm1:CAN.11_1", "wgsrpd3:ONT"),
                       relation = c("same", "same", "overlaps"),
                       fraction = c(0.99, 0.99, 0.2), stringsAsFactors = FALSE)))
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

# A place can be several polygons in one geography: a division from another system (a
# post-reform Norwegian county) spans several GADM units.  Those are its parts.
two_units <- function(b) list(fine = list(c("gadm1:CAN.11_1", b)), country = list(character(0)))

test_that("a genus introduced in one part of a place is not introduced in all of it", {
  skip_if_not_installed("data.table")
  db <- higher_db()
  # Acacia: introduced in Quebec, unrecorded in New Brunswick
  for (r in answer("s_aca", two_units("gadm1:CAN.4_1"), db)) expect_false(r$code %in% c("I", "Ie"))
  # Duo: introduced in both
  for (r in answer("s_duo", two_units("gadm1:CAN.4_1"), db)) expect_equal(r$code, "I")
})

test_that("a place of several parts is covered only when every part is", {
  skip_if_not_installed("data.table")
  db <- higher_db()
  # Quebec is listed, Ontario (CAN.9_1) has no list at all: the place is not wholly
  # covered, so Endemica is not absent from it, and its species is not Ie
  for (r in answer("s_end", two_units("gadm1:CAN.9_1"), db)) expect_false(r$code %in% "Ie")
  expect_false(NSR:::nsr_place_covered(c("gadm1:CAN.11_1", "gadm1:CAN.9_1"), character(0), db))
  # two geographies for one place: either covered is enough
  expect_true(NSR:::nsr_place_covered(c("wgsrpd3:QUE", "gadm1:CAN.9_1"), character(0), db))
})

test_that("a lost population stays A, whatever its genus", {
  skip_if_not_installed("data.table")
  # once native in Quebec (an extinct record), in a genus endemic to Brazil S: the
  # record is the species' own, so its genus does not turn "gone" into "introduced"
  for (r in answer("s_lost", quebec(), higher_db())) expect_equal(r$code, "A")
})

# The family step runs only where the genus supplies no inference, by the same rules.
test_that("a family introduced here makes a species of an unknown genus introduced", {
  skip_if_not_installed("data.table")
  for (r in answer("s_fi", quebec(), higher_db())) {
    expect_equal(r$code, "I")
    expect_equal(r$scope, "family")
    expect_match(r$reason, "its family Introfam is introduced")
  }
})

test_that("a family endemic elsewhere makes a species of an unknown genus Ie", {
  skip_if_not_installed("data.table")
  for (r in answer("s_fe", quebec(), higher_db())) {
    expect_equal(r$code, "Ie")
    expect_equal(r$scope, "family")
    expect_match(r$reason, "its family Endfam is endemic to Brazil S")
  }
})

test_that("the genus answers before the family", {
  skip_if_not_installed("data.table")
  # Acacia (introduced here) and Endfam (endemic to Brazil S) would both answer; the genus,
  # the nearer taxon, does
  for (r in answer("s_both", quebec(), higher_db())) {
    expect_equal(r$code, "I")
    expect_equal(r$scope, "genus")
  }
})

test_that("a record on a polygon overlapping the place is the species' own, extinct or not", {
  skip_if_not_installed("data.table")
  # Acacia four has an extinct introduced record in ONT, which overlaps Quebec; with
  # exclude_extinct it gives no opinion, but it is still the species' own record bearing
  # on the place, so Acacia's introduced status is not carried down to it
  for (r in answer("s_ovl", quebec(), higher_db())) {
    expect_false(identical(r$scope, "genus"))
    expect_equal(r$code, "A")
  }
})

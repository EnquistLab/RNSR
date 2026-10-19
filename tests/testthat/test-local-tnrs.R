# The offline build resolves names with TNRS_local(), which TNRS added in 0.4.0, and reads
# WCVP from TNRS's cache.  Neither may be referenced in a way R CMD check resolves against
# the installed TNRS: with an older TNRS (CRAN's 0.3.6) the check warns that
# TNRS::TNRS_local is missing and that TNRS:::tnrs_cache_dir is unexported.

test_that("the package code doesn't call TNRS_local() or TNRS internals directly", {
  skip_on_cran()
  src <- test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "package sources not available")
  code <- unlist(lapply(list.files(src, "\\.R$", full.names = TRUE), readLines, warn = FALSE))
  code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl("TNRS::", code, fixed = TRUE)))
})

test_that("an older TNRS without TNRS_local() gives a clear error", {
  skip_if_not_installed("TNRS")
  local_mocked_bindings(nsr_has_export = function(pkg, name) FALSE)
  expect_error(nsr_tnrs_local(), "needs TNRS 0.4.0 or later.*install_github")
})

test_that("TNRS_local() is found when TNRS exports it", {
  skip_if_not_installed("TNRS")
  skip_if_not("TNRS_local" %in% getNamespaceExports("TNRS"), "TNRS predates TNRS_local()")
  expect_identical(nsr_tnrs_local(), getExportedValue("TNRS", "TNRS_local"))
})

test_that("WCVP is found in TNRS's cache directory, as TNRS sets it", {
  skip_if_not_installed("TNRS")
  d <- withr::local_tempdir()
  withr::local_options(TNRS.cache_dir = d)
  expect_null(nsr_find_wcvp_zip())
  file.create(file.path(d, "wcvp_dwca.zip"))
  expect_equal(normalizePath(nsr_find_wcvp_zip()), normalizePath(file.path(d, "wcvp_dwca.zip")))
})

# Pre-release checklist for NSR. Source the sections one at a time from the
# package root. This folder is in .Rbuildignore and never ships.
#
# The GitHub Actions workflow (.github/workflows/R-CMD-check.yaml) already runs
# R CMD check on Linux/macOS/Windows for every push, so the steps here are the
# ones CRAN asks for beyond that: the --as-cran incoming checks, the API-down
# checks that got 0.1.0 archived, URL and spelling checks, tarball size, and
# the remote CRAN-like builders.

# 1. Bookkeeping ------------------------------------------------------------
# Bump Version in DESCRIPTION, add an entry to NEWS, update cran-comments.md
# (its Test environments should list the runs below once they pass).
# usethis::use_version("patch")        # or "minor" / "major"
# usethis::use_release_issue()         # tickable checklist as a GitHub issue

# 2. Regenerate docs and figures --------------------------------------------
devtools::document()

# The README figures are drawn as SVG; the PNGs are rendered from them. Only
# needed after editing an SVG.
for (svg in list.files("man/figures", "\\.svg$", full.names = TRUE)) {
  img <- magick::image_read(rsvg::rsvg_png(svg, width = 2000))
  img <- magick::image_quantize(img, max = 256, dither = FALSE)
  magick::image_write(img, sub("\\.svg$", ".png", svg), format = "png")
}

# 3. Local checks -----------------------------------------------------------
devtools::test()
chk <- devtools::check(cran = TRUE, incoming = TRUE, remote = TRUE)
stopifnot(length(chk$errors) == 0, length(chk$warnings) == 0)
# Expected NOTE: "New submission / Package was archived on CRAN", and
# rWCVPdata (Suggests) not in mainstream repositories but available through
# Additional_repositories. Both are explained in cran-comments.md.
# "unable to verify current time" is the local network, not the package.

# URLs in Rd files, README and DESCRIPTION.
urlchecker::url_check()

# Spelling. There is no inst/WORDLIST yet: fix real typos, then run
# update_wordlist() to accept the remaining terms (taxon names, NSR, WCVP...).
spelling::spell_check_package()
# spelling::update_wordlist()          # accept the current list

# Tarball must stay comfortably under CRAN's 5 MB limit. man/figures ships.
tb <- devtools::build(path = tempdir(), quiet = TRUE)
cat(basename(tb), round(file.size(tb) / 1024), "KB\n")
stopifnot(file.size(tb) < 4 * 1024^2)

# 4. With the API down ------------------------------------------------------
# 0.1.0 was archived because CRAN's Windows machines couldn't reach the NSR
# API and NSR_metadata() stopped with an error. CRAN policy: functions that
# use the internet must fail gracefully (a message, no error or warning).
# These tests mock every exported API function through an unreachable API,
# an HTTP error, an empty reply and no internet access.
devtools::test(filter = "api_failure")

# The vignette calls the API too, and is rebuilt during CRAN's checks. Render
# it with the API unreachable: it must build, showing messages, not stop.
devtools::load_all()
testthat::with_mocked_bindings(
  rmarkdown::render("vignettes/NSR.Rmd", output_dir = tempdir(), quiet = TRUE),
  check_internet = function() TRUE,
  POST = function(...) stop("Could not resolve host: nsrapi.xyz"),
  .package = "NSR")

# 5. Remote builders (results arrive by email / GitHub) ---------------------
# All three Windows flavors failed for 0.1.0, so run all three.
devtools::check_win_devel()            # CRAN's Windows R-devel builder
devtools::check_win_release()
devtools::check_win_oldrelease()
# R-hub v2 runs in this repo's own GitHub Actions (.github/workflows/rhub.yaml);
# rhub::rhub_doctor() passed on 2026-10-19. The platforms below are the
# CRAN-flavoured set for a pure-R package: the three OS runners, current and
# patched R, the alternative-BLAS Fedora build, no-Suggests, and \donttest
# examples. The call dispatches the workflow and prints the run URL; the
# package must be committed and pushed first, since the run checks what is on
# GitHub.
rhub::rhub_check(platforms = c("linux", "windows", "macos",
                               "ubuntu-release", "ubuntu-next", "atlas",
                               "nosuggests", "donttest"))

# 6. Submit -----------------------------------------------------------------
# devtools::release()                  # interactive checklist, then uploads
# After acceptance: usethis::use_github_release(); usethis::use_dev_version()

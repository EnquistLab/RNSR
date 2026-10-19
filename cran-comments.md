## Resubmission of an archived package

NSR was archived on 2026-07-28 because check errors were not corrected in time. This
submission fixes their cause.

The errors were on the Windows flavors (r-devel, r-release and r-oldrel), in the examples
and in re-building the vignette. Those machines could not reach the NSR API, and
NSR_metadata() then stopped with "'names' attribute [3] must be the same length as the
vector [0]" instead of failing gracefully: it assigned NULL results into a list and then
named three elements.

What we changed:

* NSR_metadata() now gives an informative message and returns NULL when any part of the
  metadata can't be retrieved, as the package's other API functions already did.
* New tests (tests/testthat/test-NSR_api_failure.R) run every exported function that uses
  the API under an unreachable API, an HTTP error, an empty reply and no internet access,
  with the request mocked so no network is needed. Each must give a message and return
  NULL, with no error or warning. These tests fail on the archived version with the
  error CRAN reported.
* We rendered the vignette with the API made unreachable, and it builds, showing the
  messages in place of results.
* Tests that need the live API are skipped on CRAN or replay recorded responses (vcr).

This version also adds an offline resolver, NSR_local(), whose data are built on the
user's machine by NSR_local_build(). Nothing derived from the checklists ships with the
package, and the packages it needs are in Suggests. One of them, rWCVPdata, is not on
CRAN; it is available from the repository given in Additional_repositories, and it is
used only conditionally, through requireNamespace(). We dropped rWCVP from Suggests,
since it has been archived on CRAN.

## Test environments

* local Windows 11 x64, R 4.4.1

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  - New submission. Package was archived on CRAN: addressed above.
  - Suggests or Enhances not in mainstream repositories: rWCVPdata. It is available
    using the Additional_repositories specification, as above.

(Locally there is a second NOTE, "unable to verify current time", which comes from the
local network rather than the package.)

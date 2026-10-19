# CRAN archived NSR 0.1.0 because NSR_metadata() stopped with an error on the Windows check
# machines, which had internet access but could not reach the NSR API.  CRAN policy is that
# a package using an internet resource fails gracefully - a message, no error or warning -
# when that resource is unavailable.  These tests put every exported API function through
# that situation, and two related ones, with the request mocked: no network is used.

fake_response <- function(status, body) {
  structure(list(status_code = status, content = charToRaw(body)), class = "response")
}

failures <- list(
  "the API can't be reached" = function(...) stop("Could not resolve host: nsrapi.xyz"),
  "the API returns an HTTP error" = function(...) fake_response(500L, "Internal Server Error"),
  # what the live API sends for some records (https://github.com/ojalaquellueva/nsr/issues/7)
  "the API returns an empty body" = function(...) fake_response(200L, "")
)

api_calls <- list(
  NSR = function() NSR(nsr_testfile),
  NSR_simple = function() NSR_simple(species = "Acer rubrum", country = "Canada",
                                     state_province = "Ontario"),
  NSR_citations = function() NSR_citations(),
  NSR_sources = function() NSR_sources(),
  NSR_version = function() NSR_version(),
  NSR_metadata = function() NSR_metadata(),
  NSR_political_divisions = function() NSR_political_divisions(),
  NSR_political_divisions_by_checklist = function() NSR_political_divisions(by_country = FALSE),
  NSR_data_dictionary = function() NSR_data_dictionary(),
  NSR_data_dictionary_status = function() NSR_data_dictionary(native_status = TRUE)
)

for (failure in names(failures)) {
  for (fn in names(api_calls)) {
    test_that(paste0(fn, "() gives a message and NULL, not an error, when ", failure), {
      local_mocked_bindings(check_internet = function() TRUE, POST = failures[[failure]])
      expect_no_warning(suppressMessages(expect_message(r <- api_calls[[fn]]())))
      expect_null(r)
    })
  }
}

for (fn in names(api_calls)) {
  test_that(paste0(fn, "() gives a message and NULL without internet access"), {
    local_mocked_bindings(check_internet = function() FALSE,
                          POST = function(...) stop("no request should be sent"))
    suppressMessages(expect_message(r <- api_calls[[fn]](), "requires internet access"))
    expect_null(r)
  })
}

test_that("NSR_metadata() stops at the first part it can't retrieve", {
  requests <- 0
  local_mocked_bindings(check_internet = function() TRUE,
                        POST = function(...) {
                          requests <<- requests + 1
                          stop("Could not resolve host: nsrapi.xyz")
                        })
  suppressMessages(expect_message(r <- NSR_metadata(), "NSR metadata could not be retrieved"))
  expect_null(r)
  expect_equal(requests, 1)
})

test_that("nsr_core() doesn't mistake a same-named object in the user's workspace for a reply", {
  skip_on_cran() # it puts an object in the global environment, put back as it was on exit
  local_mocked_bindings(check_internet = function() TRUE,
                        POST = function(...) stop("Could not resolve host: nsrapi.xyz"))
  # a developer running devtools::test() may have a results_json of their own
  had <- exists("results_json", envir = globalenv(), inherits = FALSE)
  if (had) previous <- get("results_json", envir = globalenv())
  withr::defer(if (had) {
    assign("results_json", previous, envir = globalenv())
  } else {
    rm("results_json", envir = globalenv())
  })
  assign("results_json", fake_response(200L, '{"meta":{"db_version":"stale"}}'),
         envir = globalenv())
  expect_message(r <- NSR:::nsr_core(mode = "meta"))
  expect_null(r)
})

# NSR_from_coordinates() talks to a separate service whose address has changed before.
# It must say so and return NULL when that service is unavailable - never error - and its
# address must be settable without a release.  No network: the request is mocked.

coords <- data.frame(species = "Acer rubrum", latitude = 45.5, longitude = -73.6, id = "1",
                     stringsAsFactors = FALSE)
fake_response <- function(status, body) {
  structure(list(status_code = status, content = charToRaw(body)), class = "response")
}

test_that("an offline service gives a message and NULL, not an error", {
  local_mocked_bindings(check_internet = function() TRUE,
                        POST = function(...) fake_response(404L, "<!DOCTYPE html><html>ERR_NGROK_3200</html>"))
  expect_message(r <- NSR_from_coordinates(coords), "returned HTTP status 404")
  expect_null(r)
})

test_that("a request that fails outright gives a message and NULL", {
  local_mocked_bindings(check_internet = function() TRUE,
                        POST = function(...) stop("Could not resolve host"))
  expect_message(r <- NSR_from_coordinates(coords), "could not be reached")
  expect_null(r)
})

test_that("a reply that is not JSON gives a message and NULL", {
  local_mocked_bindings(check_internet = function() TRUE,
                        POST = function(...) fake_response(200L, "<html>maintenance</html>"))
  expect_message(r <- NSR_from_coordinates(coords), "expected format")
  expect_null(r)
})

test_that("the service address can be set with an option", {
  seen <- NULL
  local_mocked_bindings(check_internet = function() TRUE,
                        POST = function(url, ...) {
                          seen <<- url
                          fake_response(200L, '[{"id":"1","native_status":"N"}]')
                        })
  withr::local_options(NSR.coordinates_url = "https://nsr.example.org")
  r <- NSR_from_coordinates(coords)
  expect_equal(seen, "https://nsr.example.org/batch")
  expect_equal(r$native_status, "N")
})

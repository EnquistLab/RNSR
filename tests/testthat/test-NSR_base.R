context("nsr base")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")
  
  vcr::use_cassette("nsr_base", {
    results <- results <- NSR(occurrence_dataframe = nsr_testfile,
                              url = url)
  })
  
  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = nrow(nsr_testfile))
})

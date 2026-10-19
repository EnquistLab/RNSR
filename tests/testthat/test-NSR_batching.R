context("nsr batching")


test_that("batch index splits rows into batches of at most 5000", {

  expect_length(NSR:::nsr_batch_index(22), 1)
  expect_length(NSR:::nsr_batch_index(5000), 1)

  idx <- NSR:::nsr_batch_index(12001)
  expect_equal(object = unname(lengths(idx)), expected = c(5000, 5000, 2001))
  expect_equal(object = unlist(idx, use.names = FALSE), expected = 1:12001)

})


# Mimics the API: echoes each submitted row back in the API's response layout
# (an "id" column of field names followed by one column per observation),
# with the observations returned in reverse order.
mock_nsr_core <- function(calls) {
  function(mode, data_json = NULL, ...) {
    rows <- jsonlite::fromJSON(data_json)
    calls$sizes <- c(calls$sizes, nrow(rows))
    calls$first_rows <- c(calls$first_rows, list(rows[1, ]))
    rows <- rows[rev(seq_len(nrow(rows))), , drop = FALSE]
    as.data.frame(cbind(c("species", "country", "user_id"),
                        t(rows[, c(1, 2, 5), drop = FALSE])))
  }
}


test_that("NSR splits large inputs into batches and recombines them in input order", {

  n <- 12001
  input <- NSR_template(nrow = n)
  input$species <- paste("Species", seq_len(n))
  input$country <- "Canada"
  input$state_province <- ""
  input$county_parish <- ""
  input$user_id <- NULL

  calls <- new.env()
  local_mocked_bindings(nsr_core = mock_nsr_core(calls))

  results <- NSR(occurrence_dataframe = input)

  expect_equal(object = calls$sizes, expected = c(5000, 5000, 2001))

  # Each batch is sent positionally in NSR_template() column order
  expect_equal(object = unname(unlist(calls$first_rows[[2]])),
               expected = c("Species 5001", "Canada", "", "", "5001"))

  expect_equal(object = nrow(results), expected = n)
  expect_equal(object = results$species, expected = input$species)
  expect_equal(object = as.integer(results$user_id), expected = seq_len(n))

})


test_that("NSR returns NULL with a message if any batch fails", {

  input <- NSR_template(nrow = 6000)
  input$species <- "Acer rubrum"
  input$country <- "Canada"

  n_calls <- 0
  local_mocked_bindings(nsr_core = function(mode, data_json = NULL, ...) {
    n_calls <<- n_calls + 1
    if (n_calls == 2) {
      return(invisible(NULL))
    }
    rows <- jsonlite::fromJSON(data_json)
    as.data.frame(cbind(c("country", "user_id"),
                        t(rows[, c(2, 5), drop = FALSE])))
  })

  expect_message(results <- NSR(occurrence_dataframe = input),
                 "batch 2 of 2")
  expect_null(results)

})


test_that("NSR still rejects duplicated user_ids", {

  input <- NSR_template(nrow = 2)
  input$species <- "Acer rubrum"
  input$country <- "Canada"
  input$user_id <- c(1, 1)

  expect_error(NSR(occurrence_dataframe = input), "unique")

})

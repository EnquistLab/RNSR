#'Check the native status for plant species in a political region
#'
#'NSR returns information on native status for species within a political region.
#' @param occurrence_dataframe A properly formatted dataframe, see https://bien.nceas.ucsb.edu/bien/tools/nsr/batch-mode/
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing NSR results.
#' @details The NSR API accepts at most 5,000 observations per call. Larger inputs are split into batches of at most 5,000 rows, sent separately, and recombined in the order of the input.
#' @export
#' @examples \dontrun{
#'
#' results <- NSR(occurrence_dataframe = nsr_testfile)
#'   
#' # Inspect the results
#' head(results, 10)
#' # That's a lot of columns. Let's display one row vertically
#' # to get a better understanding of the output fields
#' results.t <- t(results[,2:ncol(results)]) 
#' results.t[,1,drop =FALSE]
#' # Summarize the main results
#' results[ 1:10, 
#' c("species", "country", "state_province", "native_status", "native_status_reason")]
#' 
#' # Compare summary flag isIntroduced to more detailed native_status values
#' # and inspect souces consulted
#' results[ 1:10, 
#' c("species", "country", "state_province", "native_status", "isIntroduced", "native_status_sources")]
#' 
#' 
#' 
#' 
#' }
NSR <- function(occurrence_dataframe,
                ...){
  
  #check that input is a data.frame
  if(!inherits(occurrence_dataframe,"data.frame")){
    stop("occurrence_dataframe should be a data.frame")
  }
  
  #Check that user_id is populated properly, and populate if not
  
  if(all(is.na(occurrence_dataframe$user_id))){
    
    occurrence_dataframe$user_id <- 
      1:nrow(occurrence_dataframe)
    
    # occurrence_dataframe <- 
    #   occurrence_dataframe[c("user_id",
    #                          "taxon",
    #                          "country",
    #                          "state_province",
    #                          "county_parish")]
    
  }
  
  
  if(any(duplicated(occurrence_dataframe$user_id))) {
    stop("user_id should be either null or populated by unique values")
  }
  

  # The API accepts at most 5,000 observations per call, so send the data in batches
  batch_index <- nsr_batch_index(n = nrow(occurrence_dataframe))

  results <- vector(mode = "list", length = length(batch_index))

  for(i in seq_along(batch_index)){

    # Convert the data to JSON (row subsets keep the original column order, which the API relies on)
    data_json <- toJSON(unname(occurrence_dataframe[batch_index[[i]], , drop = FALSE]))

    batch_results <- nsr_core(mode = "resolve",
                              data_json = data_json, ...)

    #If the API didn't work, return a null
    if(is.null(batch_results)){
      if(length(batch_index) > 1){
        message(paste("NSR query failed on batch", i, "of", length(batch_index)))
      }
      return(invisible(NULL))
    }

    #Reformat results
    rownames(batch_results) <- batch_results[,1]# Get header names from first row
    batch_results <- t(batch_results)# Transpose
    batch_results <- as.data.frame(batch_results)	# Convert to dataframe (again)
    batch_results <- batch_results[-1, , drop = FALSE] # Remove the first row.
    rownames(batch_results) <- NULL	# Reset row numbers

    #If the results are properly formatted, but don't have the right columns, return a null
    if(!"country" %in% colnames(batch_results)){
      message("There appears to be a problem with the API, improperly formatted data were returned.")
      return(invisible(NULL))
    }

    results[[i]] <- batch_results

  }

  results <- do.call(rbind, results)

  #Re-order results to match original data
  results <- results[match(table = results$user_id,
                           x = occurrence_dataframe$user_id),]
  
  #reset the row numbers
  rownames(results) <- NULL 
  
  return(results)

}

#'Split rows into API-sized batches
#'
#'Internal function returning a list of row indices, each of length at most batch_size
#' @param n Number of rows.
#' @param batch_size Maximum number of rows per batch. The NSR API accepts at most 5,000 observations per call.
#' @keywords internal
nsr_batch_index <- function(n, batch_size = 5000){

  if(n == 0){
    return(list(integer(0)))
  }

  split(seq_len(n), ceiling(seq_len(n) / batch_size))

}







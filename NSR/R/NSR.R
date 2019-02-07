#'Check the native status for plant species in a political region
#'
#'NSR_batch returns information on native status for species within a political region.
#' @param occurrence_dataframe A properly formatted dataframe, see http://bien.nceas.ucsb.edu/bien/tools/nsr/batch-mode/
#' @return Dataframe containing NSR results.
#' @import RCurl, rjson
#' @export
#' @examples \dontrun{
#' nsr_testfile <- read.csv("http://bien.nceas.ucsb.edu/bien/wp-content/uploads/2019/02/nsr_testfile.csv")
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
#' results[ 1:10, c("species", "country", "state_province", "native_status", "native_status_reason")]
#' 
#' # Compare summary flag isIntroduced to more detailed native_status values
#' # and inspect souces consulted
#' results[ 1:10, c("species", "country", "state_province", "native_status", "isIntroduced", "native_status_sources")]
#' 
#' 
#' 
#' 
#' }
NSR <- function(occurrence_dataframe){

  
#<Add code here to run either batch mode or simple mode, depending on number of rows>  
  
  
return(results)
  
}







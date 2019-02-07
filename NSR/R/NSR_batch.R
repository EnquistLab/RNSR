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
#' results <- NSR_batch(occurrence_dataframe = nsr_testfile)
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
NSR_batch <- function(occurrence_dataframe){
  
# Base url for NSR Batch API
url <- "http://bien.nceas.ucsb.edu/bien/apps/nsr/nsr_wsb.php"
  
# Convert to JSON
obs_json <- toJSON(unname(split(occurrence_dataframe, 1:nrow(occurrence_dataframe))))

# Construct the request
headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')

results_json <- postForm(url, .opts=list(postfields=obs_json, httpheader=headers))

# Give the input file name to the function.
results <- fromJSON(results_json)

# Convert JSON file to a data frame
# This takes a bit of work
results <- as.data.frame(results)	# Convert to dataframe

results <- t(results)	# Transpose

colnames(results) = results[1,] # Get header names from first row

results = results[-1, ] # Remove the first row.

results <- as.data.frame(results)	# Convert to dataframe (again)

rownames(results) <- NULL	# Reset row numbers

return(results)
  
}







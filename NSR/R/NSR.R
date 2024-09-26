#'Check the native status for plant species in a political region
#'
#'NSR returns information on native status for species within a political region.
#' @param occurrence_dataframe A properly formatted dataframe, see http://bien.nceas.ucsb.edu/bien/tools/nsr/batch-mode/
#' @return Dataframe containing NSR results.
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr POST add_headers
#' @export
#' @examples \dontrun{
#' nsr_testfile <- 
#'read.csv("http://bien.nceas.ucsb.edu/bien/wp-content/uploads/2019/02/nsr_testfile.csv")
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
NSR <- function(occurrence_dataframe){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  #Set this internally rather than as an argument, since this is the only one that makes sense here (for now)
  mode="resolve"
  
  # Base url for NSR Batch API
  url <- "https://nsrapi.xyz/nsr_wsb.php"
  
  
  # Convert to JSON
  #obs_json <- toJSON(unname(split(occurrence_dataframe, 1:nrow(occurrence_dataframe))))#old RCurl code
  obs_json <- toJSON(unname(occurrence_dataframe))
  
  
  # Construct the request
  #headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')#old RCurl code
  
  # Convert the options to data frame and then JSON
  opts <- data.frame( c(mode) )
  names(opts) <- c("mode")
  opts_json <-  toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Combine the options and data into single JSON body
  input_json <- paste0('{"opts":', opts_json, ',"data":', obs_json, '}' )
  
  # Send the API request
  # Send the request in a "graceful failure" wrapper for CRAN compliance
  tryCatch(expr = results_json <- POST(url = url,
                                       add_headers('Content-Type' = 'application/json'),
                                       add_headers('Accept' = 'application/json'),
                                       add_headers('charset' = 'UTF-8'),
                                       body = input_json,
                                       encode = "json"),
           error = function(e) {
             message("There appears to be a problem reaching the API.") 
           })
  
  #Return NULL if API isn't working
  if(!exists("results_json")){return(invisible(NULL))}
  
  #results_json <- postForm(url, .opts=list(postfields=obs_json, httpheader=headers))#old RCurl code
  
  # Give the input file name to the function.
  #results <- fromJSON(results_json)#old RCurl
  
  # Extract the response and convert to data frame
  results_raw <- fromJSON(rawToChar(results_json$content)) 
  results <- as.data.frame(results_raw)
  
  
  # Convert JSON file to a data frame
  # This takes a bit of work
  #results <- as.data.frame(results)	# Convert to dataframe
  rownames(results) <- results[,1]# Get header names from first row
  results <- t(results)# Transpose
  results <- as.data.frame(results)	# Convert to dataframe (again)
  results <- results[-1, ] # Remove the first row.
  rownames(results) <- NULL	# Reset row numbers
  
  return(results)
  
}







#'Get metadata on current NSR version
#'
#'Return metadata about the current NSR version
#' @return Dataframe containing current NSR version number, build date, and code version.
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples{
#' NSR_version_metadata <- NSR_version()
#' }
#' 
NSR_version <- function(){
  
  # Base url for NSR Batch API
  #url = "https://bien.nceas.ucsb.edu/nsr/nsr_wsb.php"		# Production (nimoy)
  #url = "https://nsrapi.xyz/nsr_wsb.php"		# Production (nimoy) - using domain name)
  url = "https://bien.nceas.ucsb.edu/nsrdev/nsr_wsb.php"		# Development (nimoy)
  
  
  mode <- "meta"		
  
  # Reform the options json again
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  opts_json <- jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Make the options + data JSON
  input_json <- paste0('{"opts":', opts_json, ',"data":', data_json, '}' )
  
  # Send the request
  results_json <- POST(url = url,
                       add_headers('Content-Type' = 'application/json'),
                       add_headers('Accept' = 'application/json'),
                       add_headers('charset' = 'UTF-8'),
                       body = input_json,
                       encode = "json")
  
  # Process the response
  results_raw <- fromJSON(rawToChar(results_json$content)) 
  results <- as.data.frame(results_raw)
  
  #Return the metadata, now properly formatted
  return(results)
  
}#NSR version

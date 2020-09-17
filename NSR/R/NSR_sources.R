#'Get information on sources used by the NSR
#'
#'Return metadata about the current NSR sources
#' @return Dataframe containing information about the sources used in the current NSR version.
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr POST add_headers
#' @export
#' @examples {
#' sources <- NSR_sources()
#' }
#' 
NSR_sources <- function(){
  mode = "sources"
  
  # URL for NSR API
  #url = "https://bien.nceas.ucsb.edu/nsr/nsr_wsb.php"		# Production (nimoy)
  #url = "https://nsrapi.xyz/nsr_wsb.php"		# Production (nimoy) - using domain name)
  url = "https://bien.nceas.ucsb.edu/nsrdev/nsr_wsb.php"		# Development (nimoy)
  
  
  
  # Reform the options json again
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  opts_json <- toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Make the options + data JSON
  input_json <- paste0('{"opts":', opts_json,'}' )
  
  # Send the request
  results_json <- POST(url = url,
                       add_headers('Content-Type' = 'application/json'),
                       add_headers('Accept' = 'application/json'),
                       add_headers('charset' = 'UTF-8'),
                       body = input_json,
                       encode = "json")
  
  # Process the response
  results_raw <- fromJSON(rawToChar(results_json$content)) 
  results <- as.data.frame(results_raw$sources)
  
  # Convert JSON file to a data frame
  return(results)
  
}#NSR sources

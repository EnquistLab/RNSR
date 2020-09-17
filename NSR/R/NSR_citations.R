#'Get citation information
#'
#'Returns information needed to cite the NSR
#' @return Dataframe containing bibtex-formatted citation information
#' @note This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please do remember to cite both the sources and the NSR, as the NSR couldn't exist without these sources!
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr POST add_headers
#' @export
#' @examples {
#' citation_info <- NSR_citations()
#' }
#' 
NSR_citations <- function(){
  mode = "citations"
  
  # URL for TNRS API
  #url = "https://tnrsapidev.xyz/tnrs_api.php"
  #url = "http://vegbiendev.nceas.ucsb.edu:8975/tnrs_api.php"
  #url = "https://tnrsapi.xyz/tnrs_api.php"
  url = "https://bien.nceas.ucsb.edu/nsrdev/nsr_wsb.php"		# Development (nimoy)
  

  # Reform the options json again
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  opts_json <- toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Make the options + data JSON
  input_json <- paste0('{"opts":', opts_json, '}' )
  
  # Send the request
  results_json <- POST(url = url,
                       add_headers('Content-Type' = 'application/json'),
                       add_headers('Accept' = 'application/json'),
                       add_headers('charset' = 'UTF-8'),
                       body = input_json,
                       encode = "json")
  
  # Process the response
  results_raw <- fromJSON(rawToChar(results_json$content)) 
  results <- as.data.frame(results_raw$citations)

  return(results)
  
}#NSR citations

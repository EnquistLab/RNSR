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
NSR_sources <- function(...){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  return(nsr_core(mode = "sources",...))

}#NSR sources

#'Get metadata on current NSR version
#'
#'Return metadata about the current NSR version
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing current NSR version number, build date, and code version.
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr POST
#' @export
#' @examples{
#' NSR_version_metadata <- NSR_version()
#' }
#'
NSR_version <- function(...){
  
  #Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  return(nsr_core(mode = "meta",...))

}#NSR version

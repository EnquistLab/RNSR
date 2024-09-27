#'Get NSR data dictionary 
#'
#'Returns information from the NSR data dictionary
#' @param native_status Logical. If FALSE(Default) returns information on fields. If TRUE, returns information on Native Status categories. 
#' @param ... Additional arguments passed to internal functions.
#' @return List containing: (1) bibtex-formatted citation information, (2) information about NSR data sources, and (3) NSR version information.
#' @export
#' @examples {
#' metadata <- NSR_metadata()
#' }
#' 
NSR_data_dictionary <- function(native_status=FALSE, ...){

  if(native_status){
    
    return(nsr_core(mode = "dd_ns", ...)$dd)
    
  }else{
    
    return(nsr_core(mode="dd", ...)$dd)
    
  } 

}

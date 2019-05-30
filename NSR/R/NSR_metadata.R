#'Get metadata on NSR sources
#'
#'NSR_metadata returns metadata on the sources used by the NSR.
#' @param ... Additional parameters passed to internal functions.
#' @return data.frame containing NSR source metadata
#' @export
#' @examples \dontrun{
#' 
#' nsr_sources <- NSR_metadata()
#'  
#' }
NSR_metadata <- function(...){
  
  #Get metadata in json format
  #results_json <- getURL(url = url)
  
  results_json <- .handle_url(url = "http://bien.nceas.ucsb.edu/bien/apps/nsr/nsr_ws.php?do=meta&format=json",...)
  
  if(grepl(pattern = "http://bien.nceas.ucsb.edu",x = results_json)){return(results_json)}
  
  #Convert NULLs to NAs
  results_json<-gsub(pattern = "null",replacement = '\"\"',x = results_json)
  
  #Convert metadata from json format into a list of lists
  results <- fromJSON(results_json)
  
  #Convert lists into a dataframe
  output<-data.frame(matrix(unlist(results), nrow=length(results[[1]]), byrow=T),stringsAsFactors=FALSE)
  
  #Rename dataframe columns
  colnames(output)<-names(results[[1]][[1]][[1]])
  
  #Return the metadata, now properly formatted
  return(output)
  
}

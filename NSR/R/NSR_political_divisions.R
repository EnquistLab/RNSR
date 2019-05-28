#'Get information on checklists within the NSR
#'
#'NSR_political_divisions returns metadata on the sources used by the NSR.
#' @param country (optional) Character. Limits results to sources for a single country
#' @param checklist If TRUE (the default) limits the result to political divisions represented by one or more comprehensive checklists.
#' @return data.frame containing information on political divisions within the NSR database.
#' @note Setting checklist to FALSE returns a list of political divisions that can be to standardize spellings.
#' @export
#' @examples \dontrun{
#' 
#' #To get a list of all political divisions with comprehensive checklists:
#' nsr_checklists <- NSR_political_divisions()
#' 
#' #To get a list of political divisions to standardize spelling against:
#' nsr_poldivs <- NSR_political_divisions(checklist = F)
#' 
#' #To get checklist information for a single country:
#' 
#' nsr_checklists_canada <- NSR_political_divisions(country = "Canada")
#'  
#' }
NSR_political_divisions <- function(country = NULL, checklist = T){

  url <- "http://bien.nceas.ucsb.edu/bien/apps/nsr/nsr_ws.php?do=poldivs"
  
  #add country (optionally)
  
  if(!is.null(country)){url<-paste(url,"&country=",country,sep = "") }
  
  #add checklist
  
  if(checklist){url<-paste(url,"&checklist=true",sep = "")}
  if(!checklist){url<-paste(url,"&checklist=false",sep = "")}

  #specify json
  url<-paste(url, "&format=json",sep = "")
  

  #Get metadata in json format
  results_json <- getURL(url = url)
  
  #Convert NULLs to NAs
  results_json<-gsub(pattern = "null",replacement = '\"\"',x = results_json)
  
  #Convert metadata from json format into a list of lists
  
  results <- fromJSON(results_json,simplify = T)
  
  #Convert lists into a dataframe
  output<-data.frame(matrix(unlist(results), nrow=length(results[[1]]), byrow=T),stringsAsFactors=FALSE)
  
  #Rename dataframe columns
  colnames(output)<-names(results[[1]][[1]][[1]])
  
  #Return the metadata, now properly formatted
  return(output)
  
}

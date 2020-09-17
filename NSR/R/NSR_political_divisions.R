#'Get information on political divisions with checklists within the NSR
#'
#'NSR_political_divisions returns information on political divisions with checklist information present in the NSR.
#' @param country (optional) Character. Limits results to political divisions containing checklist information for a single country.
#' @param checklist If TRUE (the default) limits the result to political divisions represented by one or more comprehensive checklists.
#' @param ... Additional parameters passed to internal functions.
#' @return data.frame containing information on political divisions within the NSR database.
#' @note Setting checklist to FALSE returns a list of political divisions that can be used to standardize spellings.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
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
NSR_political_divisions <- function(country = NULL, checklist = T, ...){

  #url <- "http://bien.nceas.ucsb.edu/bien/apps/nsr/nsr_ws.php?do=poldivs"
  url <-  "https://bien.nceas.ucsb.edu/nsrdev/nsr_ws.php?do=poldivs"
  
  #add country (optionally)
  
  if(!is.null(country)){url<-paste(url,"&country=",country,sep = "") }
  url<-gsub(pattern = " ",replacement = "%20",x = url)
  
  
  #add checklist
  
  if(checklist){url<-paste(url,"&checklist=true",sep = "")}
  if(!checklist){url<-paste(url,"&checklist=false",sep = "")}

  #specify json
  url<-paste(url, "&format=json",sep = "")
  

  results_json <- GET(url = url)
  results_raw <- fromJSON(rawToChar(results_json$content)) 
  results_raw<-results_raw$nsr_results$nsr_result
  
  
  #If there were no results returned, print a message.  Otherwise, format the results and return them
  if(nrow(results_raw)==0){message("Country not found")}else{
  
  
  #Return the metadata, now properly formatted
  return(results_raw)
  }
}

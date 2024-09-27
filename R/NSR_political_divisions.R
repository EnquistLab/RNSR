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

  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }

  #url <- "http://bien.nceas.ucsb.edu/bien/apps/nsr/nsr_ws.php?do=poldivs"
  #url <-  "https://bien.nceas.ucsb.edu/nsrdev/nsr_ws.php?do=poldivs"
  url <-  "https://bien.nceas.ucsb.edu/nsrdev/nsr_ws.php"
  
  
  #url <- "https://nsrapi.xyz/nsr_wsb.php"
  url<- paste(url,"?do=poldivs",sep = "")
  
  #add country (optionally)
  
  if(!is.null(country)){
    url <- paste(url, "&country=", country, sep = "")
    }
  url <- gsub(pattern = " ", replacement = "%20",x = url)

  #add checklist
  
  if(checklist){
    url <- paste(url, "&checklist=true", sep = "")
    }
  if(!checklist)
  {url <- paste(url, "&checklist=false", sep = "")
    }

  #specify json
  url <- paste(url, "&format=json", sep = "")
  
  #Get results from the API, but in a wrapper to "fail gracefully"
  tryCatch(expr = results_json <- GET(url = url),
           error = function(e) {
             message("There appears to be a problem reaching the API.")
           })
  
  #Return NULL if API isn't working
  if(!exists("results_json")){return(invisible(NULL))}
  
  # catch results if text isn't properly returned
  
  tryCatch(expr = {
    results_raw <- fromJSON(rawToChar(results_json$content))
    results_raw <- results_raw$nsr_results$nsr_result
    },
    error = function(e){
      message("The API returned improperly formatted data")
      })

  # results_raw <- fromJSON(rawToChar(results_json$content)) 
  # results_raw <- results_raw$nsr_results$nsr_result
  
  #Return NULL if API isn't working
  if(!exists("results_raw")){return(invisible(NULL))}

  #If there were no results returned, print a message.  Otherwise, format the results and return them
  if(nrow(results_raw)==0){message("Country not found")}else{

  #Return the metadata, now properly formatted
  return(results_raw)
  }
}

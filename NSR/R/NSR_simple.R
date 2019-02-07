#'Check the native status for plant species in a political region
#'
#'NSR_simple returns information on native status for species within a political region.
#' @param occurrence_dataframe A properly formatted dataframe, see http://bien.nceas.ucsb.edu/bien/tools/nsr/batch-mode/
#' @return Dataframe containing NSR results.
#' @import RCurl, rjson
#' @export
#' @examples \dontrun{
#' 
#' nsr_testfile <- read.csv("http://bien.nceas.ucsb.edu/bien/wp-content/uploads/2019/02/nsr_testfile.csv")
#'
#' results <- NSR_simple(occurrence_dataframe = nsr_testfile)
#' 
#' }
NSR_simple <- function(occurrence_dataframe){
  
  #Set output dataframe
  output<-matrix(nrow = nrow(occurrence_dataframe),ncol = 11)
  output<-as.data.frame(output)
  
  
  # Base url for NSR Simple API
  base_url<-"http://bien.nceas.ucsb.edu/bien/apps/nsr/nsr_ws.php?"
    
  
  for(i in 1:nrow(occurrence_dataframe)){
    
  species_url<-paste("species=",occurrence_dataframe$species[i],sep = "")
  species_url<-gsub(pattern = " ",replacement = "%20",x = species_url)  
  
  #if there is country information, fill it in
  if(!is.na(occurrence_dataframe$country[i]) & occurrence_dataframe$country[i]!=""){  
  country_url<-paste("&country=",occurrence_dataframe$country[i],sep = "")
  #country_url<-gsub(pattern = " ",replacement = "%20",x = country_url)
  }
  
  #if there is state information, fill it in
  if(!is.na(occurrence_dataframe$state_province[i]) & occurrence_dataframe$state_province[i]!=""){  
  state_url<-paste("&stateprovince=",occurrence_dataframe$state_province[i],sep = "")
  #state_url<-gsub(pattern = " ",replacement = "%20",x = state_url)
  }else( state_url <- NULL)
  
  
  #if there is county information, fill it in
  if(!is.na(occurrence_dataframe$county_parish[i]) & occurrence_dataframe$county_parish[i]!=""){  
    county_url<-paste("&countyparish=",occurrence_dataframe$county_parish[i],sep = "")
    #county_url<-gsub(pattern = " ",replacement = "%20",x = county_url)
  }else(county_url<-NULL)
  
  
  #set return format
  format_url <- "&format=json"
  
  
  #set full URL
  full_url <- gsub(pattern = " ",replacement = "%20",x =  paste(base_url,species_url,country_url,state_url,county_url,format_url,sep = "") )
  
  
  results_json <- postForm(full_url, .opts=list(postfields=obs_json, httpheader=headers))
  results <- fromJSON(results_json)
  results<-t(results[[1]][[1]]$nsr_result)
  results<-as.data.frame(results)
  output[i,]<-results
  
  } # i loop
  
  colnames(output)<- colnames(results)  
  

  return(output)
  
}

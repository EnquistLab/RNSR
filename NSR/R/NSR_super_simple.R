#'Check the native status for plant species in a political region
#'
#'NSR_super_simple returns information on native status for species within a political region.
#' @param species A single species or a vector of species, with genus and specific epithet separated by a space.
#' @param country A single country or a vector of countries.  If a vector, length must equal length of species vector.
#' @param state_province A single state/province or a vector of states.  If a vector, length must equal length of species vector.
#' @param county_parish A single county/parish or a vector of counties.  If a vector, length must equal length of species vector.
#' @return Dataframe containing NSR results.
#' @import RCurl  rjson
#' @export
#' @examples \dontrun{
#' 
#' results <- NSR_super_simple(species = "Acer rubrum",country = "Canada",state_province = "Ontario")
#' }
NSR_super_simple <- function(species=NULL, country=NULL, state_province=NULL,county_parish=NULL){
  
  if(length(species) != length(country)){stop("Country and species vectors should be the same length.")}
  
  if(!is.null(state_province)){
    if(length(species) != length(state_province)){stop("State and vectors should be the same length.")}  
    
  }
  
  if(!is.null(county_parish)){
    if(length(species) != length(county_parish)){stop("County and vectors should be the same length.")}  
    
  }

  
  #Set output dataframe
  output<-matrix(nrow = length(species),ncol = 11)
  output<-as.data.frame(output)
  
  
  # Base url for NSR Simple API
  base_url<-"http://bien.nceas.ucsb.edu/bien/apps/nsr/nsr_ws.php?"
  
  
  for(i in 1:length(species)){
    
    species_url<-paste("species=",species[i],sep = "")
    species_url<-gsub(pattern = " ",replacement = "%20",x = species_url)  
    
    #if there is country information, fill it in
    if(!is.null(country[i])){  
      country_url<-paste("&country=",country[i],sep = "")
      #country_url<-gsub(pattern = " ",replacement = "%20",x = country_url)
    }
    
    #if there is state information, fill it in
    if(!is.null(state_province)){  
      state_url<-paste("&stateprovince=",state_province[i],sep = "")
      #state_url<-gsub(pattern = " ",replacement = "%20",x = state_url)
    }else( state_url <- NULL)
    
    
    #if there is county information, fill it in
    if(!is.null(county_parish) ){  
      county_url<-paste("&countyparish=",county_parish[i],sep = "")
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



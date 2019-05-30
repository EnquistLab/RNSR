
#'Process the URL as usual or return query information
#'
#'.handle_url returns the URL as usual, or else prints or returns the URL
#' @param url NSR URL to get using RCurl
#' @param return_url If TRUE, returns the URL rather than the results of the getURL call
#' @param print_url If TRUE, the URL is printed in addition to returning the NSR results (or URL)
#' @return If print_url = FALSE, NSR results from the specified query in JSON format.  IF print_url = T, the URL.
#' @note Internal function for de-bugging.
#' @keywords Internal
.handle_url<-function(url,return_url=F,print_url=F){
  
  if(print_url){print(url)}
  if(return_url){return(url)}
  
  #Get metadata in json format
  results_json <- RCurl::getURL(url = url)  
  
  
}



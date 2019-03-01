#'Make a template for an NSRS query
#'
#'NSRS_template builds a template that can be populated to submit an NSRS query.
#' @param nrow The number of rows to include in the template
#' @return Template data.frame that can be populated and then used in NSRS queries.
#' @export
#' @examples \dontrun{
#' 
#' template<-NSRS_template(nrow = 2)
#' template$genus<-"Acer"
#' template$species<-c("Acer rubrum", "Acer saccharum")
#' template$country<-"Canada"
#' template$user_id<-1:2
#' results <- NSRS(occurrence_dataframe = template)
#' 
#' }
NSRS_template <- function(nrow=1){
  
  
  template<-matrix(nrow = nrow, ncol= 7)
  template<-as.data.frame(template)
  colnames(template)<-c("family","genus","species","country","state_province","county_parish","user_id")      
  return(template)  
  
}



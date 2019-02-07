#'Check the native status for plant species in a political region
#'
#'NSR returns information on native status for species within a political region.
#' @param occurrence_dataframe A properly formatted dataframe, see http://bien.nceas.ucsb.edu/bien/tools/nsr/batch-mode/
#' @return Dataframe containing NSR results.
#' @export
#' @examples \dontrun{
#' nsr_testfile <- 
#' read.csv("http://bien.nceas.ucsb.edu/bien/wp-content/uploads/2019/02/nsr_testfile.csv")
#'
#' results <- NSR(occurrence_dataframe = nsr_testfile)
#' 
#' }
NSR <- function(occurrence_dataframe){


if(nrow(occurrence_dataframe)<=10){output<-NSR_simple(occurrence_dataframe = occurrence_dataframe)}
  
if(nrow(occurrence_dataframe)>10){output<- NSR_batch(occurrence_dataframe = occurrence_dataframe)}    
  
return(output)
  
}







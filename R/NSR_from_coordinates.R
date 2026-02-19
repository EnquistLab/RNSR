#'Check the native status for an occurrence using coordinates
#'
#'NSR returns information on native status for species based on their latitude and longitude.
#' @param occurrence_dataframe A properly formatted dataframe. Should contain the columns: species, latitude, and longitude, and ID in that order.
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing NSR results.
#' @importFrom jsonlite toJSON fromJSON
#' @import httr
#' @export
#' @examples \dontrun{
#'library(BIEN)
#'
#'# Get some data
#'
#'ar_occs <- BIEN_occurrence_species(species = "Acer rubrum",
#'                                   natives.only = FALSE,
#'                                   political.boundaries = TRUE,
#'                                   limit=100)
#'
#'
#'# Rename columns so that we can differentiate between coordinates declared
#'# and coordinates inferred
#'
#'colnames(ar_occs)[which(colnames(ar_occs)=="country")] <- "country_declared"
#'colnames(ar_occs)[which(colnames(ar_occs)=="state_province")] <- "state_declared"
#'colnames(ar_occs)[which(colnames(ar_occs)=="county")] <- "county_declared"
#'
#'# Select needed fields and add an ID column
#'
#'ar_occs_subset <- ar_occs[c("scrubbed_species_binomial","latitude","longitude")]
#'
#'# Toss duplicates (speeds up processing)
#'
#'ar_occs_subset <- unique(ar_occs_subset)
#'
#'# Add an id column
#'
#'ar_occs_subset$id <- as.character(1:nrow(ar_occs_subset))
#'
#'# Get results
#'
#'results_ar <- NSR_from_coordinates(occurrence_dataframe = ar_occs_subset)
#'
#'# Join results back to original data
#'
#'results_ar <- ar_occs_subset |>
#'  merge(y = results_ar,by.x = c("id"),
#'        by.y = c("input_id"))
#'
#'ar_occs <- ar_occs |> merge(results_ar,
#'                            by = c("latitude","longitude","scrubbed_species_binomial"))
#' }
NSR_from_coordinates <- function(occurrence_dataframe,
                ...){
  
  # check that input is a data.frame
  
    if(!inherits(occurrence_dataframe,"data.frame")){
      stop("occurrence_dataframe should be a data.frame")
    }
  
  # check the number of columns
  
    if(ncol(occurrence_dataframe) != 4){
      message("occurrence_dataframe should have 4 columns")
      return(invisible(NULL))
      }
      
  # check column formats
  
    if(!inherits(occurrence_dataframe[,1],"character")){
      message("first column should be character")
      return(invisible(NULL))
    }
  
    if(!inherits(x = occurrence_dataframe[,2],what = c("numeric","integer","double"))){
      message("second column should be numeric")
      return(invisible(NULL))
    }
    
    if(!inherits(x = occurrence_dataframe[,3],what = c("numeric","integer","double"))){
      message("second column should be numeric")
      return(invisible(NULL))
    }
  
  # check that ID field is unique
  
    if(any(duplicated(occurrence_dataframe[,4]))){
      message("Duplicate IDs detected")
      return(invisible(NULL))
    }
  
  # check that there are no NAs in the data.frame
  
    if(any(is.na(occurrence_dataframe))){
      message("NA values found in dataframe, please remove these records.")
      return(invisible(NULL))
    }
  
  # check for impossible coordinates
  
    if(any(occurrence_dataframe[,2] > 90 |
           occurrence_dataframe[,2] < -90 |
           occurrence_dataframe[,3] > 180 |
           occurrence_dataframe[,3] < -180
    )){
      
      message("Impossible coordinates found, please remove them before proceeding.")
      return(invisible(NULL))
      
    }
  
  
  
  # prepare query
  
    payload <- occurrence_dataframe
    colnames(payload) <- c("species", "lat", "lon","id")
    #payload$id <- as.character(seq_len(nrow(payload)))
    payload$id <- as.character(payload$id)
    
    base_url <- "https://f00f-128-111-85-31.ngrok-free.app" 

  # run query
    
    response <- POST(
      url = paste0(base_url, "/batch"),
      body = payload,
      encode = "json",
      add_headers("ngrok-skip-browser-warning" = "true"),
      content_type_json()
    )
  
  # format results and return
    
    results <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    return(results)
  
}

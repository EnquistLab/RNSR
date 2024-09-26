#'Check whether the internet is on
#'
#'Check for internet
#' @return TRUE if internet connection is detected, FALSE otherwise.
<<<<<<< HEAD:R/check_internet.R
#' @import RCurl
#' @keywords internal
check_internet <- function(){

    tryCatch(is.character(getURL("www.google.com")),
             error = function(e) {
               return(FALSE)
             })

}

=======
#' @importFrom httr GET
#' @keywords internal
check_internet <- function(){
  
  tryCatch(class(httr::GET("http://www.google.com/")) == "response",
           error = function(e) {
             return(FALSE)
           })
  
}
>>>>>>> 59890a7c41b86cd35660cfd631b100dad5c55dc5:NSR/R/check_internet.R

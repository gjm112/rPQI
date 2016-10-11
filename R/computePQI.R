#' Add together two numbers
#'
#' @param data the inpute data with diagnosis and procedure codes
#' @param pqi Which PQI's to calculate
#' @param append Should we append the output to the original data file.  Default is FALSE.
#' @param diagcode test
#' @param proccode test
#'
#'
#' @return The sum of \code{x} and \code{y}
#' @examples
#' add(1, 1)
#' add(10, 1)
#'
#'

#document(pkg="/Users/gregorymatthews/Dropbox/rPQI")

computePQI <- function(data = data, pqi = c(1,3,5), append = FALSE, diagcode = "dx", proccode = "px", agevar = "age", version = "5.0"){

  #rename variables to dx and px.
  #diagnosis codes
  paste0(diagcode,1:10)
  #procedure codes
  paste0(proccode,1:10)
  #Olivia

  ##########
  #PQI1
  ##########



}

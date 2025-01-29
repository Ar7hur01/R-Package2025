add <- function(x, y) {
  x + y
}
setwd("C:/Users/AD/Desktop/R_Package_24-01-25/ourPackage")
#' Title
#'
#' @param input
#'
#' @returns
#' @export
#'
#' @examples
hello <- function(
  input
  ) {
  print("Hello, world!")
}
installed.packages("roxygen2")
roxygen2::roxygenise()
library(roxygen2)

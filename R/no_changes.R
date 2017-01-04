## Count the number of level changes

#' Number of Changes in Factor Levels of the Design
#'
#' For a given design in matrix form, it counts the number of changes
#' in levels of each factor as well as overall number of changes in
#' the design.
#'
#' @param M a design matrix.
#' @return returns the number of changes in levels of each factor
#' and total number of such changes in the design
#' @export
#' @examples
#' # For generating a minimally changed CCD with 4 factors
#' des<-min_ccd(4)
#' # To count the number of level changes in the generated design
#' num.ch(des$CCD_design)

num.ch <- function(M){
  M <- as.matrix(M)
  n.runs <- function(x) {
    y <- rle(x)
    length(y$lengths) - 1
  }
  a1 <- apply(M, 2, n.runs)
  a2 <- sum(a1)
  list(per_factor = a1,
       total = a2
  )
}

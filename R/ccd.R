## Full Factorial: Minimally changed CCD
## Generate factorial points

#' Generate Factorial Points
#'
#' For a given number of factors, say k, it generates 2^k factorial
#' points such that the number of changes in the run sequence
#' is minimum.
#'
#' @param k an integer greater than
#' or equal to 2.
#' @return For \code{K} number of factors, generate
#' factorial points with minimum level changes.
#' @export
#' @examples
#' # For generating a full factorial for k=4 factors
#' # in 2 levels with minimum level changes
#' fact.pts(4)


fact.pts <- function(k){
  x1 <- c(-1, -1, 1, 1)
  x2 <- c(-1, 1, 1, -1)
  d.2k <- cbind(x1, x2) # factorial part when k=2
  rm(x1, x2)
  if(k == 2) {
    return(d.2k)
  } else {
    a <- c(-1, 1)
    b <- c(1, 1)
    c1 <- kronecker(a, rep(1,2 ^ (k - 1)))
    d.fact <- kronecker(b, fact.pts(k - 1))
    return( cbind(c1, d.fact))
  }
}

## minimally changed CCD

#' Minimally Changed CCD
#'
#' Generate minimally changed run sequence for central composite
#' designs (CCD).
#'
#' @param k an integer greater than
#' or equal to 2.
#' @return returns a minimally changed CCD for \code{K}
#' number of factors with full factorial points.
#' @export
#' @examples
#' # Generate minimally changed CCD with full factorial points
#' # for k=4 factors
#' min_ccd(4)


min_ccd <- function(k){
  environment(fact.pts) <- environment()
  environment(num.ch) <- environment()
  if(k < 2 | k %% 1 != 0){
    print(" ERROR: k should be an integer greater than or equal to 2")
  } else{
    alpha <- (2 ^ k) ^ (1 / 4)
    na <- 2 * k
    nc <- round(4 * 2 ^ (k / 2) + 4 - 2 * k)
    a <- c(-1, 1)
    d.ax <- kronecker(diag(k), a) * alpha
    d.cen <- matrix(0, nrow = nc, ncol = k)
    d.ax1 <- d.ax[-seq(nrow(d.ax), nrow(d.ax) - 1), ]
    d.ax2 <- d.ax[seq(nrow(d.ax), nrow(d.ax) - 1), ]
    d.ax.cen <- rbind(d.ax1, d.cen, d.ax2)
    d.fact <- fact.pts(k)
    des <- rbind(d.fact, d.ax.cen)
    n_ch <- num.ch(des)
    list(
      CCD_design = des,
      changes_factor = n_ch$per_factor,
      changes_total = n_ch$total
    )

  }
}

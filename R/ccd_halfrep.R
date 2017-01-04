## Minimal CCD with Half Replicate of the full factorialpoints

#' Generate Half Replicate of Full Factorial Points.
#'
#' For a given number of factors, say k, it generates 2^(k-1) factorial
#' points such that the number of changes in levels in the half
#' replicate is minimum.
#'
#' @param k an integer greter than or equal to 3.
#' @return For \code{K} number of factors, generate
#' factorial points in half replication with minimum level changes.
#' @export
#' @examples
#' # For generating a minimally changed half
#' # replicate of full factorial with 4 factors
#' # in 2 levels fact.pts_2(4)


fact.pts_2 <- function(k){
  environment(fact.pts) <- environment()
  d.f <- fact.pts(k - 1)
  a <- c(-1, 1)
  c1 <- kronecker(a, rep(1, 2 ^ (k - 2)))
  return(cbind(c1, d.f))
}

## Minimally changed CCD with half replicate of full factorial points

#' Generate Minimally Changed Central Composite
#'  designs (CCD) with Fractional Factorial Points (Half Replicate)
#'
#' @param k An integer greter than or equal to 3.
#' @return returns a minimally changed CCD for \code{K}
#' number of factors with half replicated factorial points.
#' @export
#' @examples
#' # Generate minimally changed CCD with fractional
#' # factorial points (half replicate) for k=4 factors
#' min_ccd_2(4)


min_ccd_2 <- function(k){
  environment(fact.pts_2) <- environment()
  environment(num.ch) <- environment()
  if(k < 4 | k %% 1 != 0){
    print("ERROR: k should be an integer greater than or equal to 4")
  } else{
    alpha <- (2 ^ k) ^ (1 / 4)
    na <- 2 * k
    nc <- round(4 * 2 ^ (k / 2) + 4 - 2 * k)
    a <-c (-1, 1)
    d.ax <- kronecker(diag(k), a) * alpha
    d.cen <- matrix(0, nrow = nc, ncol = k)
    d.ax1 <- d.ax[-seq(nrow(d.ax), nrow(d.ax) - 1), ]
    d.ax2 <- d.ax[seq(nrow(d.ax), nrow(d.ax) - 1), ]
    d.ax.cen <- rbind(d.ax1, d.cen, d.ax2)
    d.fact <- fact.pts_2(k)
    des <- rbind(d.fact, d.ax.cen)
    n_ch <- num.ch(des)
    list(
      CCD_half.rep_design = des,
      changes_factor = n_ch$per_factor,
      changes_total = n_ch$total
    )
  }
}

## Minimally changed BBD for given "k"

#' Minimally Changed BBD
#'
#' Generate Box Behnken design (BBD) with minimum level changes
#' in the run sequence.
#'
#' @param k An integer greter than or equal to 3.
#' @return returns a minimally changed BBD for the
#'  number of input factors as \code{v}.
#' @importFrom "utils" "combn"
#' @export
#' @examples
#' # To generate minimmaly changed BBD for k=4 factors
#' min_bbd(4)


min_bbd <- function (k) {
  environment(num.ch) <- environment()
  v <- k
  if(v < 3 | v %% 1 != 0 ) {
    print("ERROR: k should be an integer greater than or equal to 3")
  } else {

   bib <- t(combn(v,2))

  x1 <- c(-1, 1, 1, -1)
  x2 <- c(-1, -1, 1, 1)
  vc2 <- nrow(bib)
  des1 <- matrix(0, nrow = 4 * vc2, ncol = v)
  for(i in 1 : vc2){
    des1[(4 * i - 3) : (4 * i), bib[i, 1]] <- x1
    des1[(4 * i - 3) : (4 * i), bib[i, 2]] <- x2
  }
  des1[(nrow(des1) - 3) : nrow(des1), v] <-
    des1[(nrow(des1) - 3) : nrow(des1), v] * -1
  des2 <- matrix(0, nrow = v, ncol = v)
  des3 <- rbind(des1, des2)
  n_ch <- num.ch(des3)
  list(
    BBD_design = des3,
    changes_factor = n_ch$per_factor,
    changes_total = n_ch$total
  )
  }
}


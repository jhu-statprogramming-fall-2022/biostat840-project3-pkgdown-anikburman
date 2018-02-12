#' A pair distance for continuous variables.
#'
#' @description This function computes and returns the distance matrix computed by using
#' the specified distance measure to compute the pairwise distances between the rows of two data
#' of numerical variables.
#'
#' @param x A data matrix.
#' @param y A second data matrix.
#' @param method A  distance for numerical variables.
#' @details This is a two-data-set to compute distance.
#' It returns a matrix of all pairwise distances between rows in x and y.
#' The available distance are Manhattan weighted by rank ("mrw"), Squared Euclidean weighted by
#' variance ("sev"), Squared Euclidean weighted by rank ("ser"), and Squared Euclidean ("se").
#'
#' @author Weksi Budiaji
#'
#' @importFrom stats var
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' mrwdist[1:6,1:6]
#'
#' @export
distNumeric <- function(x, y, method = "mrw") {

  if((is.matrix(x)&&is.matrix(x))==FALSE)
    stop("x and y must be a matrix object!")

  if(ncol(x)!=ncol(y))
    stop(sQuote("x")," and ",sQuote("y"),
         " must have the same number of columns")
  ranked <- apply(rbind(x,y), 2, function(x) max(x)-min(x))
  variance <- apply(rbind(x,y), 2, var)

  num_distance <- c("mrw", "sev", "ser","se")
  method <- match.arg(method, num_distance)
  result <- switch(method,
                mrw = weightedNum(x, y, p = 1, alpha = ranked),
                ser = weightedNum(x, y, p = 2, alpha = ranked),
                sev = weightedNum(x, y, p = 2, alpha = variance),
                se = weightedNum(x, y, p = 2, alpha = 1))
  return(result)
}


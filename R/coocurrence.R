#' A co-occurrence distance for binary/ categorical variables data.
#'
#' @description This function computes and returns the distance matrix computed by
#' co-occurence distance.
#'
#' @param data A matrix or data frame of binary/ categorical variables. The values of matrix are
#' integer, i.e 1, 2, 3, ....
#'
#' @details This is a function to compute a co-occurence distance.
#' It returns a matrix of distance objects , i.e n x n.
#'
#' @return A matrix of distance from binary/ categorical variable.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Harikumar, S., PV, S., 2015. K-medoid clustering for heterogeneous data sets.
#' JProcedia Computer Science 70, 226-237.
#'
#' @examples
#' set.seed(1)
#' a <- matrix(sample(1:2, 7*3, replace = TRUE), 7, 3)
#' coocurance(a)
#'
#'
#' @export

coocurance <- function(data) {

  if(is.integer(data)==FALSE) stop ("The values in the matrix/ data frame are not integers.")

  col <- ncol(data)

  newdist <- function(data, col = col, colnum){
    nvar <- 1:col
    n <- length(levels(as.factor(data[,colnum])))
    var <- prob <- vector("list", (col-1))

    for (i in 1:(col-1)) {
      var[[i]] <- table(data[,nvar[-colnum][i]],data[,colnum])
      prob[[i]] <- var[[i]]/matrix(colSums(var[[i]]),
                                   nrow=nrow(var[[i]]), ncol = ncol(var[[i]]),
                                   byrow = TRUE)
    }

    probmat <- do.call(rbind,prob)
    matnew <- matrix(0, nrow = n, ncol = n)
    rownames(matnew) <- colnames(matnew) <- 1:n

    for (i in 1:n) {
      for (j in 1:n) {
        matnew[i,j] <- (sum(apply(probmat[,c(i,j)], 1, max))-(col-1))/(col-1)
      }
    }
    return(matnew)
  }

  newdata <- vector("list", col)
  for (i in 1:col) {
    newdata[[i]] <- newdist(data, col=col, i)
  }

  distmat <- matrix(0, nrow(data), nrow(data))
  for (i in 1:nrow(data)) {
    for (j in 1:nrow(data)){
      distsum <- numeric(col)
      for (k in 1:col) {
        distsum[k] <- newdata[[k]][data[i, k], data[j, k]]
      }
      distmat[i, j] <- sum(distsum)
    }
  }
  return(distmat)
}

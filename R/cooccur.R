#' Co-occurrence distance for binary/ categorical variables data
#'
#' @description This function calculates the co-occurrence distance proposed
#' by Ahmad and Dey (2007).
#'
#' @param data A matrix or data frame of binary/ categorical variables
#' (\emph{see} \strong{Details}).
#'
#' @details This function computes co-occurrence distance, which is a binary/
#' categorical distance, that based on the other variable's distribution
#' (\emph{see} \strong{Examples}).  In the \strong{Examples}, we have a data set:
#'
#' \tabular{lrrr}{
#' object \tab x \tab y \tab z \cr
#' 1 \tab 1 \tab 2 \tab 2 \cr
#' 2 \tab 1 \tab 2 \tab 1 \cr
#' 3 \tab 2 \tab 1 \tab 2 \cr
#' 4 \tab 2 \tab 1 \tab 2 \cr
#' 5 \tab 1 \tab 1 \tab 1 \cr
#' 6 \tab 2 \tab 2 \tab 2 \cr
#' 7 \tab 2 \tab 1 \tab 2
#' }
#'
#' The co-occurrence distance transforms each category of binary/ categorical
#' in a variable based on the distribution of other variables, for example,
#' the distance between categories 1 and 2 in the \emph{x} variable can be
#' different to the distance between categories 1 and 2 in the \emph{z}
#' variable. As an example, the transformed distance between categories 1 and 2
#' in the \emph{z} variable is presented.
#'
#' A cross tabulation between the \emph{z} and \emph{x} variables with
#' corresponding (column) proportion is
#'
#' \tabular{rrrrrr}{
#' \tab 1 \tab 2 \tab ||\tab 1 \tab 2 \cr
#' 1 \tab 2 \tab 1 \tab ||\tab 1.0 \tab 0.2 \cr
#' 2 \tab 0 \tab 4 \tab ||\tab 0.0 \tab 0.8
#' }
#'
#' A cross tabulation between the \emph{z} and \emph{y} variables with
#' corresponding (column) proportion is
#'
#' \tabular{rrrrrr}{
#' \tab 1 \tab 2 \tab ||\tab 1 \tab 2 \cr
#' 1 \tab 1 \tab 3 \tab ||\tab 0.5 \tab 0.6 \cr
#' 2 \tab 1 \tab 2 \tab ||\tab 0.5 \tab 0.4
#' }
#'
#' Then, the maximum values of the proportion in each row are taken such that
#' they are 1.0, 0.8, 0.6, and 0.5. The new distance between categories 1 and
#' 2 in the \emph{z} variable is
#' \deqn{\delta_{1,2}^z = \frac{(1.0+0.8+0.6+0.5) - 2}{2} = 0.45}
#' The constant \eqn{2} in the formula applies because the \emph{z} variable
#' depends on the 2 other variable distributions, i.e the \emph{x} and \emph{y}
#' variables. The new distances of each category in the
#' for the \emph{x} and \emph{y} variables can be calculated in a similar way.
#'
#' Thus, the distance between objects 1 and 2 is 0.45. It is only the \emph{z}
#' variable counted to calculate the distance between objects 1 and 2
#' because objects 1 and 2 have similar values in both the \emph{x} and \emph{y}
#' variables.
#'
#' The \code{data} argument can be supplied with either a matrix or data frame,
#' in which the class of the element has to be an integer. If it is not
#' an integer, it will be converted to an integer class. If the \code{data}
#' of a variable only, a simple matching is calculated. The co-occurrence
#' is absent due to its dependency to the distribution of other variables
#' and a \code{warning} message appears.
#'
#' @return Function returns a distance matrix (\emph{n x n}).
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Ahmad, A., and Dey, L. 2007. A K-mean clustering algorithm for
#' mixed numeric and categorical data. Data and Knowledge Engineering 63,
#' pp. 503-527.
#' @references Harikumar, S., PV, S., 2015. K-medoid clustering for heterogeneous data sets.
#' JProcedia Computer Science 70, 226-237.
#'
#' @examples
#' set.seed(1)
#' a <- matrix(sample(1:2, 7*3, replace = TRUE), 7, 3)
#' cooccur(a)
#'
#'
#' @export

cooccur <- function(data) {

  if((is.matrix(data)||is.data.frame(data))==FALSE)
    stop("The data must be a matrix or a data frame!")

  rn <- rownames(data)

  if(is.logical(data)==TRUE) data <- data.matrix(as.data.frame(data)) + as.integer(1)
  if(is.numeric(data)==TRUE) data <- apply(data, 2, function(x) as.integer(x))
  if(is.data.frame(data)==TRUE) data <- apply(data, 2, function(x) as.integer(as.factor(x)))

  col <- ncol(data)

  if (col != 1) {

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

    rownames(distmat) <- colnames(distmat) <- rn

  } else {

    distmat <- kmed::matching(data, data)
    rownames(distmat) <- colnames(distmat) <- rn
    warning('Due to only 1 variable, simple matching distance is calculated instead!
            To produce coocurrence distance, it requires at least 2 variables.')
  }

  return(distmat)

}



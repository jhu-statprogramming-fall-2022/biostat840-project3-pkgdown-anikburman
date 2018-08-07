#' Consensus matrix from A bootstrap replicate matrix
#'
#' @description This function create a consensus matrix from a bootstrap
#' replicate matrix.
#'
#'
#' @param bootdata A matrix of bootstrap replicate (n x b) membership.
#' @param nclust The number of clusters
#' @param reorder Any clustering algorithm function with the input is a distance and the end result being only membership.
#'
#' @details This is a function to obtain a consensus matrix from a bootstrap
#' evaluation for a cluster. The consensus matrix can be further plotted.
#'
#' @return Function returns a consensus matrix (n x n).
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @importFrom stats as.dist
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' parkboot <- function(x, nclust) {
#' res <- fastkmed(x, nclust, iterate = 50)
#' return(res$cluster)
#' }
#' irisboot <- clustboot(mrwdist, nclust=3, parkboot, nboot=7)
#' wardorder <- function(x, nclust) {
#' res <- hclust(x, method = "ward.D2")
#' member <- cutree(res, nclust)
#' return(member)
#' }
#' consensusiris <- consensusmatrix(irisboot, nclust = 3, wardorder)
#' consensusiris[c(1:5,51:55,101:105),c(1:5,51:55,101:105)]
#'
#' @export
consensusmatrix <- function(bootdata, nclust, reorder) {

  if(is.matrix(bootdata)==FALSE)
    stop("The input must be a bootstrap replicates matrix result")

  # if(is.integer(bootdata)==FALSE)
  #   stop("The input must be an integer bootstrap replicates matrix result")

  n <- nrow(bootdata)
  res <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      dat <- bootdata[c(i,j), bootdata[i,]!=0 & bootdata[j,]!=0, drop = FALSE]
      same <- sum(dat[1,] == dat[2,])
      nsame <- ncol(dat)
      if(nsame == 0) nsame <- 1
      res[i,j] <- same/ nsame
    }
  }
  resdist <- as.dist(1-res)
  reorder <- match.fun(reorder)
  idobj <- reorder(resdist, nclust)
  rownames(res) <- colnames(res) <- idobj
  result <- res[order(rownames(res)),order(colnames(res))]
  return(result)

}

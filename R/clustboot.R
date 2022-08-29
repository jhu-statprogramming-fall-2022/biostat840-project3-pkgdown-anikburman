#' Bootstrap replications for clustering alorithm
#'
#' @description This function does bootstrap replications for a clustering
#' algorithm. Any hard clustering algorithm is valid.
#'
#' @param distdata A distance matrix (n x n)/ dist object or
#' a data frame.
#' @param nclust A number of clusters.
#' @param algorithm A clustering algorithm function (see \strong{Details}).
#' @param nboot A number of bootstrap replicates.
#' @param diss A logical if \code{distdata} is a distance matrix/ object or
#' a data frame.
#'
#' @details This is a function to obtain bootstrap evaluation for cluster results.
#' The \code{algorithm} argument is a function where this function has two input
#' arguments. The two input arguments are a distance matrix/ object or
#' a data frame, and number of clusters. Then the output is only
#' a vector of cluster memberships.
#'
#' The default \code{algorithm} is \code{fastclust} applying the
#' \code{\link{fastkmed}} function. The code of the \code{fastclust} is
#'
#' fastclust <- function(x, nclust) \{
#'
#' res <- fastkmed(x, nclust, iterate = 50)
#'
#' return(res$cluster)
#'
#' \}
#'
#' For other examples, see \strong{Examples}. It applies ward and kmeans
#' algorithms. When kmeans is applied, for example, \code{diss} is set to be
#' \code{FALSE} because the input of the \code{kmclust} and
#' \code{\link{clustboot}} is a data frame instead of a distance.
#'
#' @return Function returns a matrix of bootstrap replicates with a dimension of
#' n x b, where n is the number of objects and b is the
#' number of bootstrap replicates.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Dolnicar, S. and Leisch, F. 2010. Evaluation of structure
#' and reproducibility of cluster solutions using the bootstrap.
#' Marketing Letters 21 pp. 83-101.
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' ward.D2 <- function(x, nclust) {
#' res <- hclust(as.dist(x), method = "ward.D2")
#' member <- cutree(res, nclust)
#' return(member)
#' }
#' kmclust <- function(x, nclust) {
#' res <- kmeans(x, nclust)
#' return(res$cluster)
#' }
#' irisfast <- clustboot(mrwdist, nclust=3, nboot=7)
#' head(irisfast)
#' irisward <- clustboot(mrwdist, nclust=3, algorithm = ward.D2, nboot=7)
#' head(irisward)
#' iriskmeans <- clustboot(num, nclust=3, algorithm = kmclust, nboot=7, diss = FALSE)
#' head(iriskmeans)
#'
#' @export

clustboot <- function(distdata, nclust=2, algorithm = fastclust, nboot=25, diss = TRUE) {

  if(any(is.na(distdata))) stop("Cannot handle missing values!")

  if((is.matrix(distdata)||inherits(distdata, "dist"))==FALSE)
    stop("The distdata must be a matrix or a dist object!")
  if(inherits(distdata, "dist")) distdata <- as.matrix(distdata)

  matboot <- matrix(0, nrow = nrow(distdata), ncol = nboot)
  algorithm <- match.fun(algorithm)
  for (i in 1:nboot) {
    idx <- sample(1:nrow(distdata), nrow(distdata), replace = TRUE)
    idx <- sort(unique(idx))
    if(diss == TRUE) {
      bootclust <- algorithm(distdata[idx,idx], nclust = nclust)
    } else {
      bootclust <- algorithm(distdata[idx,], nclust = nclust)
    }
    matboot[idx,i] <- bootclust
  }
  return(matboot)
}

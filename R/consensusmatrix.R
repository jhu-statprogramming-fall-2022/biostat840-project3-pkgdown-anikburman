#' Consensus matrix from A matrix of bootstrap replicates
#'
#' @description This function creates a consensus matrix from a matrix of
#' bootstrap replicates. It transforms an n x b matrix into an
#' n x n matrix, where n is the number of objects and b
#' is the number of bootstrap replicates.
#'
#' @param bootdata A matrix of bootstrap replicate (n x b)
#' (see \strong{Details}).
#' @param nclust A number of clusters.
#' @param reorder Any distance-based clustering algorithm function
#' (see \strong{Details}).
#'
#' @details This is a function to obtain a consensus matrix from a matrix of
#' bootstrap replicates to evaluate the clustering result. The
#' \code{bootdata} argument can be supplied directly from a matrix produced
#' by the \code{\link{clustboot}} function. The values of the consensus matrix,
#' \strong{A}, are calculated by
#' \deqn{a_{ij} = a_{ji} = \frac{\#n \:of \:objects \:i \:and \:j
#' \:in \:the \:same \:cluster}{\#n \:of \:objects \:i \:and \:j
#' \:sampled \:at \:the \:same \:time}}
#' where \eqn{a_{ij}} is the agreement index between objects i and
#' j. Note that due to the agreement between objects i and
#' j equal to the agreement between objects j and i,
#' the consensus matrix is a symmetric matrix.
#'
#' Meanwhile, the \code{reorder} argument is a function to reorder the objects
#' in both the row and column of the consensus matrix such that similar objects
#' are close to each other. This task can be solved by applying a clustering
#' algorithm in the consensus matrix. The \code{reorder} has to consist of
#' two input arguments. The two input arguments are a
#' distance matrix/ object and number of clusters.
#' The output is only a vector of cluster memberships. Thus,
#' the algorihtm that can be applied in the \code{reorder} argument is the
#' distance-based algorithm with a distance as the input.
#'
#' The default \code{reorder} is \code{fastclust} applying the
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
#' For other examples, see \strong{Examples}. It applies centroid and
#' complete linkage algorithms.
#'
#' @return Function returns a consensus/ agreement matrix of n x n
#' dimension.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Monti, S., P. Tamayo, J. Mesirov, and T. Golub. 2003. Consensus
#' clustering: A resampling-based method for class discovery and visualization
#' of gene expression microarray data. Machine Learning 52 pp. 91-118.
#'
#' @importFrom stats as.dist
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' irisfast <- clustboot(mrwdist, nclust=3, nboot=7)
#' consensusfast <- consensusmatrix(irisfast, nclust = 3)
#' centroid <- function(x, nclust) {
#' res <- hclust(as.dist(x), method = "centroid")
#' member <- cutree(res, nclust)
#' return(member)
#' }
#' consensuscentroid <- consensusmatrix(irisfast, nclust = 3, reorder = centroid)
#' complete <- function(x, nclust) {
#' res <- hclust(as.dist(x), method = "complete")
#' member <- cutree(res, nclust)
#' return(member)
#' }
#' consensuscomplete <- consensusmatrix(irisfast, nclust = 3, reorder = complete)
#' consensusfast[c(1:5,51:55,101:105),c(1:5,51:55,101:105)]
#' consensuscentroid[c(1:5,51:55,101:105),c(1:5,51:55,101:105)]
#' consensuscomplete[c(1:5,51:55,101:105),c(1:5,51:55,101:105)]
#'
#' @export
consensusmatrix <- function(bootdata, nclust, reorder = fastclust) {

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

#' Silhoutte index of each cluster.
#'
#' @description This function create a silhoutte index and its plot.
#'
#' @param distdata A distance object/ a n x n distance matrix.
#' @param idmedoid A vector of id medoids.
#' @param idcluster A vector of cluster membership.
#'
#'
#' @details This is a function to produce a silhoutte index and its plot
#' for each cluster. The id medoids must match with the cluster
#' membership, for example, if the id medoids are 2, 25, and 57 (3 medoids),
#' the idcluster must have 3 unique memberships.
#'
#' @return Function returns a silhoutte index and plot.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references P. J. Rousseeuw. 1987 Silhouettes: a graphical aid to
#' the interpretation and validation of cluster analysis.
#' Journal of Computational and Applied Mathematics, vol. 20, pp. 53-65
#'
#' @examples
#' distiris <- as.matrix(dist(iris[,1:4]))
#' res <- fastkmed(distiris, 3)
#' silh <- silhoutte(distiris, res$medoid, res$cluster)
#' silh$result[c(1:3,70:75,101:103),]
#' silh$plot
#'
#' @export

silhoutte <- function(distdata, idmedoid, idcluster) {

  if((is.matrix(distdata)||class(distdata)=="dist")==FALSE)
    stop("The distdata must be a matrix or a dist object!")
  if(is.matrix(distdata)==TRUE) {
    nr <- nrow(distdata)
    nc <- ncol(distdata)
    if (nc!=nr) stop("The distdata is not an x n distance matrix!")
  }
  if(class(distdata)=="dist") distdata <- as.matrix(distdata)

  nclust <- length(unique(idcluster))

  if (length(idmedoid) != nclust)
    stop("The idmedoid and idcluster do not match, revised them!")
  if (length(idcluster) != nrow(distdata))
    stop("The vector of membership must have the same length with the number of objects!")

  si <- vector("list", nclust)
  for (i in 1:nclust) {
    dista <- distdata[idcluster==i,idcluster==i, drop = FALSE]
    n <- max(2, ncol(dista))
    ai <- rowSums(dista)/(n-1)
    distb <- distdata[idcluster==i, idmedoid, drop = FALSE]
    bi <- numeric(length(ai))
    clclose <- apply(distb, 1, secondorder)
    for (j in 1:length(ai)) {
      distbi <- distdata[names(ai)[j],idcluster==clclose[j],drop = FALSE]
      bi[j] <- sum(distbi)/ length(distbi)
    }
    si[[i]] <- (bi-ai)/pmax(bi,ai)
  }
  vecsi <- do.call(c, si)
  ord <- order(as.numeric(names(vecsi)))
  result <- data.frame(silhoutte = vecsi[ord], cluster = idcluster)
  orderesult <- orderindex(result$silhoutte, result$cluster)
  plot1 <- plotsil(orderesult)
  return(list(result = result, plot = plot1))
}

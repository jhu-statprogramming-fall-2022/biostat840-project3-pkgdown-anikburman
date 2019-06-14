#' Silhouette index and plot
#'
#' @description This function creates silhouette indices and silhouette plots of
#' each cluster. The plot presents also the mean of the silhouette indices per
#' cluster.
#'
#' @param distdata A distance matrix (\emph{n x n}) or \emph{dist} object.
#' @param idmedoid A vector of id medoids (\emph{see} \strong{Details}).
#' @param idcluster A vector of cluster membership (\emph{see} \strong{Details}).
#' @param title A title of the plot.
#'
#' @details The silhouette index of object \emph{i} is calculated by
#' \deqn{si(i)=\frac{b_i-a_i}{max(a_i, b_i)}}
#' where \eqn{a_i} is the average distance of object \emph{i} to all objects
#' within the cluster, and \eqn{b_i} is the average distance of object \emph{i}
#' to all objects within the nearest cluster.
#'
#' The \code{idmedoid} argument corresponds to the \code{idcluster} argument.
#' If the length of \code{idmedoid} is 3, for example, the \code{idcluster} has
#' to have 3 unique memberships, or it returns \code{Error} otherwise. The
#' length of the \code{idcluster} has also to be equal to \emph{n}
#' (the number of objects).
#'
#' @return Function returns a list with following components:
#'
#' \code{result} is a data frame of the silhouette indices for all objects
#'
#' \code{plot} is the silhouette plots of each cluster.
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
#' silhouette <- sil(distiris, res$medoid, res$cluster)
#' silhouette$result[c(1:3,70:75,101:103),]
#' silhouette$plot
#'
#' @export

sil <- function(distdata, idmedoid, idcluster, title = "") {

  if((is.matrix(distdata)||inherits(distdata, "dist"))==FALSE)
    stop("The distdata must be a matrix or a dist object!")
  if(is.matrix(distdata)==TRUE) {
    nr <- nrow(distdata)
    nc <- ncol(distdata)
    if (nc!=nr) stop("The distdata is not an x n distance matrix!")
  }
  if(inherits(distdata, "dist")) distdata <- as.matrix(distdata)

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
  result <- data.frame(silhouette = vecsi[ord], cluster = idcluster)
  orderesult <- orderindex(result$silhouette, result$cluster)
  plot1 <- plotsil(orderesult, tit = title)
  return(list(result = result, plot = plot1))
}

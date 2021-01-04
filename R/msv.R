#' Medoid shadow value (MSV) index and plot
#'
#' @description This function computes medoid shadow values and shadow value plots of
#' each cluster. The plot presents the mean of the shadow values as well.
#'
#' @param distdata A distance matrix (\emph{n x n}) or \emph{dist} object.
#' @param idmedoid A vector of id medoids (\emph{see} \strong{Details}).
#' @param idcluster A vector of cluster membership (\emph{see} \strong{Details}).
#' @param title A title of the plot.
#'
#' @details The origin of the shadow value is calculated in the \code{shadow}
#' function of the \pkg{flexclust} package, in which it is based on the first and
#' second closest centroid. The \code{msv} function in this package modifies
#' the centroid into medoid such that the formula to compute shadow value of
#' object \emph{i} is
#' \deqn{msv(i) = \frac{d(i, m'(i))-d(i, m(i))}{d(i, m'(i))}}
#' where \eqn{d(i, m(i))} is the distance between object \emph{i} to the first
#' closest medoid and \emph{d(i, m'(i))} is the distance between object
#' \emph{i} to the second closest medoid.
#'
#' The \code{idmedoid} argument corresponds to the \code{idcluster} argument.
#' If the length of \code{idmedoid} is 3, for example, the \code{idcluster} has
#' to have 3 unique cluster memberships, or it returns \code{Error} otherwise.
#' The length of the \code{idcluster} has also to be equal to \emph{n}
#' (the number of objects). In contrast to the centroid shadow value,
#' the medoid shadow value is interpreted likewise a silhoutte value,
#' the higher value the better separation.
#'
#' @return Function returns a list with following components:
#'
#' \code{result} is a data frame of the shadow values for all objects
#'
#' \code{plot} is the shadow value plots of each cluster.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references F. Leisch. 2010 Neighborhood graphs, stripes and shadow plots
#' for cluster visualization. Statistics and Computing. vol. 20, pp. 457-469
#' @references
#' W. Budiaji. 2019 Medoid-based shadow value validation and visualization.
#' International Journal of Advances in Intelligent Informatics.  Vol 5 No 2
#' pp. 76-88
#'
#' @examples
#' distiris <- as.matrix(dist(iris[,1:4]))
#' res <- fastkmed(distiris, 3)
#' sha <- msv(distiris, res$medoid, res$cluster)
#' sha$result[c(1:3,70:75,101:103),]
#' sha$plot
#'
#' @export

msv <- function(distdata, idmedoid, idcluster, title = "") {

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

  rownames(distdata) <- 1:nrow(distdata)

  shad <- vector("list", nclust)
  for (i in 1:nclust) {
    dista1 <- distdata[idcluster==i, idmedoid[i], drop = FALSE]
    distmedo <- distdata[idcluster==i, idmedoid, drop = FALSE]
    clclose <- apply(distmedo, 1, secondorder)
    dista2 <- diag(distmedo[,clclose, drop = FALSE])
    shad[[i]] <- (dista2-dista1)/ dista2
  }

  vecsha <- do.call(rbind, shad)
  ord <- order(as.numeric(rownames(vecsha)))
  result <- data.frame(msv = vecsha[ord], cluster = idcluster)
  orderesult <- orderindex(result$msv, result$cluster)
  plot1 <- plotsil(orderesult, tit = title)
  return(list(result = result, plot = plot1))
}




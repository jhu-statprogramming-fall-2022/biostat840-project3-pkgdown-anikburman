#' Increasing number of clusters in k-medoids algorithm
#'
#' @description This function runs the increasing number of  clusters in
#' the k-medoids algorithm proposed by Yu et. al. (2018).
#'
#' @param distdata A distance matrix (\emph{n x n}) or \emph{dist} object.
#' @param ncluster A number of clusters.
#' @param iterate A number of iterations for the clustering algorithm.
#' @param alpha A stretch factor to determine the range of initial medoid
#' selection (\emph{see} \strong{Details}).
#'
#' @details This algorithm is claimed to manage with the weakness of the
#' simple and fast-kmedoids (\code{\link{fastkmed}}). The origin of the
#' algorithm is a centroid-based algorithm by applying the Euclidean distance.
#' Then, Bbecause the function is a medoid-based algorithm, the object mean
#' (centroid) and variance are redefined into medoid and deviation, respectively.
#'
#' The \code{alpha} argument is a stretch factor, i.e. a constant defined by
#' the user. It is applied to determine a set of medoid candidates. The medoid
#' candidates are calculated by
#' \eqn{O_c = }\{\eqn{X_i}| \eqn{\sigma_i \leq \alpha \sigma,
#' i = 1, 2, \ldots, n} \},
#' where \eqn{\sigma_i}  is the average deviation of object \emph{i}, and
#' \eqn{\sigma} is the average deviation of the data set. They are computed by
#' \deqn{\sigma = \sqrt{\frac{1}{n-1} \sum_{i=1}^n d(O_i, v_1)}}
#' \deqn{\sigma_i = \sqrt{\frac{1}{n-1} \sum_{i=1}^n d(O_i, O_j)}}
#' where \emph{n} is the number of objects, \eqn{O_i} is the object \emph{i},
#' and \eqn{v_1} is the most centrally located object.
#'
#' @return Function returns a list of components:
#'
#' \code{cluster} is the clustering memberships result.
#'
#' \code{medoid} is the id medoids.
#'
#' \code{minimum_distance} is the distance of all objects to their cluster
#' medoid.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Yu, D., Liu, G., Guo, M., Liu, X., 2018. An improved K-medoids
#' algorithm based on step increasing and optimizing medoids. Expert Systems
#' with Applications 92, pp. 464-473.
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' result <- inckmed(mrwdist, ncluster = 3, iterate = 50, alpha = 1.5)
#' table(result$cluster, iris[,5])
#'
#'
#' @export

inckmed <- function(distdata, ncluster, iterate = 10, alpha = 1) {

  if(any(is.na(distdata))) stop("Cannot handle missing values!")

  if((is.matrix(distdata)||inherits(distdata, "dist"))==FALSE)
    stop("The distdata must be a matrix or a dist object!")

  if(is.matrix(distdata)==TRUE) {
    nr <- nrow(distdata)
    nc <- ncol(distdata)
    if (nc!=nr) stop("The distdata is not an x n distance matrix!")
    }

  if(inherits(distdata, "dist")) distdata <- as.matrix(distdata)

  if(is.null(rownames(distdata))==TRUE) rownames(distdata) <- 1:nrow(distdata)

  n <- nrow(distdata)

  if (ncluster > n)
    stop ("The number of cluster is bigger than the number of objects, reduce it!")
  index <- 1:n

  sorted_object <- order(unique(colSums(distdata/sum(distdata))))
  o1 <- sorted_object[1]

  if (ncluster == 1) {
    medinit <- fastkmed(distdata, ncluster, init =o1)$medoid
  }

  sigma <- sum(distdata[,o1]-distdata[o1,o1])/(n-1)
  sigmai <- apply(distdata, 1, function (x) sum(x)/(n-1))
  sm <- index[sigmai <= sigma*1.1]
  distsm <- distdata[sm,o1,drop=FALSE]
  o2 <- sm[which.max(distsm)]

  med1 <- c(o1, o2)
  med1uni <- unique(med1)
  if (length(med1uni) != 2)
    stop ("Increase the value of alpha!")

  if (ncluster == 2) {
    medinit <- fastkmed(distdata, ncluster, init =med1)$medoid
  }

  if (ncluster > 2) {

    ncluster1 <- 2
    medinit <- fastkmed(distdata, ncluster1, init = med1)$medoid

    repeat{
      omed <- length(medinit)
      sigmatemp <- numeric(omed)
      smtemp <- distsm <- vector("list", omed)
      for (i in 1:omed) {
        sigmatemp[i] <- sum(distdata[,medinit[i]]-
                              distdata[medinit[i],medinit[i]])/(n-1)
        smtemp[[i]] <- index[sigmai <= sigmatemp[i]*1.1]
        for (l in 1:omed) {
          medidx <- smtemp[[i]][smtemp[[i]]!=medinit[l]]
          smtemp[[i]] <- medidx
        }
        distsm[[i]] <- distdata[smtemp[[i]],medinit[i],drop=FALSE]
      }
      iddistmax <- which.max(unlist(distsm))
      oi <- unlist(smtemp)[iddistmax]
      med2 <- c(medinit,oi)
      medinit <- fastkmed(distdata, length(med2), init =med2)$medoid
      if (ncluster1 == ncluster) {
        break
      }
      med1 <- med2
      ncluster1 <- ncluster1 + 1
    }
    medinit <- med1
  }

  result <- fastkmed(distdata, ncluster, iterate = iterate, init = medinit)

  return(result)
}

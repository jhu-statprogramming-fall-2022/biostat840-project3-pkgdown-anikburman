#' Consensus matrix heatmap from A consensus matrix
#'
#' @description This function creates a consensus matrix heatmap from a
#' consensus/ agreement matrix. The values of the consensus/ agreement
#' matrix are transformed in order to plot the heatmap.
#'
#' @param consmat A matrix of consensus/ agreement matrix (\emph{see}
#' \strong{Details}).
#' @param title A title of the plot.
#'
#' @details This is a function to produce a consensus matrix heatmap from a
#' consensus/ agreement matrix. A matrix produced by the
#' \code{\link{consensusmatrix}} function can be directly provided in the
#' \code{consmat} argument. The values of the consensus matrix, \strong{A},
#' are then transformed via a non-linear transformation by applying
#' \deqn{a_{ij}^{trf} = \frac{a_{ij} - min(a_{..})}{max(a_{..}) - min(a_{..})}}
#' where \eqn{a_{ij}} is the value of the consensus matrix in row \emph{i} and
#' column \emph{j}, and \eqn{a_{..}} is the all values of the matrix
#' (\eqn{\forall}\strong{A}).
#'
#' @return Function returns a heatmap plot.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Monti, S., P. Tamayo, J. Mesirov, and T. Golub. 2003. Consensus
#' clustering: A resampling-based method for class discovery and visualization
#' of gene expression microarray data. Machine Learning 52 pp. 91-118.
#' @references Hahsler, M., and Hornik, K., 2011. Dissimilarity plots:
#' A visual exploration tool for partitional clustering. Journal of
#' Computational and Graphical Statistics 20(2) pp. 335-354.
#'
#' @import ggplot2
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' irisfast <- clustboot(mrwdist, nclust=3, nboot=7)
#' complete <- function(x, nclust) {
#' res <- hclust(as.dist(x), method = "complete")
#' member <- cutree(res, nclust)
#' return(member)
#' }
#' consensuscomplete <- consensusmatrix(irisfast, nclust = 3, reorder = complete)
#' clustheatmap(consensuscomplete)
#'
#' @export

clustheatmap <- function(consmat, title = "") {

  if(is.matrix(consmat)==FALSE)
    stop("The input must be a consensus matrix result")

  nr <- nrow(consmat)
  nc <- ncol(consmat)
  meltdata <- data.frame(value = c(consmat),
                         Var1 = rep(1:nr, nc),
                         Var2 = rep(1:nc, each = nr))
  meltdata$grad <- (meltdata$value - min(meltdata$value))/
    (max(meltdata$value) - min(meltdata$value))
  gplot <- ggplot(meltdata, aes_string(x = "Var2", y = "Var1")) +
    geom_tile(aes(fill = "red"), alpha = meltdata$grad) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.border = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = "none", plot.caption = element_text(hjust = 0.5, size = 15)) +
    xlab("") +
    ylab("") +
    scale_y_reverse() +
    labs(caption = title)
  return(gplot)

}


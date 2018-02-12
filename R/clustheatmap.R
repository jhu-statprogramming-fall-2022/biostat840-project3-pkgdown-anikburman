#' A consensus matrix heatmap from A consensus matrix
#'
#' @description This function create a consensus matrix heatmap from a consensus
#' matrix.
#'
#' @param consmat A matrix of consensus matrix.
#' @param title A character of plot title
#'
#' @details This is a function to produce a consensus matrix heatmap from a consensus
#' matrix.
#'
#' @return Function returns a heatmap plot.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @import ggplot2
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
#' clustheatmap(consensusiris)
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


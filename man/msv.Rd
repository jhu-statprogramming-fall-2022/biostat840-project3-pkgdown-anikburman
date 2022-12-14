% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msv.R
\name{msv}
\alias{msv}
\title{Medoid shadow value (MSV) index and plot}
\usage{
msv(distdata, idmedoid, idcluster, title = "")
}
\arguments{
\item{distdata}{A distance matrix (n x n) or dist object.}

\item{idmedoid}{A vector of id medoids (see \strong{Details}).}

\item{idcluster}{A vector of cluster membership (see \strong{Details}).}

\item{title}{A title of the plot.}
}
\value{
Function returns a list with following components:

\code{result} is a data frame of the shadow values for all objects

\code{plot} is the shadow value plots of each cluster.
}
\description{
This function computes medoid shadow values and shadow value plots of
each cluster. The plot presents the mean of the shadow values as well.
}
\details{
The origin of the shadow value is calculated in the \code{shadow}
function of the \pkg{flexclust} package, in which it is based on the first and
second closest centroid. The \code{msv} function in this package modifies
the centroid into medoid such that the formula to compute shadow value of
object i is
\deqn{msv(i) = \frac{d(i, m'(i))-d(i, m(i))}{d(i, m'(i))}}
where \eqn{d(i, m(i))} is the distance between object i to the first
closest medoid and d(i, m'(i)) is the distance between object
i to the second closest medoid.

The \code{idmedoid} argument corresponds to the \code{idcluster} argument.
If the length of \code{idmedoid} is 3, for example, the \code{idcluster} has
to have 3 unique cluster memberships, or it returns \code{Error} otherwise.
The length of the \code{idcluster} has also to be equal to n
(the number of objects). In contrast to the centroid shadow value,
the medoid shadow value is interpreted likewise a silhoutte value,
the higher value the better separation.
}
\examples{
distiris <- as.matrix(dist(iris[,1:4]))
res <- fastkmed(distiris, 3)
sha <- msv(distiris, res$medoid, res$cluster)
sha$result[c(1:3,70:75,101:103),]
sha$plot

}
\references{
F. Leisch. 2010 Neighborhood graphs, stripes and shadow plots
for cluster visualization. Statistics and Computing. vol. 20, pp. 457-469

W. Budiaji. 2019 Medoid-based shadow value validation and visualization.
International Journal of Advances in Intelligent Informatics.  Vol 5 No 2
pp. 76-88
}
\author{
Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
}

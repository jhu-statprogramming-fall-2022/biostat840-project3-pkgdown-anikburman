## ------------------------------------------------------------------------
library(kmed)
num <- as.matrix(iris[,1:4])
mrwdist <- distNumeric(num, num, method = "mrw")
mrwdist[1:6,1:6]

## ------------------------------------------------------------------------
set.seed(1)
a <- matrix(sample(1:2, 7*3, replace = TRUE), 7, 3)
matching(a, a)
coocurance(a)

## ------------------------------------------------------------------------
a1 <- matrix(sample(1:3, 7*3, replace = TRUE), 7, 3)
mixdata <- cbind(iris[1:7,1:3], a, a1)
colnames(mixdata) <- c(paste(c("num"), 1:3, sep = ""), paste(c("bin"), 1:3, sep = ""), paste(c("cat"), 1:3, sep = ""))
distmix(mixdata, method = "gower", idnum = 1:3, idbin = 4:6, idcat = 7:9)
distmix(mixdata, method = "wishart", idnum = 1:3, idbin = 4:6, idcat = 7:9)

## ------------------------------------------------------------------------
result <- fastkmed(mrwdist, ncluster = 3, iterate = 50)
table(result$cluster, iris[,5])

## ------------------------------------------------------------------------
k <- 3
parkboot <- function(x, nclust) {
  res <- fastkmed(x, nclust, iterate = 50)
  return(res$cluster)
}
irisboot <- clustboot(mrwdist, nclust=k, parkboot, nboot=50)
irisboot[1:5,c(1:5,46:50)]

## ------------------------------------------------------------------------
kmboot <- function(x, nclust) {
  res <- kmeans(x, nclust)
  return(res$cluster)
}
kmboot <- clustboot(num, nclust=k, kmboot, nboot=50, diss = FALSE)
kmboot[1:5,c(1:5,46:50)]

## ------------------------------------------------------------------------
wardorder <- function(x, nclust) {
  res <- hclust(x, method = "ward.D2")
  member <- cutree(res, nclust)
  return(member)
}
consensusiris <- consensusmatrix(irisboot, nclust = k, wardorder)
consensusiris[c(1:5,51:55,101:105),c(1:5,51:55,101:105)]

## ------------------------------------------------------------------------
clustheatmap(consensusiris, "Iris Data via Park and Jun")

## ------------------------------------------------------------------------
consensusiris2 <- consensusmatrix(kmboot, nclust = k, wardorder)
clustheatmap(consensusiris2, "Iris Data via kmeans")


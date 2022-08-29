## -----------------------------------------------------------------------------
library(kmed)
iris[1:3,]

## -----------------------------------------------------------------------------
num <- as.matrix(iris[,1:4])
rownames(num) <- rownames(iris)
#calculate the Manhattan weighted by range distance of all iris objects
mrwdist <- distNumeric(num, num)
#show the distance among objects 1 to 3
mrwdist[1:3,1:3]

## -----------------------------------------------------------------------------
#extract the range of each variable
apply(num, 2, function(x) max(x)-min(x))

## -----------------------------------------------------------------------------
#the distance between objects 1 and 2
abs(5.1-4.9)/3.6 + abs(3.5 - 3.0)/2.4 + abs(1.4-1.4)/5.9 + abs(0.2-0.2)/2.4

## ---- echo=FALSE--------------------------------------------------------------
#data object
num[1:2,]

## -----------------------------------------------------------------------------
#calculate the squared Euclidean weighthed by range distance of all iris objects
serdist <- distNumeric(num, num, method = "ser")
#show the distance among objects 1 to 3
serdist[1:3,1:3]

## -----------------------------------------------------------------------------
#the distance between objects 1 and 2
(5.1-4.9)^2/3.6 + (3.5 - 3.0)^2/2.4 + (1.4-1.4)^2/5.9 + (0.2-0.2)^2/2.4

## -----------------------------------------------------------------------------
#calculate the squared Euclidean weighthed by squared range distance of 
#all iris objects
ser.2dist <- distNumeric(num, num, method = "ser.2")
#show the distance among objects 1 to 3
ser.2dist[1:3,1:3]

## -----------------------------------------------------------------------------
(5.1-4.9)^2/3.6^2 + (3.5 - 3.0)^2/2.4^2 + (1.4-1.4)^2/5.9^2 + (0.2-0.2)^2/2.4^2

## ---- echo=FALSE--------------------------------------------------------------
#data object
num[1:2,]

## -----------------------------------------------------------------------------
#calculate the squared Euclidean weighthed by variance distance of 
#all iris objects
sevdist <- distNumeric(num, num, method = "sev")
#show the distance among objects 1 to 3
sevdist[1:3,1:3]

## -----------------------------------------------------------------------------
#calculate the range of each variable
apply(num[,1:4], 2, function(x) var(x))

## -----------------------------------------------------------------------------
(5.1-4.9)^2/0.6856935 + (3.5 - 3.0)^2/0.1899794 + (1.4-1.4)^2/3.1162779 +
  (0.2-0.2)^2/0.5810063

## -----------------------------------------------------------------------------
#calculate the squared Euclidean distance of all iris objects
sedist <- distNumeric(num, num, method = "se")
#show the distance among objects 1 to 3
sedist[1:3,1:3]

## -----------------------------------------------------------------------------
(5.1-4.9)^2 + (3.5 - 3.0)^2 + (1.4-1.4)^2 + (0.2-0.2)^2

## -----------------------------------------------------------------------------
set.seed(1)
bin <- matrix(sample(1:2, 4*2, replace = TRUE), 4, 2)
rownames(bin) <- 1:nrow(bin)
colnames(bin) <- c("x", "y")

## -----------------------------------------------------------------------------
bin
#calculate simple matching distance
matching(bin, bin)

## -----------------------------------------------------------------------------
((1 == 1) + (1 == 2))/ 2

## -----------------------------------------------------------------------------
#calculate co-occurrence distance
cooccur(bin)

## -----------------------------------------------------------------------------
bin

## -----------------------------------------------------------------------------
#cross tabulation to define score in the y variable
(tab.y <- table(bin[,'x'], bin[,'y']))
#cross tabulation to define score in the x variable
(tab.x <- table(bin[,'y'], bin[,'x']))

## -----------------------------------------------------------------------------
#proportion in the y variable
(prop.y <- apply(tab.y, 2, function(x) x/sum(x)))
#proportion in the x variable
(prop.x <- apply(tab.x, 2, function(x) x/sum(x)))

## -----------------------------------------------------------------------------
#maximum proportion in the y variable
(max.y <- apply(prop.y, 2, function(x) max(x)))
#maximum proportion in the x variable
(max.x <- apply(prop.x, 2, function(x) max(x)))

## -----------------------------------------------------------------------------
#score mis-match in the y variable
(sum(max.y) - 1)/1
#score mis-match in the x variable
(sum(max.x) - 1)/1

## -----------------------------------------------------------------------------
cat <- matrix(c(1, 3, 2, 1, 3, 1, 2, 2), 4, 2)
mixdata <- cbind(iris[c(1:2, 51:52),3:4], bin, cat)
rownames(mixdata) <- 1:nrow(mixdata)
colnames(mixdata) <- c(paste(c("num"), 1:2, sep = ""), 
                       paste(c("bin"), 1:2, sep = ""), 
                       paste(c("cat"), 1:2, sep = ""))

## -----------------------------------------------------------------------------
mixdata

## -----------------------------------------------------------------------------
#calculate the Gower distance
distmix(mixdata, method = "gower", idnum = 1:2, idbin = 3:4, idcat = 5:6)

## -----------------------------------------------------------------------------
#extract the range of each numerical variable
apply(mixdata[,1:2], 2, function(x) max(x)-min(x))

## -----------------------------------------------------------------------------
#the Gower similarity
(gowsim <- ((1-abs(4.7-4.5)/3.3) + (1-abs(1.4-1.5)/1.3) + 1 + 1 + 0 + 1)/ 6 ) 

## -----------------------------------------------------------------------------
#the Gower distance
1 - gowsim

## -----------------------------------------------------------------------------
#calculate the Wishart distance
distmix(mixdata, method = "wishart", idnum = 1:2, idbin = 3:4, idcat = 5:6)

## -----------------------------------------------------------------------------
#extract the variance of each numerical variable
apply(mixdata[,1:2], 2, function(x) var(x))

## -----------------------------------------------------------------------------
wish <- (((4.7-4.5)^2/3.42) + ((1.4-1.5)^2/0.5225) + 0 + 0 + 1 + 0)/ 6 
#the Wishart distance
sqrt(wish)

## -----------------------------------------------------------------------------
#calculate Podani distance
distmix(mixdata, method = "podani", idnum = 1:2, idbin = 3:4, idcat = 5:6)

## -----------------------------------------------------------------------------
poda <- ((4.7-4.5)^2/3.3^2) + ((1.4-1.5)^2/1.3^2) + 0 + 0 + 1 + 0 
#the Podani distance
sqrt(poda)

## ---- echo=FALSE--------------------------------------------------------------
#data object
mixdata[3:4,]

## -----------------------------------------------------------------------------
#calculate the Huang distance
distmix(mixdata, method = "huang", idnum = 1:2, idbin = 3:4, idcat = 5:6)

## -----------------------------------------------------------------------------
#find the average standard deviation of the numerical variables
mean(apply(mixdata[,1:2], 2, function(x) sd(x)))

## -----------------------------------------------------------------------------
(4.7-4.5)^2 + (1.4-1.5)^2 + 1.286083*(0 + 0) + 1.286083*(1 + 0)

## -----------------------------------------------------------------------------
#calculate Harikumar-PV distance
distmix(mixdata, method = "harikumar", idnum = 1:2, idbin = 3:4, idcat = 5:6)

## -----------------------------------------------------------------------------
cooccur(mixdata[,5:6])

## -----------------------------------------------------------------------------
abs(4.7-4.5) + abs(1.4-1.5) + (0 + 0) + (0.5)

## ---- echo=FALSE--------------------------------------------------------------
#data object
mixdata[3:4,]

## -----------------------------------------------------------------------------
#calculate Ahmad-Dey distance
distmix(mixdata, method = "ahmad", idnum = 1:2, idbin = 3:4, idcat = 5:6)

## -----------------------------------------------------------------------------
cooccur(mixdata[,3:6])

## -----------------------------------------------------------------------------
(1.4-4.7)^2 + (0.2-1.4)^2 + (2)^2

## ---- echo=FALSE--------------------------------------------------------------
#data object
mixdata[2:3,]

## -----------------------------------------------------------------------------
#run the sfkm algorihtm on iris data set with mrw distance
(sfkm <- fastkmed(mrwdist, ncluster = 3, iterate = 50))

## -----------------------------------------------------------------------------
(sfkmtable <- table(sfkm$cluster, iris[,5]))

## -----------------------------------------------------------------------------
(3+11)/sum(sfkmtable)

## -----------------------------------------------------------------------------
#set the initial medoids
set.seed(1)
(kminit <- sample(1:nrow(iris), 3))
#run the km algorihtm on iris data set with mrw distance
(km <- fastkmed(mrwdist, ncluster = 3, iterate = 50, init = kminit))

## -----------------------------------------------------------------------------
(kmtable <- table(km$cluster, iris[,5]))

## -----------------------------------------------------------------------------
(3+9)/sum(kmtable)

## -----------------------------------------------------------------------------
#run the rkm algorihtm on iris data set with mrw distance and m = 10
(rkm <- rankkmed(mrwdist, ncluster = 3, m = 10, iterate = 50))

## -----------------------------------------------------------------------------
(rkmtable <- table(rkm$cluster, iris[,5]))

## -----------------------------------------------------------------------------
(3+3)/sum(rkmtable)

## -----------------------------------------------------------------------------
#run the inckm algorihtm on iris data set with mrw distance and alpha = 1.2
(inckm <- inckmed(mrwdist, ncluster = 3, alpha = 1.1, iterate = 50))

## -----------------------------------------------------------------------------
(inckmtable <- table(inckm$cluster, iris[,5]))

## -----------------------------------------------------------------------------
(9+3)/sum(inckmtable)

## -----------------------------------------------------------------------------
#run the sfkm algorihtm on iris data set with mrw distance
(simplekm <- skm(mrwdist, ncluster = 3, seeding = 50))

## -----------------------------------------------------------------------------
(simpletable <- table(simplekm$cluster, iris[,5]))

## -----------------------------------------------------------------------------
(4+11)/sum(simpletable)

## -----------------------------------------------------------------------------
#calculate silhouette of the RKM result of iris data set 
siliris <- sil(mrwdist, rkm$medoid, rkm$cluster, 
                     title = "Silhouette plot of Iris data set via RKM")

## -----------------------------------------------------------------------------
#silhouette indices of objects 49 to 52
siliris$result[c(49:52),]

## ---- fig.width=7, fig.asp=0.8------------------------------------------------
siliris$plot

## -----------------------------------------------------------------------------
#calculate centroid-base shadow value of the RKM result of iris data set 
csviris <- csv(mrwdist, rkm$medoid, rkm$cluster, 
                     title = "CSV plot of Iris data set via RKM")

## -----------------------------------------------------------------------------
#shadow values of objects 49 to 52
csviris$result[c(49:52),]

## ---- fig.width=7, fig.asp=0.8------------------------------------------------
csviris$plot

## -----------------------------------------------------------------------------
#calculate medoid-based shadow value of the RKM result of iris data set 
msviris <- msv(mrwdist, rkm$medoid, rkm$cluster, 
                     title = "MSV plot of Iris data set via RKM")

## -----------------------------------------------------------------------------
#Medoid-based shadow values of objects 49 to 52
msviris$result[c(49:52),]

## ---- fig.width=7, fig.asp=0.8------------------------------------------------
msviris$plot

## -----------------------------------------------------------------------------
#The RKM function for an argument input
rkmfunc <- function(x, nclust) {
  res <- rankkmed(x, nclust, m = 10, iterate = 50)
  return(res$cluster)
}

## -----------------------------------------------------------------------------
#The RKM algorthim evaluation by inputing the rkmfunc function
#in the algorithm argument
rkmbootstrap <- clustboot(mrwdist, nclust=3, nboot=50, algorithm = rkmfunc)

## -----------------------------------------------------------------------------
rkmbootstrap[1:4,1:5]

## -----------------------------------------------------------------------------
#The ward function to order the objects in the consensus matrix
wardorder <- function(x, nclust) {
  res <- hclust(as.dist(x), method = "ward.D2")
  member <- cutree(res, nclust)
  return(member)
}
consensusrkm <- consensusmatrix(rkmbootstrap, nclust = 3, wardorder)

## -----------------------------------------------------------------------------
consensusrkm[c(1:4),c(1:4)]

## ---- fig.width=7, fig.asp=0.8------------------------------------------------
clustheatmap(consensusrkm, "Iris data evaluated by the RKM, ordered by Ward linkage")

## ---- fig.width=7, fig.asp=0.8------------------------------------------------
#convert the data set into principle component object
pcadat <- prcomp(iris[,1:4], scale. = TRUE)
#plot the pca with the corresponding RKM clustering result 
pcabiplot(pcadat, colobj = rkm$cluster, o.size = 2)

## ---- fig.height=3, fig.width=7-----------------------------------------------
pcabiplot(pcadat, y = "PC3",colobj = rkm$cluster, o.size = 1.5)

## ---- fig.width=7, fig.asp=0.8------------------------------------------------
barplotnum(iris[,1:4], rkm$cluster, alpha = 0.05)

## ---- fig.width=7, fig.asp=0.8------------------------------------------------
barplotnum(iris[,1:4], rkm$cluster, nc = 2, alpha = 0.01)


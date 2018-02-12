#' A distance for mixed variables.
#'
#' @description This function computes and returns the distance matrix computed by using
#' the specified distance measure to compute the mixed variable data.
#'
#' @param data A data frame or a matrix object.
#' @param method A  distance for mixed variables: "gower", "wishart", "podani",
#' "huang", and "harikumar".
#' @param idnum A vector of index of numerical variables.
#' @param idbin A vector of index of binary variables.
#' @param idcat A vector of index of categorical variables.
#'
#' @details This is a function to compute distance of mixed variable data.
#' It returns a matrix of all object distances.
#' The available distance are Gower ("gower"), Wishart ("wishart"), Podani ("podani"),
#' Huang ("huang"), and Harikumar-PV ("harikumar"). Because it computes distance of mixed
#' variable data, at least two different class of variables in idnum, idbin, or idcat
#' must be supplied, such as numerical and binary or binary and categorical indices.
#'
#' @author Weksi Budiaji
#'
#' @references Gower, J., 1971. A general coefficient of similarity and some of its properties.
#' Biometrics 27, 857-871
#' @references Harikumar, S., PV, S., 2015. K-medoid clustering for heterogeneous data sets.
#' JProcedia Computer Science 70, 226-237.
#' @references Huang, Z., 1997. Clustering large data sets with mixed numeric and categorical
#' values, in: The First Pacific-Asia Conference on Knowledge Discovery and
#' Data Mining, pp. 21-34.
#' @references Podani, J., 1999. Extending gower's general coefficient of similarity to ordinal
#' characters. Taxon 48, 331-340.
#' @references Wishart, D., 2003. K-means clustering with outlier detection, mixed variables
#' and missing values, in: Exploratory Data Analysis in Empirical Research:
#' Proceedings of the 25th Annual Conference of the Gesellschaft fur Klassifikation e.V.,
#' University of Munich, March 14-16, 2001, Springer Berlin Heidelberg, Berlin,
#' Heidelberg. pp. 216-226.
#'
#' @importFrom stats dist
#' @importFrom stats sd
#'
#' @examples
#' set.seed(1)
#' a <- matrix(sample(1:2, 7*3, replace = TRUE), 7, 3)
#' a1 <- matrix(sample(1:3, 7*3, replace = TRUE), 7, 3)
#' mixdata <- cbind(iris[1:7,1:3], a, a1)
#' colnames(mixdata) <- c(paste(c("num"), 1:3, sep = ""),
#'                        paste(c("bin"), 1:3, sep = ""),
#'                        paste(c("cat"), 1:3, sep = ""))
#' distmix(mixdata, method = "gower", idnum = 1:3, idbin = 4:6, idcat = 7:9)
#'
#' @export
distmix <- function(data, method = "gower", idnum = NULL, idbin = NULL, idcat = NULL) {

  if(any(is.na(data))) stop("Cannot handle missing values!")

  if((is.matrix(data)||is.data.frame(data))==FALSE)
    stop("The data must be a matrix or a data frame object!")

  if(is.null(idnum)&&is.null(idbin)&&is.null(idcat))
    stop("There is no distance computation, specify the numerical, binary, categorical variables!")

  if(is.null(idbin)&&is.null(idcat)||is.null(idnum)&&is.null(idcat)||is.null(idnum)&&is.null(idbin))
    stop("There is no mixed variables!")

  dist_num4 <- c("gower", "wishart", "podani","huang", "harikumar")
  method <- match.arg(method, dist_num4)

  if(is.null(idnum)) {
    num <- 0
    msd <- 0
    dist_numeric <- 0
  } else {
    num <- length(idnum)
    msd <- mean(apply(data[, idnum, drop = FALSE], 2, sd))
    x <- as.matrix(data[,idnum, drop=FALSE])
    dist_numeric <- switch(method,
        gower = distNumeric(x, x, method = "mrw"),
        wishart = distNumeric(x, x, method = "sev"),
        podani = distNumeric(x, x, method = "ser"),
        huang = distNumeric(x, x, method = "se"),
        harikumar = as.matrix(dist(x, method = "manhattan")))
  }

  if(is.null(idbin)) {
    bin <- 0
    dist_binary <- 0
  } else {
    bin <- length(idbin)
    dist_matchbin <- matching(data[,idbin, drop=FALSE], data[,idbin, drop=FALSE])
    if (method == "harikumar") {
      dist_binary <- dist_matchbin*bin
    } else {
      dist_binary <- dist_matchbin
    }
  }

  if(is.null(idcat)) {
    cat <- 0
    dist_cat <- 0
  } else {
    cat <- length(idcat)
    dist_matchcat <- matching(data[,idcat, drop=FALSE], data[,idcat, drop=FALSE])
    if (method == "harikumar") {
      dist_cat <- coocurance(data[,idcat, drop=FALSE])
    } else {
      dist_cat <- dist_matchcat
    }
  }

  nvar <- num + bin + cat
  dist_mix <- switch(method,
      gower = dist_numeric*1/nvar + dist_binary*bin/nvar + dist_cat*cat/nvar,
      wishart = dist_numeric*1/nvar + dist_binary*bin/nvar + dist_cat*cat/nvar,
      podani = (dist_numeric + dist_binary*bin + dist_cat*cat)^0.5,
      huang = dist_numeric + dist_binary*msd + dist_cat*msd,
      harikumar = dist_numeric + dist_binary + dist_cat)

  return(dist_mix)

}


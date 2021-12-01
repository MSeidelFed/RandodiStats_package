#' A Random distributions Function
#'
#' This function allows you to infer the distribution shapes of response variables as a two column matrix with the kurtosis and square of skeweness.
#' @param Distribution_test_mat Defaults to a matrix built with distribution_test_mat().
#' @keywords distributions shapes
#' @export
#' @examples
#' test_mat_distribution_shapes <- Variables2Shapes()

Variables2Shapes <- function(Distribution_test_mat = RandoDiStats::distribution_test_mat(class_method = "discovery")) {

  ### funtion to Transform a list into a data frame (https://www.rdocumentation.org/packages/qdapTools/versions/1.3.3/topics/list2df)

  list2df <- function(x) {
    MAX.LEN <- max(sapply(x, length), na.rm = TRUE)
    DF <- data.frame(lapply(x, function(x) c(x, rep(NA, MAX.LEN - length(x)))))
    colnames(DF) <- paste("V", seq(ncol(DF)), sep = "")
    DF
  }

  ### main

  if (class(Distribution_test_mat)[1] == "matrix") {

    SKEW <- vector(mode = "list", length = dim(Distribution_test_mat)[2])

    KURT <- vector(mode = "list", length = dim(Distribution_test_mat)[2])

    for (i in 1:dim(Distribution_test_mat)[2]) {

      SKEW[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i])), graph = F)$skewness

      KURT[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i])), graph = F)$kurtosis

    }

    average_distribution_plot <- t(rbind("square of skewness" = list2df(SKEW)^2,
                                         kurtosis = list2df(KURT)))


  } else if (class(Distribution_test_mat)[1] == "list") {

    SKEW <- vector(mode = "list", length = length(Distribution_test_mat))

    KURT <- vector(mode = "list", length = length(Distribution_test_mat))

    for (i in 1:length(Distribution_test_mat)) {

      SKEW[[i]]  <- fitdistrplus::descdist(unlist(Distribution_test_mat[i]), graph = F)$skewness

      KURT[[i]]  <- fitdistrplus::descdist(unlist(Distribution_test_mat[i]), graph = F)$kurtosis

    }

    average_distribution_plot <- t(rbind("square of skewness" = list2df(SKEW)^2,
                                         kurtosis = list2df(KURT)))

  }

  return(average_distribution_plot)
}

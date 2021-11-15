#' A Random distributions Function
#'
#' This function allows you to test the distributions of response variables and output per response variable the nearest GLM family. The assumption is that the mean kurtosis and square of skewness represent the average of the distributions.
#' @param Distribution_test_mat Defaults to a matrix built with distribution_test_mat().
#' @keywords distributions test
#' @export
#' @examples
#' test_mat_distributions <- testing_distributions()

testing_distributions <- function(Distribution_test_mat = distribution_test_mat()) {

  protein_coords <- as.data.frame(x = Variables2Shapes(Distribution_test_mat),
                                  row.names = colnames(Distribution_test_mat))

  distribution_mat <- Variables2Shapes(distribution_test_mat())

  ### this object must be the kurtosis and square of skewness of the distributions

  distribution_coords <- rbind(colMeans(distribution_mat[1:1000,]),
                               colMeans(distribution_mat[1001:2000,]),
                               colMeans(distribution_mat[2001:3000,]),
                               colMeans(distribution_mat[3001:4000,]),
                               colMeans(distribution_mat[4001:5000,]),
                               colMeans(distribution_mat[5001:6000,]),
                               colMeans(distribution_mat[6001:7000,]))

  dist_mat <- as.matrix(raster::pointDistance(p1 = protein_coords,
                                              p2 = distribution_coords, lonlat = F, allpairs = T))

  Family_selection_GLM_R <- cbind(
    Family = c("gamma","logis","beta","normal","binomial","poisson","exponential"),
    Link = c("Inverse","Logit","Logit","Identity","Logit","Log","Inverse"),
    GLM_R = c("Gamma","quasibinomial","quasibinomial","gaussian","quasibinomial","quasipoisson","Gamma"))

  runner <- c()

  for (i in 1:dim(dist_mat)[1]) {

    test <- Family_selection_GLM_R[, "GLM_R"][which(dist_mat[i,] == min(dist_mat[i,]))]
    
    if (length(test) > 0) {
      
      runner[i] <- test
      
    }
  }

  return(runner)

}

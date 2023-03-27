#' A Random distributions Function
#'
#' This function allows you to test the distributions of response variables and output per response variable the nearest GLM family. The assumption is that the mean kurtosis and square of skewness represent the average of the distributions.
#' @param Distribution_test_mat Defaults to a matrix built with distribution_test_mat().
#' @keywords distributions test
#' @export
#' @examples
#' test_mat_distributions <- testing_distributions()


testing_distributions <- function(Distribution_test_mat = distribution_test_mat()) {
  
  # Calls Variables2shapes with the input matrix. Gives back for each feature the values for skweness^2 and kurtosis. 
  protein_coords <- as.data.frame(x = Variables2Shapes(Distribution_test_mat),
                                  row.names = colnames(Distribution_test_mat))
  
  # This calls Variables2Shapes with the distribution_test_mat() as an input. Distribution_test_mat gives back a matrix
  # with a thousand random values for square skewness and kurtosis of the 7 supported exponential distributions in R 
  distribution_mat <- Variables2Shapes(distribution_test_mat())
  
  ### this object must be the kurtosis and square of skewness of the distributions
  
  # Takes the means of the thousand values for square skewness and kurtosis of each of the 7 distributions
  distribution_coords <- rbind(colMeans(distribution_mat[1:1000,]),
                               colMeans(distribution_mat[1001:2000,]),
                               colMeans(distribution_mat[2001:3000,]),
                               colMeans(distribution_mat[3001:4000,]),
                               colMeans(distribution_mat[4001:5000,]))
  
  # Creates a distance matrix between the values for protein_coords and the random values from distribution_coords. 
  dist_mat <- as.matrix(raster::pointDistance(p1 = protein_coords,
                                              p2 = distribution_coords, lonlat = F, allpairs = T))
  
  Family_selection_GLM_R <- cbind(
    Family = c("gamma","logis","beta","normal","exponential"),
    Link = c("Inverse","Logit","Logit","Identity","Inverse"),
    GLM_R = c("Gamma","quasipoisson","quasibinomial","gaussian","Gamma"))
  
  runner <- c()
  
  # Here we loop over the rows of the distance and the smallest value for each row (smallest euclidean distance) is
  # selected. This value determines the chosen distribution.
  
  Family = c("gamma","logis","beta","normal","exponential")
  for (i in 1:dim(dist_mat)[1]) {

    test <- Family_selection_GLM_R[, "Family"][which(dist_mat[i,] == min(dist_mat[i,]))]
    equal = distribution_verification(feature = Distribution_test_mat[,i], regfamily = test)
    # assign the appropriate glm input to the test
    if (equal) {
      print(Family_selection_GLM_R[, "Family"][which(dist_mat[i,] == min(dist_mat[i,]))])
      test = Family_selection_GLM_R[, "GLM_R"][which(Family_selection_GLM_R[,"Family"] == test)]
    }
    # if the selected distributions differ, test will be non-parametric. 
    if (!equal) {
      test = "non-parametric"
    }
    
    if (length(test) > 0) {
      runner[i] <- test
    }
  }
  return(runner)
  
}


#' A Random distributions Function
#'
#' This function allows you to generate a matrix with n randomly-drawn specific distribution shapes that follow GLM supported regression families.
#' @param nrow_x Defaults to 1000.
#' @param n_random_distributions Defaults to 1000.
#' @keywords distributions
#' @export
#' @examples
#' test_mat <- distribution_test_mat()

distribution_test_mat <- function(nrow_x = 1000,
                                  n_random_distributions = 1000,
                                  class_method = "comparison") {

  if (class_method == "comparison") {

    ### vectors of gamma distribution
    test1 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- rgamma(n = nrow_x,shape = 0.1,rate = 10)
      test1[,i] <- x
    }
    ### vectors of logis distribution
    test2 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- rlogis(n = nrow_x, location = 0.1, scale = 0.01)
      test2[,i] <- x
    }
    ### vectors of beta distribution
    test3 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- rbeta(n = nrow_x, shape1 = 2, shape2 = 1, ncp = 5)
      test3[,i] <- x
    }
    ### vectors of normal distribution
    test4 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- rnorm(n = nrow_x, mean = 0.5, sd = 0.1)
      test4[,i] <- x
    }
    ### vectors of binomial distribution
    test5 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- rbinom(n = nrow_x, size = 1, prob = 0.5)
      test5[,i] <- x
    }
    ### vectors of poisson distribution
    test6 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- rpois(n = nrow_x, lambda = 0.01)
      test6[,i] <- x
    }
    ### vectors of exponential distribution
    test7 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- rexp(n = nrow_x, rate = 20)
      test7[,i] <- x
    }
    return(cbind(test1, test2, test3, test4, test5, test6, test7))

  } else if (class_method == "discovery") {

    ### vectors of gamma distribution
    test1 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- round(rgamma(n = nrow_x,shape = 2,rate = 0.2),1)
      test1[,i] <- x
    }
    ### vectors of logis distribution
    test2 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- round(rlogis(n = nrow_x, location = 2,scale = 0.2),1)
      test2[,i] <- x
    }
    ### vectors of beta distribution
    test3 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- round(rbeta(n = nrow_x, shape1 = 2, shape2 = 1, ncp = 5),1)
      test3[,i] <- x
    }
    ### vectors of normal distribution
    test4 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- round(rnorm(n = nrow_x, mean = 0, sd = 1),1)
      test4[,i] <- x
    }
    ### vectors of binomial distribution
    test5 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- rbinom(n = nrow_x, size = 10, prob = 0.5)
      test5[,i] <- x
    }
    ### vectors of poisson distribution
    test6 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- rpois(n = nrow_x, lambda = 1)
      test6[,i] <- x
    }
    ### vectors of exponential distribution
    test7 <- matrix(data = NA, nrow = nrow_x, ncol = n_random_distributions)
    for (i in 1:n_random_distributions) {
      x <- round(rexp(n = nrow_x), 1)
      test7[,i] <- x
    }
    return(cbind(test1, test2, test3, test4, test5, test6, test7))
    }

}

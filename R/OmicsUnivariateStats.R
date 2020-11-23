#' A Random distributions Function
#'
#' This function allows you to test response variables using a generalized linear model with one or two factors and multiple levels per factor, i.e., multiple regressors, customizing the family of regression for each test according to the response variable distribution.
#' @param class_comparison_mat Defaults to distribution_test_mat().
#' @param Factor1. Needs to be defined
#' @param Factor2 defaults to NULL.
#' @param Contrast defaults to TRUE.
#' @keywords Univariate Statistics
#' @export
#' @examples
#' Factor1_eg <- as.factor(c(rep("RED", 200), rep("GREEN", 200), rep("BLACK", 200),rep("WHITE", 200), rep("YELLOW", 200)))
#' test_OUS <- OmicsUnivariateStats(Factor1 = Factor1_eg)
#' ...

OmicsUnivariateStats <- function(class_comparison_mat = abs(distribution_test_mat()),
                                 Factor1, Factor2 = NULL, Contrast = T) {


  if (dim(class_comparison_mat)[1] == dim(distribution_test_mat())[1] &
      dim(class_comparison_mat)[2] == dim(distribution_test_mat())[2]) {

    class_comparison_mat[which(class_comparison_mat == 0)] <- 0.0001

  }
  
  regfamily <- testing_distributions(class_comparison_mat)

  ## defining the formula for the models

  if (length(Factor2) > 1) {
    
    FactorNo <- 2

    if (Contrast == T) { Formula = class_comparison_mat[,i] ~ Factor1 * Factor2}
    else {  Formula = class_comparison_mat[,i] ~ Factor1 + Factor2 }

    Levene_factor = paste(Factor1, Factor2, sep = "_")

  } else {
    
    FactorNo <- 1

    Formula = class_comparison_mat[,i] ~ Factor1
    Levene_factor = Factor1

  }
  
  print(Formula)
  
  #print(dim(class_comparison_mat))
  
  ## Levene´s test produces errors when there is no variance between treatments, 
  ## here we remove features with no sd in individual treatments
  
  mean_treatment_sd <- c()
  
  for (i in 1:dim(class_comparison_mat)[2]) {
    
    mean_treatment_sd[i] <- mean(aggregate(Formula, FUN = sd)[,(FactorNo + 1)])
    
    #print(mean_treatment_sd[i])
  }
  
  null_sd_features <- colnames(class_comparison_mat[,which(mean_treatment_sd == 0)])
  
  #print(null_sd_features)
  
  if (length(null_sd_features) > 0) {
    
    cat("Features with NULL factor variance : ", null_sd_features)
    cat("...", "\n")
    
    class_comparison_mat = as.matrix(class_comparison_mat[,-c(which(mean_treatment_sd == 0))])
    
    #print(dim(class_comparison_mat))
  }
  
  ### removing features with global null standard deviation
  
  test_global_sd <- which(rowSds(as.matrix(class_comparison_mat)) == 0)
  
  if (length(test_global_sd) > 0) {
    
    class_comparison_mat = as.matrix(class_comparison_mat[,-c(test_global_sd)])
    
    #print(dim(class_comparison_mat))
    
  }

  
  ## for loop for tests

  ### empty mat to store results

  i = 1

  test_out <- matrix(NA,
                     nrow = dim(class_comparison_mat)[2],
                     ncol = length(c(glm(Formula)[["coefficients"]],
                                     "Homoscedastic & Parametric")))

  count = 0

  for (i in 1:dim(class_comparison_mat)[2]) {

    count = count + 1


    print(c(regfamily[i], "Column Number", count))

    if (levene.test(y = class_comparison_mat[,i],
                    group = Levene_factor,
                    location = "median",
                    bootstrap = F)[["p.value"]] > 0.05 &
        shapiro.test(x = class_comparison_mat[,i])[["p.value"]] > 0.05) {


        test <- c(summary(lm(Formula))$coef[1:dim(summary(glm(Formula,
                                                              gaussian))$coef)[1],
                                            "Pr(>|t|)"],
                  "Homoscedastic & Parametric")


    } else if (levene.test(y = class_comparison_mat[,i],
                             group = Levene_factor,
                             location = "median",
                             bootstrap = F)[["p.value"]] < 0.05 &
                 shapiro.test(x = class_comparison_mat[,i])[["p.value"]] > 0.05) {

          test <- c(summary(lm(Formula))$coef[1:dim(summary(glm(Formula,
                                                    gaussian))$coef)[1],
                                              "Pr(>|t|)"],
                    "Heteroscedastic & Parametric")


    } else if (levene.test(y = class_comparison_mat[,i],
                              group = Levene_factor,
                              location = "median",
                              bootstrap = F)[["p.value"]] > 0.05 &
                  shapiro.test(x = class_comparison_mat[,i])[["p.value"]] < 0.05) {

          test <- c(summary(glm(Formula, regfamily[i]))$coef[1:dim(summary(glm(Formula,
                                                                        regfamily[i]))$coef)[1],
                                                      "Pr(>|t|)"],
                    "Homoscedastic & Non-parametric")


    } else if (levene.test(y = class_comparison_mat[,i],
                              group = Levene_factor,
                              location = "median",
                              bootstrap = F)[["p.value"]] < 0.05 &
                  shapiro.test(x = class_comparison_mat[,i])[["p.value"]] < 0.05) {

          test <- c(summary(glm(Formula, regfamily[i]))$coef[1:dim(summary(glm(Formula,
                                                                        regfamily[i]))$coef)[1],
                                                      "Pr(>|t|)"],
                    "Heteroscedastic & Non-parametric")

    }

    test_out[i,] <- test

    colnames(test_out) <- c(names(glm(Formula,
                                      gaussian)[["coefficients"]]),
                            "Assumptions_tested")

  }

  rownames(test_out) <- colnames(class_comparison_mat)


  ### FDR adjustment

  FDR_adjustment <- matrix(NA, nrow = dim(test_out)[1], ncol = (dim(test_out)[2]-1))

  for (j in 1:(dim(test_out)[2]-1)) {

    runner <- p.adjust(test_out[,j], method = "fdr")

    FDR_adjustment[,j] <- runner

  }


  adjusted_test_out <- cbind(test_out,
                             FDR_adjustment)

  colnames(adjusted_test_out) <-c(paste(colnames(test_out)[-length(colnames(test_out))],
                                        "P values",
                                        sep = "_"),
                                  "Assumptions_tested",
                                  paste(colnames(test_out)[-length(colnames(test_out))],
                                        "Q values",
                                        sep = "_"))

  return(adjusted_test_out)

}

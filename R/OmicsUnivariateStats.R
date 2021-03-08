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
                                 Factor1,
                                 Factor2 = NULL,
                                 Contrast = T,
                                 ReturnTukeyPlots = T) {
  
  ## functions needed
  
  show_condition <- function(code) {
    tryCatch(code,
             error = function(c) "error",
             message = function(c) "message"
    )
  }
  
  ## main


  if (dim(class_comparison_mat)[1] == dim(distribution_test_mat())[1] &
      dim(class_comparison_mat)[2] == dim(distribution_test_mat())[2]) {

    class_comparison_mat[which(class_comparison_mat == 0)] <- 0.0001

  }

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
  
  ## removing all kind of error prone features from the input matrices
  
  #print(dim(class_comparison_mat))
  
  ## remove rows full of NAs
  
  indexes_NAs <- as.numeric(which(is.na(colMeans(class_comparison_mat, na.rm = T))))
  
  if (length(indexes_NAs) > 0) {
    
    cat("Features with only NAs: ", "\n", colnames(class_comparison_mat[,indexes_NAs]))
    cat("...", "\n")
    
    class_comparison_mat = as.matrix(class_comparison_mat[,-c(indexes_NAs)])
    
    #print(dim(class_comparison_mat))
  }
  
  ## Levene´s test produces errors when there is no variance between treatments, 
  ## here we remove features with no sd in individual treatments
  
  mean_treatment_sd <- c()
  
  NA_remover <- c()
  
  for (i in 1:dim(class_comparison_mat)[2]) {
    
    mean_treatment_sd[i] <- mean(aggregate(Formula, FUN = sd)[,(FactorNo + 1)])
    
    ## test 1 gets factors with no replication out of the question
    
    test1NA <- any(is.na(aggregate(Formula, FUN = mean)[,(FactorNo + 1)]))
    
    ## test 2 is necessary because when only few factors have the reps the no NAs will appear in the final object
      
    test2NA <- dim(aggregate(Formula, FUN = mean))[1] != length(levels(as.factor(paste0(Factor1 ,Factor2))))
    
    if (test1NA | test2NA) {
      
      NA_remover <- c(NA_remover, i)
      
    }
    
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
  
  ### removing features with not enough replication due to NAs
  
  NA_features <- colnames(class_comparison_mat[,NA_remover])
  
  #print(NA_features)
  
  if (length(NA_features) > 0) {
    
    cat("Features without replication due to NAs : ", "\n", NA_features)
    cat("...", "\n")
    
    class_comparison_mat = as.matrix(class_comparison_mat[,-c(NA_remover)])
    
    #print(dim(class_comparison_mat))
  }
  
  
  ### removing features with global null standard deviation
  
  test_global_sd <- which(rowSds(as.matrix(class_comparison_mat)) == 0)
  
  if (length(test_global_sd) > 0) {
    
    class_comparison_mat = as.matrix(class_comparison_mat[,-c(test_global_sd)])
    
    #print(dim(class_comparison_mat))
    
  }
  
  regfamily <- testing_distributions(Distribution_test_mat = class_comparison_mat)
  
  ## for loop for tests

  ### empty mat to store results

  i = 1

  test_out <- matrix(NA,
                     nrow = dim(class_comparison_mat)[2],
                     ncol = length(c(glm(Formula)[["coefficients"]],
                                     "Homoscedastic & Parametric")))
  
  TukeyHSD_info <- matrix(NA,
                          nrow = dim(class_comparison_mat)[2],
                          ncol = length(levels(Factor1))*length(levels(Factor2)))

  count = 0
  
  if(ReturnTukeyPlots == T) {
    
    pdf("TukeyHSD_Plots.pdf")
    
    par(mar=c(7,10,2,2))
    
    par(mfrow = c(2,1))
    
  }
  
  for (i in 1:dim(class_comparison_mat)[2]) {

    count = count + 1
    
    ## TukeyPlots
    
    if(ReturnTukeyPlots == T) {
      
      return_letters <- TukeyCustomized(variable = class_comparison_mat[,i],
                                        factor = as.factor(Levene_factor),
                                        MainTitle = colnames(class_comparison_mat)[i])
      
      TukeyHSD_info[i,] <- t(return_letters)[1,]
      
    }
    
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
      
      if(show_condition(glm(Formula, regfamily[i])) == "error") {
        
        No_Regressors <- (length(levels(Factor1))-1)*(length(levels(Factor2))-1)
        
        test <- c(summary(glm(Formula, regfamily[i],
                      start = c(1, rep(0, No_Regressors-1))))$coef[1:dim(summary(glm(Formula,
                                                                                    regfamily[i],
                                                                                    start = c(1, rep(0, No_Regressors-1))))$coef)[1],
                                                  "Pr(>|t|)"],"Heteroscedastic & Parametric")
        
      } else {
        
        test <- c(summary(glm(Formula, regfamily[i]))$coef[1:dim(summary(glm(Formula,
                                                                             regfamily[i]))$coef)[1],
                                                           "Pr(>|t|)"],
                  "Homoscedastic & Non-parametric")
        
      }

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
  
  if(ReturnTukeyPlots == T) {
    
    dev.off()
    
  }

  ### FDR adjustment

  FDR_adjustment <- matrix(NA, nrow = dim(test_out)[1], ncol = (dim(test_out)[2]-1))

  for (j in 1:(dim(test_out)[2]-1)) {

    runner <- p.adjust(test_out[,j], method = "fdr")

    FDR_adjustment[,j] <- runner

  }

  
  if(ReturnTukeyPlots == T) {
    
    adjusted_test_out <- cbind(test_out,
                               regfamily,
                               FDR_adjustment,
                               TukeyHSD_info)
    
    colnames(adjusted_test_out) <-c(paste(colnames(test_out)[-length(colnames(test_out))],
                                          "P values",
                                          sep = "_"),
                                    "Assumptions_tested",
                                    "Regression_family",
                                    paste(colnames(test_out)[-length(colnames(test_out))],
                                          "Q values",
                                          sep = "_"),
                                    t(return_letters)[2,])
    
    
    
  } else {
    
    adjusted_test_out <- cbind(test_out,
                               regfamily,
                               FDR_adjustment)
    
    colnames(adjusted_test_out) <-c(paste(colnames(test_out)[-length(colnames(test_out))],
                                          "P values",
                                          sep = "_"),
                                    "Assumptions_tested",
                                    "Regression_family",
                                    paste(colnames(test_out)[-length(colnames(test_out))],
                                          "Q values",
                                          sep = "_"))
    
  }

  

  return(adjusted_test_out)

}

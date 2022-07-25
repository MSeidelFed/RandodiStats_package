#' A Random distributions Function to perform univariate statistical inference
#'
#' This function allows you to test response variables using a generalized linear model with one or two factors and multiple levels per factor, i.e., multiple regressors, customizing the family of regression for each test according to the response variable distribution.
#' @param class_comparison_mat Defaults to distribution_test_mat().
#' @param Factor1. Needs to be defined
#' @param Factor2 defaults to NULL.
#' @param Contrast defaults to TRUE.
#' @param TukeyReturns Tukey HSD can return either "MeanComparisons" or "Letters".
#' @param ReturnTukeyPlots defaults to TRUE, returns a PDF with the plots.
#' @param TukeyPDFName defaults to "test".
#' @param marginsTukey margins of the Tukey HSD plots
#' @param returnObject returns either the results from the "OmicsTests" or the cropped tested "class_comparison_mat"
#' @keywords Univariate Statistics
#' @export
#' @examples
#' Factor1_eg <- as.factor(c(rep("RED", 200), rep("GREEN", 200), rep("BLACK", 200),rep("WHITE", 200), rep("YELLOW", 200)))
#' test_OUS <- OmicsUnivariateStats(Factor1 = Factor1_eg)
#' ...

OmicsUnivariateStats <- function(class_comparison_mat = abs(RandoDiStats::distribution_test_mat()),
                                 Factor1,
                                 Factor2 = NULL,
                                 Contrast = F,
                                 TukeyReturns = c("MeanComparisons", "Letters"),
                                 ReturnTukeyPlots = T,
                                 TukeyPDFName = "test",
                                 marginsTukey = c(6,12,3,3),
                                 returnObject = c("OmicsTests", "class_comparison_mat")) {
  
  ## functions needed
  
  show_condition <- function(code) {
    tryCatch(code,
             error = function(c) "error",
             message = function(c) "message"
    )
  }
  
  ## main
  
  #What is this exactly used for? Only execute if input matrix is distribution_test_mat?
  if (dim(class_comparison_mat)[1] == dim(RandoDiStats::distribution_test_mat())[1] &
      dim(class_comparison_mat)[2] == dim(RandoDiStats::distribution_test_mat())[2]) {
    
    class_comparison_mat[which(class_comparison_mat == 0)] <- 0.0001
    
  }
  
  ## defining the formula for the models
  
  if (length(Factor2) > 1) {
    
    FactorNo <- 2
    
    if (Contrast == T) {Formula = class_comparison_mat[,i] ~ Factor1 * Factor2} else {  
      
      Formula = class_comparison_mat[,i] ~ Factor1 + Factor2 
      
      }
    
      Levene_factor = as.factor(paste(Factor1, Factor2, sep = "_"))
    
  } else {
    
    FactorNo <- 1
    
    # This is an object of type formula
    Formula = class_comparison_mat[,i] ~ Factor1
    # Levene factor in our case length 18, 6 levels
    Levene_factor = as.factor(Factor1)
    
  }
  
  print(Formula)
  
  ## removing all kind of error prone features from the input matrices
  
  #### unity-based normalization
  
  mat <- class_comparison_mat
  
  # Here each matrix value undergoes feature scaling (column wise). Values will end up being between 0 and 1.(Binomial and Quasibinomial) 
  # NAs are preserved 
  mat_Ubased_norm <- (mat - rep(matrixStats::colMins(mat, na.rm = T),
                                rep.int(nrow(mat),
                                        ncol(mat)))) / (rep(matrixStats::colMaxs(mat, na.rm = T),
                                                            rep.int(nrow(mat),
                                                                    ncol(mat))) - rep(matrixStats::colMins(mat, na.rm = T),
                                                                                      rep.int(nrow(mat),
                                                                                              ncol(mat))))
  
  #### zero replacement by small values
  
  # Here, zeros are replaced with very small values drawn from a normal distribution with given mean and standard deviation
  # to mimic noise, as true metabolite levels of 0 do not occur in practice. Is the magnitude of noise always positive?
  mat_Ubased_norm[which(mat_Ubased_norm == 0)] <- abs(rnorm(n = length(which(mat_Ubased_norm == 0)),
                                                            mean = 0.0000000000001, sd = 0.0000000001))
  
  
  class_comparison_mat <- mat_Ubased_norm
  
  
  ## remove rows full of NAs
  
  # Here, all columns (variables) that contain only NA's are identified and then removed. 
  indexes_NAs <- as.numeric(which(is.na(colMeans(class_comparison_mat, na.rm = T))))
  
  if (length(indexes_NAs) > 0) {
    
    cat("Features with only NAs: ", "\n", colnames(class_comparison_mat)[indexes_NAs])
    cat("...", "\n")
    
    class_comparison_mat = as.matrix(class_comparison_mat[,-c(indexes_NAs)])
  }
  
  ## Levene´s test produces errors when there is no variance between treatments, 
  ## here we remove features with no sd in individual treatments
  ## aggregate by rep number to ensure enough reps for SD and full coverage among treatments
  
  mean_treatment_sd <- c()
  min_treatment_sd <- c()
  NA_remover <- c()
  
  # Loop over columns ...
  for (i in 1:dim(class_comparison_mat)[2]) {
    # mean of standard deviations for each treatment in column i. 
    mean_treatment_sd[i] <- mean(aggregate(Formula, FUN = sd)[,(FactorNo + 1)])
    
    # Number of treatments for each level in column i
    treatments <- aggregate(Formula, FUN = length)[, 
                                                   (FactorNo + 1)]
    # Number of treatments in column i
    treatment_length <- length(treatments)
    
    # Takes the min number of treatments among all the levels in column i
    min_treatment_sd[i] <- min(treatments)
    
    # Treatment number smaller 3 -> remove column 
    if (min_treatment_sd[i] < 3 | treatment_length < length(levels(Levene_factor))) {
      NA_remover <- c(NA_remover, i)
    }
    
  }
  
  ### removing features with not enough replication due to NAs
  
  NA_features <- colnames(class_comparison_mat[,NA_remover])
  
  if (length(NA_features) > 0) {
    
    cat("Features without replication due to NAs : ", "\n", NA_features)
    cat("...", "\n")
    
    class_comparison_mat = as.matrix(class_comparison_mat[,-c(NA_remover)])
  }
  
  #### null variance in treatments
  
  # for mean_treatment_sd to be 0, the variance for all treatments must be 0
  null_sd_features <- colnames(class_comparison_mat[,which(mean_treatment_sd == 0)])
  
  if (length(null_sd_features) > 0) {
    
    cat("Features with NULL factor variance : ", null_sd_features)
    cat("...", "\n")
    
    class_comparison_mat = as.matrix(class_comparison_mat[,-c(which(mean_treatment_sd == 0))])
    
  }
  
  ### removing features with global null standard deviation
  
  # Here SD of rows (a particular treatment) are taken. Shouldn't it be SD of the columns (mass features)?
  test_global_sd <- which(matrixStats::colSds(as.matrix(class_comparison_mat)) == 0)
  
  if (length(test_global_sd) > 0) {
    
    class_comparison_mat = as.matrix(class_comparison_mat[,-c(test_global_sd)])
    
    #print(dim(class_comparison_mat))
    
  }
  
  ## defining regression families
  
  regfamily <- c()
  
  # calling testing_distributions with input matrix, gives back a distribution for all columns (features)
  regfamily <- RandoDiStats::testing_distributions(Distribution_test_mat = class_comparison_mat)
  
  ### while loop to prevent empty GLM family error
  
  # same as above, is this because testing_distributions sometimes does not give back a distribution? 
  while (length(regfamily) == 0) {
  
    regfamily <- RandoDiStats::testing_distributions(Distribution_test_mat = class_comparison_mat)
  
  }
  
  ## for loop for tests
  
  
  
  i = 1
  
  # empty mat to store results of the analysis
  test_out <- matrix(NA,
                     nrow = dim(class_comparison_mat)[2],
                     ncol = length(c(glm(Formula)[["coefficients"]],
                                     "Homoscedastic & Parametric")))
  
  if(TukeyReturns == "Letters") {
    
    TukeyHSD_info <- matrix(NA,
                            nrow = dim(class_comparison_mat)[2],
                            ncol = length(levels(Levene_factor)))
    
  } else if (TukeyReturns == "MeanComparisons"){
    
    TukeyHSD_info <- matrix(NA,
                            nrow = dim(class_comparison_mat)[2],
                            ncol = dim(arrangements::combinations(levels(Levene_factor), k = 2))[1])
    
  } else {
    
    stop("ERROR: set valid TukeyReturns in the function call")
    
  }
  
  count = 0
  
  if(ReturnTukeyPlots == T) {
    
    pdf(paste0(TukeyPDFName, "_", "TukeyHSD.pdf"))
    
    par(mar = marginsTukey)
    
    par(mfrow = c(2,1))
    
  }
  
  # loop over columns
  for (i in 1:dim(class_comparison_mat)[2]) {
    
    count = count + 1
    
    ## TukeyPlots
    
    if(ReturnTukeyPlots == T) {
      
      # call TukeyCustomized on the i'th column, outputs a plot value~treatment 
      return_letters <- RandoDiStats::TukeyCustomized(variable = class_comparison_mat[,i],
                                                      factor = as.factor(Levene_factor),
                                                      MainTitle = colnames(class_comparison_mat)[i], 
                                                      returnObject = TukeyReturns)
      
      if (TukeyReturns == "Letters") {
        
        TukeyHSD_info[i,] <- t(return_letters)[1,]
        
      } else if (TukeyReturns == "MeanComparisons") {
        
        TukeyHSD_info[i,] <- return_letters
        
      }
      
      
      
    }
    
    print(c(regfamily[i], "Column Number", count))
    
    # Removal of NA's in i'th column for variable and factor
    NA_free_var <- class_comparison_mat[,i][!is.na(class_comparison_mat[,i])]
    
    NA_free_factor <- Levene_factor[!is.na(class_comparison_mat[,i])]
    
    # Here either a lm or a glm is build depending on the results of the Levene's and shapiro test.
    # The intercept and the coefficients are stored in test. The intercept is always the control. 
    # A significant p-value indicates, that the mean of the particular group differs from the mean of the control. 
    
    # Levene and Shapiro test. Here, homoscedastic and normally distributed. 
    if (lawstat::levene.test(y = NA_free_var,
                             group = NA_free_factor,
                             location = "median",
                             bootstrap = F)[["p.value"]] > 0.05 &
        shapiro.test(x = class_comparison_mat[,i])[["p.value"]] > 0.05) {
      
      
      test <- c(summary(lm(Formula))$coef[1:dim(summary(glm(Formula,
                                                            gaussian))$coef)[1],
                                          "Pr(>|t|)"],
                "Homoscedastic & Parametric")
      
    # Levene and Shapiro test. Here, heteroscedastic and normally distributed  
    } else if (lawstat::levene.test(y = NA_free_var,
                                    group = NA_free_factor,
                                    location = "median",
                                    bootstrap = F)[["p.value"]] < 0.05 &
               shapiro.test(x = class_comparison_mat[,i])[["p.value"]] > 0.05) {
      
      test <- c(summary(lm(Formula))$coef[1:dim(summary(glm(Formula,
                                                            gaussian))$coef)[1],
                                          "Pr(>|t|)"],
                "Heteroscedastic & Parametric")
      
    # Levene and Shapiro test. Here, homoscedastic and not normally distributed  
    } else if (lawstat::levene.test(y = NA_free_var,
                                    group = NA_free_factor,
                                    location = "median",
                                    bootstrap = F)[["p.value"]] > 0.05 &
               shapiro.test(x = class_comparison_mat[,i])[["p.value"]] < 0.05) {
      
      test <- c(summary(glm(Formula, regfamily[i]))$coef[1:dim(summary(glm(Formula,
                                                                           regfamily[i]))$coef)[1],
                                                         "Pr(>|t|)"],
                "Homoscedastic & Non-parametric")
      
    # Levene and Shapiro test. Here, heteroscedastic and not normally distributed  
    } else if (lawstat::levene.test(y = NA_free_var,
                                    group = NA_free_factor,
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
    
    if (TukeyReturns == "Letters") {
      
      namesTukey <- t(return_letters)[2,]
      
    } else if (TukeyReturns == "MeanComparisons") {
      
      namesTukey <- names(return_letters)
      
    }
    
    
    
    colnames(adjusted_test_out) <-c(paste(colnames(test_out)[-length(colnames(test_out))],
                                          "P values",
                                          sep = "_"),
                                    "Assumptions_tested",
                                    "Regression_family",
                                    paste(colnames(test_out)[-length(colnames(test_out))],
                                          "Q values",
                                          sep = "_"),
                                    paste0(rep("TukeyHSD", length(namesTukey)), "_", namesTukey))
                                    
                                    
    
    
    
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
  
  if (returnObject == "class_comparison_mat") {
    
    return(class_comparison_mat)
    
  } else if (returnObject == "OmicsTests") {
    
    return(adjusted_test_out)
     
  } else {
    
    stop("ERROR: input a valid return object in the function call")
    
  }

  
}

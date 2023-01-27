library(dunn.test)

Dunn.Test = function(variable,
                    factor,
                    altp = TRUE,
                    method = "bh",
                    MainTitle = "",
                    returnObject = c("Letters", "MeanComparisons")) {
  
  test = dunn.test(variable,factor,altp = altp, method = method)
  test = cbind.data.frame(test$comparisons,test$Z,test$altP.adjusted)
  test = test[order(test$`test$comparisons`),]
  test_out = test$`test$altP.adjusted`
  names(test_out) = test$`test$comparisons`
  return(test_out)
  
}



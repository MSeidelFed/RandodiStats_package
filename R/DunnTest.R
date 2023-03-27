DunnTest = function(variable,
                     factor,
                     method = "BH")  {
  
  data <- cbind.data.frame(value = as.numeric(variable), treatment = as.factor(factor))
  test = rstatix::dunn_test(data = data, formula = value~treatment, p.adjust.method = method)$p.adj
  return(test)
  
}


feature_scaling <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) } 
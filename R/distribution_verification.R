# Function that takes a feature and a distribution and verifies whether the feature is distributed according to the given 
# distribution. Returns TRUE if that is the case, FALSE otherwise

distribution_verification = function(feature,regfamily) {
  
  variance = var(feature)
  std_dev = sd(feature)
  mu = mean(feature)
  feature_len = length(feature)
  

  if (regfamily =="gamma") {
    
    #shape a=(mean^2/variance) and rate b = mean/variance.
    a = (mu^2/variance)
    b = (mu/variance)
    gamm <- rgamma(n = 20, shape = a, rate = b)
    test = ks.test(gamm,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (regfamily == "logis") {
    
    # here location should equal the mean of the distribution. variance = (s^2*pi^2)/3
    location = mu
    scale = sqrt((variance*3)/(pi^2))
    logis <- rlogis(n = 20, location=location, scale = scale)
    test = ks.test(logis,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (regfamily == "normal") {
    
    normal <- rnorm(n = 20, mean = mu, sd = std_dev)
    test = ks.test(normal,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (regfamily == "binomial") {
    
    # mean should be size*prob. how to choose size? As if now, it is the length of the feature.
    # prob = mu/feature_len
    prob = mu
    binom <- rbinom(n = 20, size = 1, prob = prob)
    test = ks.test(binom,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (regfamily == "poisson") {
    
    ### vectors of poisson distribution. lambda = variance and lambda = mean
    lambda = std_dev^2
    pois <- rpois(n = 20, lambda = lambda)
    test = ks.test(pois,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (regfamily == "exponential") {
  
    ### vectors of exponential distribution. mean = standard deviation = 1/rate
    rate = 1/std_dev
    expon = rexp(n = 20, rate = rate)
    test = ks.test(expon,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else {
    
    stop("ERROR: invalid regression family given ")
  }
}




# Function calls for each distribution

gamm = rgamma(n = 20,shape = 0.2,rate = 4)
distribution_verification(feature = gamm, regfamily = "gamma")

logis = rlogis(n = 20, location = 0.1, scale = 0.01)
distribution_verification(feature = logis, regfamily = "logis")

binom = rbinom(n = 20, size = 1, prob = 0.5)
distribution_verification(feature = binom, regfamily = "binomial")

pois <- rpois(n = 20, lambda = 0.1)
distribution_verification(feature = pois, regfamily = "poisson")

expon = rexp(n = 20, rate = 1)
distribution_verification(feature = expon, regfamily = "exponential")




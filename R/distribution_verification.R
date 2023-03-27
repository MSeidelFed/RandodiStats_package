# Function that takes a feature and a distribution and verifies whether the feature is distributed according to the given 
# distribution. Returns TRUE if that is the case, FALSE otherwise

distribution_verification = function(regfamily,feature) {
  
  #funtion to autoscale (unit mean and sd) for the feature vectors
  autoscale = function(vec){
    return((vec - rep(mean(vec),length(vec)))/rep(sd(vec),length(vec)))
  }
  
  feature = autoscale(feature)
  len = length(feature)
  #estBetaParams <- function(mu, var) {
  #  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  #  beta <- alpha * (1 / mu - 1)
  #  return(params = list(alpha = alpha, beta = beta))
  #}
  
  
  #variance = var(feature)
  #std_dev = sd(feature)
  #mu = mean(feature)
  #feature_len = length(feature)
  
  if(regfamily =="beta") {
    vec2 = autoscale(rbeta(len, shape1 = 2, shape2 = 1, ncp = 5))
    test = ks.test(feature,vec2)
    #test = ks.test(feature,"pbeta",shape1 = 2, shape2 = 1, ncp = 5)
    if (test$p.value>0.05) {
      #print(test$p.value)
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  

  else if (regfamily =="gamma") {
    
    #shape a=(mean^2/variance) and rate b = mean/variance.
    #a = (mu^2/variance)
    #b = (mu/variance)
    #gamm <- rgamma(n = 20, shape = a, rate = b)
    vec2 = autoscale(rgamma(len, shape = 0.1, rate= 10))
    test = ks.test(feature,vec2)
    #print(test$p.value)
    #test = ks.test(feature,"pgamma",shape = 0.1,rate = 10)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (regfamily == "logis") {
    
    # here location should equal the mean of the distribution. variance = (s^2*pi^2)/3
    #location = mu
    #scale = sqrt((variance*3)/(pi^2))
    #logis <- rlogis(n = 20, location=location, scale = scale)
    vec2 = autoscale(rlogis(len,location=0.1,scale=0.01))
    test = ks.test(feature,vec2)
    #test = ks.test(feature,"plogis",location = 0.1, scale = 0.01)
    if (test$p.value>0.05) {
      #print(test$p.value)
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (regfamily == "normal") {
    vec2 = autoscale(rnorm(len,mean=0.5,sd=0.1))
    test = ks.test(feature,vec2)
    #test = ks.test(feature,"pnorm",mean = 0.5, sd = 0.1)
    if (test$p.value>0.05) {
      #print(test$p.value)
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  # } else if (regfamily == "binomial") {
  #   
  #   # mean should be size*prob. how to choose size? As if now, it is the length of the feature.
  #   # prob = mu/feature_len
  #   prob = mu
  #   binom <- rbinom(n = 20, size = 1, prob = prob)
  #   test = ks.test(binom,feature)
  #   if (test$p.value>0.05) {
  #     return(TRUE)
  #   } else {
  #     return(FALSE)
  #   }
  #   
  # } else if (regfamily == "poisson") {
  #   
  #   ### vectors of poisson distribution. lambda = variance and lambda = mean
  #   lambda = std_dev^2
  #   pois <- rpois(n = 20, lambda = lambda)
  #   test = ks.test(pois,feature)
  #   if (test$p.value>0.05) {
  #     return(TRUE)
  #   } else {
  #     return(FALSE)
  #   }
    
  } else if (regfamily == "exponential") {
  
    ### vectors of exponential distribution. mean = standard deviation = 1/rate
    #rate = 1/std_dev
    #expon = rexp(n = 20, rate = rate)
    vec2 = autoscale(rexp(len,rate=20))
    test = ks.test(feature,vec2)
    #test = ks.test(feature,"pexp",rate=20)
    if (test$p.value>0.05) {
      #print(test$p.value)
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else {
    
    stop("ERROR: invalid regression family given ")
  }
}



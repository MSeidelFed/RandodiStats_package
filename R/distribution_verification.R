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
    gamm <- rgamma(n = feature_len, shape = a, rate = b)
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
    logis <- rlogis(n = feature_len, location=location, scale = scale)
    test = ks.test(logis,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (regfamily == "beta") {
    
    stop("how to calculate ncp from a given vector?")
    ### vectors of beta distribution. alpha =  ((1 - mean) / var - 1 / mean) * mean ^ 2 and beta = alpha * (1 / mean - 1)
    #beta <- rbeta(n = feature_len, shape1 = 8, shape2 = 2, ncp = 5)
    #test = ks.test(beta,feature)
    #if (test$p.value>0.05) {
    #  print("agree")
    #} else {
    #  print("disagree")
    #}
  
  } else if (regfamily == "normal") {
    
    normal <- rnorm(n = feature_len, mean = mu, sd = std_dev)
    test = ks.test(normal,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (regfamily == "binomial") {
    
    # mean should be n*prob. how to choose size, as if now, it is the length of the feature?
    prob = mu/feature_len
    binom <- rbinom(n = feature_len, size = feature_len, prob = prob)
    test = ks.test(binom,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (regfamily == "poisson") {
    
    ### vectors of poisson distribution. lambda = variance and lambda = mean
    lambda = std_dev^2
    pois <- rpois(n = feature_len, lambda = lambda)
    test = ks.test(pois,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (regfamily == "exponential") {
  
    ### vectors of exponential distribution. mean = standard deviation = 1/rate
    rate = 1/std_dev
    expon = rexp(n = feature_len, rate = rate)
    test = ks.test(expon,feature)
    if (test$p.value>0.05) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else {
    
    #stop("ERROR: invalid regression family given ")
  }
}




# Function calls with for each distribution

gamm = rgamma(n = 20,shape = 0.2,rate = 4)
distribution_verification(feature = gamm, regfamily = "gamma")

logis = rlogis(n = 20, location = 0.1, scale = 0.01)
distribution_verification(feature = logis, regfamily = "logis")

binom = rbinom(n = 20, size = 20, prob = 0.5)
distribution_verification(feature = binom, regfamily = "binomial")

pois <- rpois(n = feature_len, lambda = lambda)
distribution_verification(feature = pois, regfamily = "poisson")

expon = rexp(n = feature_len, rate = rate)
distribution_verification(feature = expon, regfamily = "exponential")



# testing the approach. 
# Create a random vector of size 20 for each distribution. Reverse engineer the parameters and use them as input to a second
# vector of the same size and distribution. 
# Check if both vectors come from the same distribution. Should not occur less than in 95% of cases 


# gamma: shape and rate cannot be estimated from small vectors as it seems, for large vectors it works however. 
# Count is high nonetheless
count = 0
for(i in 1:100) {
  gamm = rgamma(n = 20,shape = 0.2,rate = 4)
  shape = (mean(gamm)^2)/var(gamm)
  rate = mean(gamm)/var(gamm)
  gamm2 = rgamma(n = 20,shape = shape, rate = rate)
  test = ks.test(gamm,gamm2)
  if(test$p.value>0.05) {
    count = count + 1
  }
}

#logis
count = 0
for(i in 1:100) {
  logis = rlogis(n = 20, location = 0.1, scale = 0.01)
  location = mean(logis)
  scale = sqrt((var(logis*3))/(pi^2))
  logis2 = rlogis(n=20,location = location, scale = scale)
  test = ks.test(logis,logis2)
  if(test$p.value > 0.05) {
    count = count + 1
  }
}

#normal
count = 0
for(i in 1:100) {
  normal = rnorm(n=20, mean=0, sd= 1)
  mu = mean(normal)
  std_dev = sd(normal)
  normal2 = rnorm(n=20,mean=mu,sd=std_dev)
  test = ks.test(normal,normal2)
  if (test$p.value>0.05) {
    count = count+1
  }
}

# binom
count = 0
for(i in 1:100){
  binom = rbinom(n = 20, size = 20, prob = 0.5)
  prob = mean(binom)/length(binom)
  binom2 = rbinom(n=20,size = 20,prob = prob)
  test = ks.test(binom,binom2)
  if(test$p.value>0.05) {
    count = count +1
  }
}

# poisson
count = 0
for(i in 1:100){
  pois = rpois(n = 20, lambda = 0.01)
  lambda = sd(pois)^2
  pois2 = rpois(n=20, lambda = lambda)
  test = ks.test(pois,pois2)
  if(test$p.value>0.05) {
    count = count +1
  }
}

# exponential
count = 0
for(i in 1:100){
  exp = rexp(n = 20, rate = 20)
  rate = 1/sd(exp)
  exp2 = rexp(n=20, rate = rate)
  test = ks.test(exp,exp2)
  if(test$p.value>0.05) {
    count = count +1
  }
}



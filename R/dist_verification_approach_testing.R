# Testing the approach. 
# Create a random vector of size 20 for each distribution. Reverse engineer the parameters and use them as input to a second
# vector of the same size and distribution. 
# Check if both vectors come from the same distribution. Should not occur less than in 95% of cases 

# Function to calculate parameters of the beta distribution
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}



vec_length = c(10,20,30,40,50,60,70,80,90,100,150,200,300,400,500)
res = vector(mode = 'numeric', length = length(vec_length))
par(mfrow=c(2,3))


#beta
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500){
    rbet1 = rbeta(20,shape1 = 0.3,shape2 = 1)
    mu = mean(rbet1)
    variance = var(rbet1)
    params = estBetaParams(mu,variance)
    alpha = params[[1]]
    beta = params[[2]]
    rbet2 = rbeta(20,shape1=alpha,shape2 = beta)
    test = ks.test(rbet1,rbet2)
    if(test$p.value>0.05){
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Beta", ylab="",xlab="" ,ylim=c(0.8,1))

# gamma
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    gamm = rgamma(n = 20,shape = 0.2,rate = 4)
    shape = (mean(gamm)^2)/var(gamm)
    rate = mean(gamm)/var(gamm)
    gamm2 = rgamma(n = vec_length[i],shape = shape, rate = rate)
    test = ks.test(gamm,gamm2)
    if(test$p.value>0.05) {
      count = count + 1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Gamma", ylab="",xlab="" ,ylim=c(0.8,1))

# logis 
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    logis = rlogis(n = 20, location = 0.1, scale = 0.01)
    location = mean(logis)
    scale = sqrt((var(logis*3))/(pi^2))
    logis2 = rlogis(n=vec_length[i],location = location, scale = scale)
    test = ks.test(logis,logis2)
    if(test$p.value > 0.05) {
      count = count + 1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Logistic", ylab="",xlab="" ,ylim=c(0.8,1))


# normal
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    normal = rnorm(n=20, mean=0, sd= 1)
    mu = mean(normal)
    std_dev = sd(normal)
    normal2 = rnorm(n=vec_length[i],mean=mu,sd=std_dev)
    test = ks.test(normal,normal2)
    if (test$p.value>0.05) {
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Normal", ylab="",xlab="" ,ylim=c(0.8,1))


# binom. 
# Here size is chosen as 1 because in our normalized data we have values only between 0 and 1, is that correct?
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    binom = rbinom(n = 20, size = 1, prob = 0.5)
    prob = mean(binom)
    binom2 = rbinom(n=vec_length[i],size = 1,prob = prob)
    test = ks.test(binom,binom2)
    if (test$p.value>0.05) {
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Binomial", ylab="",xlab="" ,ylim=c(0.8,1))


# poisson
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    pois = rpois(n = 20, lambda = 0.01)
    lambda = sd(pois)^2
    pois2 = rpois(n=vec_length[i], lambda = lambda)
    test = ks.test(pois,pois2)
    if (test$p.value>0.05) {
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Poisson", ylab="",xlab="" ,ylim=c(0.8,1))


# exponential
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    exp = rexp(n = 20, rate = 20)
    rate = 1/sd(exp)
    exp2 = rexp(n=vec_length[i], rate = rate)
    test = ks.test(exp,exp2)
    if (test$p.value>0.05) {
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Exponential", ylab="",xlab="" ,ylim=c(0.8,1))


# Testing the approach for a varying length of input feature (1st vector) and a fixed length of the second random vector constructed
# through mean and standard deviation of the feature vector. Arial 7 width 13.6 and 8.9 

vec_length = c(3,5,7,10,20,40,50,60,70,80,90,100,110,120,200,400,600,800,1000)
res = vector(mode = 'numeric', length = length(vec_length))
par(mfrow=c(1,1))

# gamma
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    gamm = rgamma(n = vec_length[i],shape = 0.2,rate = 4)
    shape = (mean(gamm)^2)/var(gamm)
    rate = mean(gamm)/var(gamm)
    gamm2 = rgamma(n = vec_length[i],shape = shape, rate = rate)
    test = ks.test(gamm,gamm2)
    if(test$p.value>0.05) {
      count = count + 1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Gamma", ylab="",xlab="" ,ylim=c(0.8,1))


# logis - works only well for where the feature vector is <=40 
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    logis = rlogis(n = vec_length[i], location = 0.1, scale = 0.01)
    location = mean(logis)
    scale = sqrt((var(logis*3))/(pi^2))
    logis2 = rlogis(n=vec_length[i],location = location, scale = scale)
    test = ks.test(logis,logis2)
    if(test$p.value > 0.05) {
      count = count + 1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Logistic", ylab="",xlab="" ,ylim=c(0.8,1))


# normal
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    normal = rnorm(n= vec_length[i], mean=0, sd= 1)
    mu = mean(normal)
    std_dev = sd(normal)
    normal2 = rnorm(n=vec_length[i],mean=mu,sd=std_dev)
    test = ks.test(normal,normal2)
    if (test$p.value>0.05) {
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Normal", ylab="",xlab="" ,ylim=c(0.8,1))


# binom. 
# Here size is chosen as 1 because in our normalized data we have values only between 0 and 1, is that correct?
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    binom = rbinom(n = vec_length[i], size = 1, prob = 0.5)
    prob = mean(binom)
    binom2 = rbinom(n=vec_length[i],size = 1,prob = prob)
    test = ks.test(binom,binom2)
    if (test$p.value>0.05) {
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Binomial", ylab="",xlab="" ,ylim=c(0.8,1))

# poisson
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    pois = rpois(n = vec_length[i], lambda = 0.01)
    lambda = sd(pois)^2
    pois2 = rpois(n=vec_length[i], lambda = lambda)
    test = ks.test(pois,pois2)
    if (test$p.value>0.05) {
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Poisson", ylab="",xlab="" ,ylim=c(0.8,1))


# exponential
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500) {
    exp = rexp(n = vec_length[i], rate = 20)
    rate = 1/sd(exp)
    exp2 = rexp(n=vec_length[i], rate = rate)
    test = ks.test(exp,exp2)
    if (test$p.value>0.05) {
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Exponential", ylab="",xlab="" ,ylim=c(0.8,1))

#beta
for(i in 1:length(vec_length)){
  count = 0
  for(j in 1:500){
    rbet1 = rbeta(20,shape1 = 0.3,shape2 = 1)
    mu = mean(rbet1)
    variance = var(rbet1)
    params = estBetaParams(mu,variance)
    alpha = params[[1]]
    beta = params[[2]]
    rbet2 = rbeta(20,shape1=alpha,shape2 = beta)
    test = ks.test(rbet1,rbet2)
    if(test$p.value>0.05){
      count = count+1
    }
  }
  res[i] = count/j
}
plot(vec_length,res,main="Beta", ylab="",xlab="" ,ylim=c(0.8,1))





#Altered function to Verify the accuracy with the real data. 

verify_dist_approach <- function(Distribution_test_mat = distribution_test_mat()) {
  
  # Calls Variables2shapes with the input matrix. Gives back for each feature the values for skweness^2 and kurtosis. 
  protein_coords <- as.data.frame(x = Variables2Shapes(Distribution_test_mat),
                                  row.names = colnames(Distribution_test_mat))
  
  # This calls Variables2Shapes with the distribution_test_mat() as an input. Distribution_test_mat gives back a matrix
  # with a thousand random values for square skewness and kurtosis of the 7 supported exponential distributions in R 
  distribution_mat <- Variables2Shapes(distribution_test_mat())
  
  ### this object must be the kurtosis and square of skewness of the distributions
  
  # Takes the means of the thousand values for square skewness and kurtosis of each of the 7 distributions
  distribution_coords <- rbind(colMeans(distribution_mat[1:1000,]),
                               colMeans(distribution_mat[1001:2000,]),
                               colMeans(distribution_mat[2001:3000,]),
                               colMeans(distribution_mat[3001:4000,]),
                               colMeans(distribution_mat[4001:5000,]),
                               colMeans(distribution_mat[5001:6000,]))
  
  # Creates a distance matrix between the values for protein_coords and the random values from distribution_coords. 
  dist_mat <- as.matrix(raster::pointDistance(p1 = protein_coords,
                                              p2 = distribution_coords, lonlat = F, allpairs = T))
  
  Family_selection_GLM_R <- cbind(
    Family = c("gamma","logis","beta","normal","poisson","exponential"),
    Link = c("Inverse","Logit","Logit","Identity","Log","Inverse"),
    GLM_R = c("Gamma","quasibinomial","quasibinomial","gaussian","quasipoisson","Gamma"))
  
  runner <- c()
  
  # Here we loop over the rows of the distance and the smallest value for each row (smallest euclidean distance) is
  # selected. This value determines the chosen distribution.
  # Question: How can a case occur where test is empty? 
  for (i in 1:dim(dist_mat)[1]) {
    
    test <- Family_selection_GLM_R[, "Family"][which(dist_mat[i,] == min(dist_mat[i,]))]
    # equal should be true if the distributions match and false if they do not
    equal = distribution_verification(feature = Distribution_test_mat[,i], regfamily = test)
    # assign the appropriate glm input to the test
    if (equal) {
      test = Family_selection_GLM_R[, "Family"][which(Family_selection_GLM_R[,"Family"] == test)]
    }
    # if the selected distributions differ, test will be non-parametric. 
    if (!equal) {
      test = "non-parametric"
    }
    
    if (length(test) > 0) {
      
      runner[i] <- test
      
    }
  }
  return(runner)
  
}

# Calculate accuracy of the whole approach on real data. Accuracy is averaged over all distributions together
distributions = verify_dist_approach(class_comparison_mat)
1 - (length(which(distributions=="non-parametric")))/length(distributions)


# Verify accuracy with a negative control. Here, I want to find out, how long the feature/random vector must be in order for 
# a ks.test() to give accurate results!

#take a vector and return a second random vector that is normally distributed with same mean and sd as input 
r_normal = function(vec){
  n = length(vec)
  normal = rnorm(n=n,mean=mean(vec),sd=sd(vec))
  return(normal)
}

r_expon = function(vec){
  n = length(vec)
  rate = 1/sd(vec)
  expon = rexp(n=n, rate = rate)
  return(expon)
}

r_binom = function(vec){
  n = length(vec)
  prob = mean(vec)
  binom = rbinom(n=n,size = 1,prob = prob)
  return(binom)
}

r_poisson = function(vec){
  n = length(vec)
  lambda = sd(vec)^2
  pois = rpois(n=n, lambda = lambda)
  return(pois)
}

r_beta = function(vec){
  n = length(vec)
  mu = mean(vec)
  variance = var(vec)
  params = estBetaParams(mu,variance)
  alpha = params[[1]]
  beta = params[[2]]
  print(alpha)
  print(beta)
  rbet = rbeta(n,shape1=alpha,shape2 = beta)
  return(rbet)
}

r_gamma = function(vec){
  n = length(vec)
  shape = (mean(vec)^2)/var(vec)
  rate = mean(vec)/var(vec)
  gamma = rgamma(n,rate=rate,shape=shape)
  return(gamma)
}

verify_normal = function(){
  vec_length = c(7,10,20,40,50,60,70,80,90,100,110,120,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  for(i in 1:length(vec_length)){
    print(i)
    normal = rnorm(n=vec_length[i],mean=0,sd=1)
    normal = feature_scaling(normal)
    #both vectors from same distribution and are correctly identified as that
    TP = 0
    #both vectors from different distribution and are correctly identified as that
    TN = 0
    #both vectors from different distribution but not corretly identified as different
    FN = 0
    #both vectors from same distribution but not corretly identified as same
    FP = 0
    for(j in 1:500){
      print(j)
      normal2 = r_normal(normal)
      expon = r_expon(normal)
      pois = r_poisson(normal) 
      binom = r_binom(normal)
      gamma = r_gamma(normal)
      #beta = r_beta(normal)
      p1 = ks.test(normal,normal2)$p.value
      p2 = ks.test(normal,expon)$p.value
      p3 = ks.test(normal,pois)$p.value
      p4 = ks.test(normal,binom)$p.value
      p5 = ks.test(normal,gamma)$p.value
      #p6 = ks.test(normal,beta)$p.value

      if(p1>0.05){
        TP = TP + 1
      } else {
        FP = FP + 1
      }
      if(p2<=0.05){
        TN = TN +1
      } else {
        FN = FN +1
      }
      if (p3<=0.05) {
        TN = TN +1 
      } else {
        FN = FN +1
      }
      if (p4<=0.05) {
        TN = TN +1
      } else {
        FN = FN +1
      }
      if (p5<=0.05) {
        TN = TN +1
      } else {
        FN = FN +1
      }
    }
    accuracy[i] = (TP+TN) / (TP+TN+FN+FP)
    print(accuracy[i])
  }
  plot(vec_length,accuracy,main="Normal", ylab="",xlab="" ,ylim=c(0.8,1))
}

verify_normal()


verify_expon = function(){
  vec_length = c(7,10,20,40,50,60,70,80,90,100,110,120,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  for(i in 1:length(vec_length)){
    print(i)
    expon = rexp(n=vec_length[i],rate = 10)
    expon = feature_scaling(expon)
    #both vectors from same distribution and are correctly identified as that
    TP = 0
    #both vectors from different distribution and are correctly identified as that
    TN = 0
    #both vectors from different distribution but not corretly identified as different
    FN = 0
    #both vectors from same distribution but not corretly identified as same
    FP = 0
    for(j in 1:500){
      print(j)
      normal = r_normal(expon)
      expon2 = r_expon(expon)
      pois = r_poisson(expon) 
      binom = r_binom(expon)
      gamma = r_gamma(expon)
      #beta = r_beta(normal)
      p1 = ks.test(expon,expon2)$p.value
      p2 = ks.test(expon,normal)$p.value
      p3 = ks.test(expon,pois)$p.value
      p4 = ks.test(expon,binom)$p.value
      p5 = ks.test(expon,gamma)$p.value
      #p6 = ks.test(normal,beta)$p.value
      
      if(p1>0.05){
        TP = TP + 1
      } else {
        FP = FP + 1
      }
      if(p2<=0.05){
        TN = TN +1
      } else {
        FN = FN +1
      }
      if (p3<=0.05) {
        TN = TN +1 
      } else {
        FN = FN +1
      }
      if (p4<=0.05) {
        TN = TN +1
      } else {
        FN = FN +1
      }
      if (p5<=0.05) {
        TN = TN +1
      } else {
        FN = FN +1
      }
    }
    accuracy[i] = (TP+TN) / (TP+TN+FN+FP)
    print(accuracy[i])
  }
  plot(vec_length,accuracy,main="Normal", ylab="",xlab="" ,ylim=c(0,1))
}
verify_expon()

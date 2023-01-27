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
    bin = rgamma(n = 20,shape = 0.2,rate = 4)
    shape = (mean(bin)^2)/var(bin)
    rate = mean(bin)/var(bin)
    gamm2 = rgamma(n = vec_length[i],shape = shape, rate = rate)
    test = ks.test(bin,gamm2)
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
    bin = rgamma(n = vec_length[i],shape = 0.2,rate = 4)
    shape = (mean(bin)^2)/var(bin)
    rate = mean(bin)/var(bin)
    gamm2 = rgamma(n = vec_length[i],shape = shape, rate = rate)
    test = ks.test(bin,gamm2)
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
  vec = feature_scaling(vec)
  n = length(vec)
  mu = mean(vec)
  variance = var(vec)
  params = estBetaParams(mu,variance)
  alpha = params[[1]]
  beta = params[[2]]
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
  
    #both vectors from same distribution and are correctly identified as that
    TP = 0
    #both vectors from different distribution and are correctly identified as that
    TN = 0
    #both vectors from different distribution but not corretly identified as different
    FN = 0
    #both vectors from same distribution but not corretly identified as same
    FP = 0
    for(j in 1:100){
      normal = rnorm(n=vec_length[i],mean=0.5,sd=0.1)
      normal = feature_scaling(normal)
      p1 = ks.test(normal,"pnorm", mean = params_normal(normal)[1],sd = params_normal(normal)[2])$p.value
      p2 = ks.test(normal,"pexp",rate = params_expon(normal)[1])$p.value
      p3 = ks.test(normal,"ppois",lambda = params_poisson(normal)[1])$p.value
      p4 = ks.test(normal,"pbinom", size= 1, prob = params_binom(normal)[1])$p.value
      p5 = ks.test(normal,"pgamma",rate = params_gamma(normal)[1],shape = params_gamma(normal)[2])$p.value
      #p6 = ks.test(normal,beta)$p.value
      
      # keep this for vectors > 10 or 15, for vectors smaller than that, take strict unambiguous definitions
      if(p1>0.05){
        TP = TP + 1
      } else {
        FN = FN + 1
      }
      if(p2<=0.05 & p3 <=0.05 & p4 <=0.05 & p5 <=0.05 ){
        TN = TN +1
      } else if (p2 > 0.05 | p3 > 0.05 | p4 > 0.05 | p5 > 0.05) {
        # is the vector only normal or can it also come from more different distributions
        FP = FP +1
      }
    }
    print(c(p1,p2,p3,p4,p5))
    print(c(TP,FN,TN,FP))
    accuracy[i] = (TP+TN) / (TP+TN+FN+FP)
    print(accuracy[i])
  }
  plot(vec_length,accuracy,main="Normal", ylab="",xlab="" ,ylim=c(0,1))
}

verify_normal()


verify_expon = function(){
  vec_length = c(7,10,20,40,50,60,70,80,90,100,110,120,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  for(i in 1:length(vec_length)){
    #both vectors from same distribution and are correctly identified as that
    TP = 0
    #both vectors from different distribution and are correctly identified as that
    TN = 0
    #both vectors from different distribution but not corretly identified as different
    FN = 0
    #both vectors from same distribution but not corretly identified as same
    FP = 0
    for(j in 1:100){
      expon = rexp(n=vec_length[i],rate = 10)
      expon = feature_scaling(expon)
      normal = r_normal(expon)
      expon2 = r_expon(expon)
      pois = r_poisson(expon) 
      binom = r_binom(expon)
      gamma = r_gamma(expon)
      beta = r_beta(expon)
      p1 = ks.test(expon,expon2)$p.value
      p2 = ks.test(expon,normal)$p.value
      p3 = ks.test(expon,pois)$p.value
      p4 = ks.test(expon,binom)$p.value
      p5 = ks.test(expon,gamma)$p.value
      p6 = ks.test(expon,beta)$p.value
      
      if(p1>0.05 & p2<=0.05 & p3<=0.05 & p4<=0.05 & p5<=0.05){
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
  plot(vec_length,accuracy,main="Exponential", ylab="",xlab="" ,ylim=c(0,1))
}
verify_expon()


verify_gamma = function(){
  vec_length = c(7,10,20,40,50,60,70,80,90,100,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  for(i in 1:length(vec_length)){
    #both vectors from same distribution and are correctly identified as that
    TP = 0
    #both vectors from different distribution and are correctly identified as that
    TN = 0
    #both vectors from different distribution but not corretly identified as different
    FN = 0
    #both vectors from same distribution but not corretly identified as same
    FP = 0
    for(j in 1:100){
      gamm = rgamma(n=vec_length[i],shape = 0.2,rate = 4)
      gamm = feature_scaling(gamm)
      normal = r_normal(gamm)
      expon = r_expon(gamm)
      pois = r_poisson(gamm) 
      binom = r_binom(gamm)
      gamm2 = r_gamma(gamm)
      beta = r_beta(gamm)
      p1 = ks.test(gamm,gamm2)$p.value
      p2 = ks.test(gamm,normal)$p.value
      p3 = ks.test(gamm,pois)$p.value
      p4 = ks.test(gamm,binom)$p.value
      p5 = ks.test(gamm,expon)$p.value
      
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
  plot(vec_length,accuracy,main="Gamma", ylab="",xlab="" ,ylim=c(0,1))
}
verify_gamma()


verify_binom = function(){
  vec_length = c(7,10,20,40,50,60,70,80,90,100,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  for(i in 1:length(vec_length)){
    bin = rbinom(n=vec_length[i], size=1, prob= 0.5)
    bin = feature_scaling(bin)
    #both vectors from same distribution and are correctly identified as that
    TP = 0
    #both vectors from different distribution and are correctly identified as that
    TN = 0
    #both vectors from different distribution but not corretly identified as different
    FN = 0
    #both vectors from same distribution but not corretly identified as same
    FP = 0
    for(j in 1:100){
      print(j)
      normal = r_normal(bin)
      expon = r_expon(bin)
      pois = r_poisson(bin) 
      bin2 = r_binom(bin)
      gamm = r_gamma(bin)
      #beta = r_beta(normal)
      p1 = ks.test(bin,bin2)$p.value
      p2 = ks.test(bin,normal)$p.value
      p3 = ks.test(bin,pois)$p.value
      p4 = ks.test(bin,gamm)$p.value
      p5 = ks.test(bin,expon)$p.value
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
  plot(vec_length,accuracy,main="Binomial", ylab="",xlab="" ,ylim=c(0,1))
}

verify_binom()

verify_pois = function(){
  vec_length = c(7,10,20,40,50,60,70,80,90,100,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  for(i in 1:length(vec_length)){
    #same as in the dist_test_mat
    pois = rpois(n=vec_length[i], lambda = 0.01)
    pois = autoscale(pois)
    #both vectors from same distribution and are correctly identified as that
    TP = 0
    #both vectors from different distribution and are correctly identified as that
    TN = 0
    #both vectors from different distribution but not corretly identified as different
    FN = 0
    #both vectors from same distribution but not corretly identified as same
    FP = 0
    for(j in 1:100){
      print(j)
      # all other vectors arbitrary parameters and autoscaled
      normal = r_normal(pois)
      expon = r_expon(pois)
      # random input parameter
      pois2 = rpois(n=vec_length[i],lambda = 0.1) 
      binom = r_binom(pois)
      gamm = r_gamma(pois)
      #beta = r_beta(normal)
      p1 = ks.test(pois,pois2)$p.value
      p2 = ks.test(pois,normal)$p.value
      p3 = ks.test(pois,binom)$p.value
      p4 = ks.test(pois,gamm)$p.value
      p5 = ks.test(pois,expon)$p.value
      #p6 = ks.test(normal,beta)$p.value
      
      if(p1>0.05){
        TP = TP + 1

      } else {
        FN = FN + 1
        
      }
      
      if(p2<=0.05 & p3 <=0.05 & p4 <=0.05){
        TN = TN +1 
      } else if (p2 > 0.05 | p3 > 0.05 | p4 > 0.05) {
        # is the vector only normal or can it also come from more different distributions
        FP = FP +1
      }
    }
    accuracy[i] = (TP+TN) / (TP+TN+FN+FP)
    print(accuracy[i])
  }
  plot(vec_length,accuracy,main="Poisson", ylab="",xlab="" ,ylim=c(0,1))
}

verify_pois()


params_normal = function(vec){
  mu = mean(vec)
  std = sd(vec)
  return(c(mu,std))
}

params_expon = function(vec){
  rate = 1/sd(vec)
  return(c(rate))
}

params_binom = function(vec){
  prob = mean(vec)
  return(c(prob))
}

params_poisson = function(vec){
  lambda = sd(vec)^2
  return(c(lambda))
}

params_beta = function(vec){
  vec = feature_scaling(vec)
  mu = mean(vec)
  variance = var(vec)
  params = estBetaParams(mu,variance)
  alpha = params[[1]]
  beta = params[[2]]
  return(c(alpha,beta))
}

params_gamma = function(vec){
  vec = feature_scaling(vec)
  rate = mean(vec)/var(vec)
  shape = (mean(vec)^2)/var(vec)
  return(c(rate,shape))
}

params_logis = function(vec){
  vec = feature_scaling(vec)
  location = mean(vec)
  scale = sqrt((var(vec*3))/(pi^2))
  return(c(location,scale))
}


vec = rbeta(100,1,2)

ks.test(vec,"pgamma",params_gamma(vec)[1],params_gamma(vec)[2])
ks.test(vec,"pnorm",params_normal(vec)[1],params_normal(vec)[2])
ks.test(vec,"pbinom", size= 1,params_binom(vec)[1])
ks.test(vec,"pexp",params_expon(vec)[1])
ks.test(vec,"ppois",params_poisson(vec)[1])
#ks.test(vec,"pbeta",params_beta(vec)[1],params_beta(vec)[2])


verify_normal = function(){
  vec_length = c(7,10,20,40,50,60,70,80,90,100,110,120,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  for(i in 1:length(vec_length)){
    #normal = rnorm(n=vec_length[i],mean=0,sd=1)
    #normal = feature_scaling(normal)
    TP = 0
    TN = 0
    FP = 0
    FN = 0
    
    for(j in 1:100){
      normal = rnorm(n=vec_length[i],mean=0.5,sd=0.1)
      normal = feature_scaling(normal)
      p1 = ks.test(normal,"pnorm", mean = params_normal(normal)[1],sd = params_normal(normal)[2])$p.value
      p2 = ks.test(normal,"pexp",rate = params_expon(normal)[1])$p.value
      p3 = ks.test(normal,"ppois",lambda = params_poisson(normal)[1])$p.value
      p4 = ks.test(normal,"pbinom", size= 1, prob = params_binom(normal)[1])$p.value
      p5 = ks.test(normal,"pgamma",rate = params_gamma(normal)[1],shape = params_gamma(normal)[2])$p.value
      #p6 = ks.test(normal,beta)$p.value
      
      # keep this for vectors > 10 or 15, for vectors smaller than that, take strict unambiguous definitions
      if(p1>0.05){
        TP = TP + 1
      } else {
        FN = FN + 1
        
        if(p2<=0.05 & p3 <=0.05 & p4 <=0.05 & p5 <=0.05 ){
          TN = TN +1
        } else if (p2 > 0.05 | p3 > 0.05 | p4 > 0.05 | p5 > 0.05) {
          # is the vector only normal or can it also come from more different distributions
          FP = FP +1
        }
      }
      
    }
    print(c(p1,p2,p3,p4,p5))
    accuracy[i] = (TP+TN) / (TP+TN+FN+FP)
    print(accuracy[i])
  }
  plot(vec_length,accuracy,main="Normal", ylab="",xlab="" ,ylim=c(0,1))
}

verify_normal()

verify_expon = function(){
  vec_length = c(7,10,20,40,50,60,70,80,90,100,110,120,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  for(i in 1:length(vec_length)){
    
    #correct prediction
    correct = 0
    #incorrect prediction
    incorrect = 0

    for(j in 1:100){
      expon = rexp(n=vec_length[i],rate = 20)
      expon = feature_scaling(expon)
      p1 = ks.test(expon,"pexp",rate = params_expon(expon)[1])$p.value
      p2 = ks.test(expon,"pnorm", mean = params_normal(expon)[1],sd = params_normal(expon)[2])$p.value
      p3 = ks.test(expon,"ppois",lambda = params_poisson(expon)[1])$p.value
      p4 = ks.test(expon,"pbinom", size= 1, prob = params_binom(expon)[1])$p.value
      p5 = ks.test(expon,"pgamma",rate = params_gamma(expon)[1],shape = params_gamma(expon)[2])$p.value
      #p6 = ks.test(normal,beta)$p.value
      
      if(p1>0.05 & p2 <= 0.05 & p3 <= 0.05 & p4 <= 0.05 & p5 <= 0){
        correct = correct + 1
      } else {
        incorrect = incorrect + 1
      }
    }
    print(c(correct,incorrect))
    print(c(p1,p2,p3,p4,p5))
    accuracy[i] = (correct) / (correct+incorrect)
    print(accuracy[i])
  }
  plot(vec_length,accuracy,main="Exponential", ylab="",xlab="" ,ylim=c(0,1))
}

verify_expon()

verify_gamma = function(){
  vec_length = c(7,10,20,40,50,60,70,80,90,100,110,120,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  for(i in 1:length(vec_length)){
    
    #correct prediction
    correct = 0
    #incorrect prediction
    incorrect = 0
    
    for(j in 1:100){
      gamma = rgamma(n=vec_length[i], rate = 4 ,shape = 0.2)
      gamma = feature_scaling(gamma)
      p1 = ks.test(gamma,"pgamma",rate = params_gamma(gamma)[1],shape = params_gamma(gamma)[2])$p.value
      p2 = ks.test(gamma,"pnorm", mean = params_normal(gamma)[1],sd = params_normal(gamma)[2])$p.value
      p3 = ks.test(gamma,"ppois",lambda = params_poisson(gamma)[1])$p.value
      p4 = ks.test(gamma,"pbinom", size= 1, prob = params_binom(gamma)[1])$p.value
      p5 = ks.test(gamma,"pexp",rate = params_expon(gamma)[1])$p.value
      
      if(p1>0.05 & p2 <= 0.05 & p3 <= 0.05 & p4 <= 0.05 & p5 <= 0.5){
        correct = correct + 1
      } else {
        incorrect = incorrect + 1
      }
    }
    print(c(correct,incorrect))
    print(c(p1,p2,p3,p4,p5))
    accuracy[i] = (correct) / (correct+incorrect)
    print(accuracy[i])
  }
  plot(vec_length,accuracy,main="Exponential", ylab="",xlab="" ,ylim=c(0,1))
}

verify_gamma()




test_mat = matrix(NULL,nrow = 50,ncol = 1000)
for(i in 1:5)


vec = rgamma(10000,1,0.1)
ks.test(vec,"pnorm",mean(vec),sd(vec))
fact = as.factor(rep(c("A","B","C","D","E","F","G","H","I","J"),1000))
model = glm(vec~fact,"Gamma")
fit = model$fitted.values
residuals = model$residuals
plot(fit,residuals)
shapiro.test(residuals)


res_model = lm(fit~residuals)
abline(res_model$coefficients[1] ,  res_model$coefficients[2] )





set.seed(19191)     
x = rnorm(1000,0,1)
y = rgamma(20,shape=2,rate = 2)
plot(ecdf(x))
plot(ecdf(y))
curve(pnorm, from = 0, to = 3,add=TRUE)
ks.test(y,"pnorm",mean(y),sd(y))


y = runif(200,-3,3)
plot(ecdf(y))     
curve(pnorm, from = -3, to = 3,add=TRUE)
ks.test(y,"pnorm",mean(y),sd(y))





# Autoscale input vectors - autoscale vectors i am testing againsts - initial parameters same one as from distribution_test_mat
autoscale = function(vec){
  return((vec - rep(mean(vec),length(vec)))/rep(sd(vec),length(vec)))
}
vec = (vec - mean(vec))/sd(vec)

vec1 = rgamma(20,shape=2,rate=1)
vec2 = rgamma(20,shape = 0.1,rate = 10) 
ks.test(autoscale(vec1),autoscale(vec2))
plot(sort(vec1),sort(vec2))
plot(autoscale(sort(vec1)),autoscale(sort(vec2)))
plot(autoscale(vec1))
plot(autoscale(sort(vec1)))+abline()


plot(vec1,type="l",col="red")+
lines(vec2,col="green")

df = as.data.frame(cbind(x=c(1:length(vec1)),y1=sort(vec1),y2=sort(vec2)))
df2 = as.data.frame(cbind(x=c(1:length(vec1)),y1=sort(autoscale(vec1)),y2=sort(autoscale(vec2))))
g <- ggplot(df2, aes(x))
g <- g + geom_line(aes(y=y1), colour="red")
g <- g + geom_line(aes(y=y2), colour="green")
g

g+theme_classic()

# This is my task ..
verify_normal = function(){
  
  #function to autoscale a vector
  autoscale = function(vec){
    return((vec - rep(mean(vec),length(vec)))/rep(sd(vec),length(vec)))
  }
  
  vec_length = c(7,10,20,40,50,70,100,200,400,600,800,1000)
  accuracy = vector(mode = 'numeric', length = length(vec_length))
  
  for(i in 1:length(vec_length)){
    #both vectors from same distribution and are correctly identified as that
    TP = 0
    #both vectors from different distribution and are correctly identified as that
    TN = 0
    #both vectors from different distribution but not corretly identified as different
    FN = 0
    #both vectors from same distribution but not corretly identified as same
    FP = 0
    for(j in 1:100){
      #same as in the dist_test_mat
      normal = autoscale(rnorm(n=vec_length[i], mean = 0.5, sd = 0.1))
      # all other vectors arbitrary parameters and autoscaled
      normal2 = autoscale(rnorm(vec_length[i],mean = 2, sd = 1))
      expon = autoscale(rexp(vec_length[i],rate = 0.1))
      gamm = autoscale(rgamma(vec_length[i],shape=2,rate=2))
      logis = autoscale(rlogis(vec_length[i],location = 0.1,scale = 0.01))
      beta = autoscale(rbeta(vec_length[i],shape1 = 2,shape2 = 1))
      
      p1 = ks.test(normal,normal2)$p.value
      p2 = ks.test(normal,expon)$p.value
      p3 = ks.test(normal,gamm)$p.value
      p4 = ks.test(normal,logis)$p.value
      p5 = ks.test(normal,beta)$p.value
      
      if(p1>0.05){
        TP = TP + 1
      } else {
        FN = FN + 1
      }
      if(p2<=0.05 & p3 <=0.05 & p4 <=0.05 & p5 <= 0.05){
        TN = TN +1 
      } else if (p2 > 0.05 | p3 > 0.05 | p4 > 0.05 | p5 > 0.05) {
        FP = FP +1
      }
    }
    print(vec_length[i])
    accuracy[i] = (TP+TN) / (TP+TN+FN+FP)
    print(c(TP,TN,FP,FN))
    print(accuracy[i])
    print(c(p1,p2,p3,p4,p5))
  }
  plot(vec_length,accuracy,main="", ylab="Accuracy",xlab="Vector length" ,ylim=c(0,1))
}

verify_normal()


mean_sim <- 0
std_sim <- 2

lcb <- ((mean_sim - (3 * std_sim)) - 5)
ucb <- (((2 * mean_sim) + (3 * (2 * std_sim))) + 5)

u <- seq(from = lcb,
         to = ucb,
         length.out = 1e+5)
v1 <- dnorm(x = u,
            mean = mean_sim,
            sd = std_sim)
v2 <- dnorm(x = u,
            mean = 4,
            sd = 2)

matplot(x = u,
        y = cbind(v1, v2),
        type = "l",
        lty = 1,
        col = c("red", "blue"),
        xlab = "values",
        ylab = "densities",
        main = "Normal distributions with different means")
legend(x = "topright",
       legend = paste("Distbn.", 1:2, ", Mean = ", c(0,4)),
       col = c("red", "blue"),
       lty = 1)




#Problem: Autoscaling inflates the p-value?!

norm = rnorm(100)
#gamm = rgamma(100,shape=1,rate=0.01)
#gamm = autoscale(gamm)
norm2 = rnorm(100,mean=4)
plot(ecdf(norm),xlim=c(-3,6),col="red",xlab="",main="Autoscaled culmulative distribution function")
lines(ecdf(norm2),col="blue")
lines(ecdf(gamm))
ks.test(norm,norm2)

norm = autoscale(norm)
norm2 = autoscale(norm2)


normal = autoscale(rnorm(n=100, mean = 0.5, sd = 0.1))
normal2 = autoscale(rnorm(100,mean=4))
expon = autoscale(rexp(100,rate = 0.1))
gamm = autoscale(rgamma(100,shape=0.1,rate=10))
logis = autoscale(rlogis(100,location = 0.1,scale = 0.01))
beta = autoscale(rbeta(100,shape1 = 2,shape2 = 1))


par(mfrow= c(2,3))
#normal vs normal 
test = ks.test(normal,normal2)
plot(ecdf(normal),xlim=c(-3,6),col="red",xlab="",main=paste("ks.test(normal,normal2)    p1 =", round(test$p.value,2)))
lines(ecdf(normal2),col="blue")
legend("bottomright",
       col = c("blue","red"),
       pch = 18,
       legend = c("normal","normal2"))
#normal vs expon
test = ks.test(normal,expon)
plot(ecdf(normal),xlim=c(-3,6),col="red",xlab="",main=paste("ks.test(normal,exponential)    p2 =", round(test$p.value,2)))
lines(ecdf(expon),col="blue")
legend("bottomright",
       col = c("blue","red"),
       pch = 18,
       legend = c("normal","exponential"))

#normal vs gamm
test = ks.test(normal,gamm)
plot(ecdf(normal),xlim=c(-3,6),col="red",xlab="",main=paste("ks.test(normal,gamma)    p3 =", round(test$p.value,2)))
lines(ecdf(gamm),col="blue")
legend("bottomright",
       col = c("blue","red"),
       pch = 18,
       legend = c("normal","gamma"))

#normal vs logis
test = ks.test(normal,logis)
plot(ecdf(normal),xlim=c(-3,6),col="red",xlab="",main=paste("ks.test(normal,logistic)    p4 =", round(test$p.value,2)))
lines(ecdf(logis),col="blue")
legend("bottomright",
       col = c("blue","red"),
       pch = 18,
       legend = c("normal","logistic"))

#normal vs beta
test = ks.test(normal,beta)
plot(ecdf(normal),xlim=c(-3,6),col="red",xlab="",main=paste("ks.test(normal,beta)    p5 =", round(test$p.value,2)))
lines(ecdf(beta),col="blue")
legend("bottomright",
       col = c("blue","red"),
       pch = 18,
       legend = c("normal","beta"))




fact = rep(c("A","B","C","D","E","F","G","H","I","J"),10)
normal = feature_scaling(normal)
model1 = glm(normal~fact,family = "gaussian")
rss = sum(resid(model1)^2)
residuals = resid(model1)
fitted = fitted(model1)
plot(fitted,residuals,main = paste("Residuals assuming normal distribution. RSS = ", round(rss,2)))



normal = feature_scaling(normal)
model2 = glm(normal~fact,family = "quasibinomial")
residuals = resid(model2)
fitted = fitted(model2)
rss = sum(resid(model2)^2)

plot(fitted,residuals,main=paste("Residuals assuming logistic distribution. RSS = ", round(rss,2)))



normal = feature_scaling(normal)
model2 = glm(normal~fact,family = "quasibinomial")
res = resid(model2)
fit = fitted(model2)
plot(fit,res,main=paste("Model assuming beta distribution. Residual Standard Error = ", round(rse,2)))


summary(model1)

tes1 = autoscale(rgamma(1000,shape=20,rate=1))
hist(tes1)

tes2 = autoscale(rgamma(1000,shape=1,rate=20))
hist(tes2)

ks.test(tes1,tes2)










# Testing the approach. 
# Create a random vector of size 20 for each distribution. Reverse engineer the parameters and use them as input to a second
# vector of the same size and distribution. 
# Check if both vectors come from the same distribution. Should not occur less than in 95% of cases 


vec_length = c(10,20,30,40,50,60,70,80,90,100,150,200,300,400,500)
res = vector(mode = 'numeric', length = length(vec_length))
par(mfrow=c(2,3))

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

vec_length = c(40,50,60,70,80,90,100,110,120)
res = vector(mode = 'numeric', length = length(vec_length))
par(mfrow=c(2,3))

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


# logis - works only well for where the feature vector is <=40 
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
    normal = rnorm(n= 20, mean=0, sd= 1)
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


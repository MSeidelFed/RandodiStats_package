library(readxl)
library(dunn.test)
detach("package:RandoDiStats",unload=TRUE)


file = "C:/Users/Dell/Desktop/thesis_prepare/preparation/stats_no_means.xlsx"
my_data = read_excel(file)
my_data = as.data.frame(my_data)
col_names = my_data[,3]
my_data = my_data[,-c(1:3)]
rownames(my_data) = col_names
my_data = t(my_data)
row_names = rownames(my_data)
my_data = apply(my_data, 2, as.numeric)
rownames(my_data) = row_names
class(my_data[1,1])



fact1 = as.factor(c(rep("WT_D",3),rep("WT_L",3),rep("delkaiABC_D",3),rep("delkaiABC_L",3),rep("rpaA_D",3),rep("rpaA_L",3)))
res = RandoDiStats::OmicsUnivariateStats(class_comparison_mat = my_data, Factor1 = fact1, TukeyReturns = "MeanComparisons", returnObject = "OmicsTests",ReturnTukeyPlots = TRUE)
my_res = OmicsUnivariateStats(class_comparison_mat = my_data, Factor1 = fact1, TukeyReturns = "MeanComparisons", returnObject = "OmicsTests",ReturnTukeyPlots = TRUE)

my_data[,40]
test = kruskal.test(my_data[,56]~fact1)



test = pairwise.wilcox.test(my_data[,56],fact1)

test = dunn.test(my_data[,56],fact1,altp=TRUE,method="bh")

feature = my_data[,1]

data <- cbind.data.frame(value = as.numeric(feature), treatment = fact1)
model=lm(data$value ~ data$treatment)
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=0.95)
COMPARISONS <- TUKEY[["data$treatment"]][,"p adj"]

#glm with feacture~groups where regfamily=quasibinomial
test = glm(class_comparison_mat[,1]~fact1,"quasibinomial")
#these are the p-values for each group vs the control
summary(test2)$coef[,4]



test1 = lm(class_comparison_mat[,1]~fact1)
summary(test1)$coef[,4]

test2 = aov(test1)
# here again we get the p-values for each group vs the control. 
test2$coefficients



fact1 = as.factor(c(rep("delkaiABC_D",3),rep("delkaiABC_L",3),rep("rpaA_D",3),rep("rpaA_L",3),rep("WT_D",3),rep("WT_L",3)))
fact1 = factor(fact1,levels = c("delkaiABC_D","delkaiABC_L","rpaA_D","rpaA_L","WT_D","WT_L"))
anova = aov(class_comparison_mat[,26]~fact1)
test1 = TukeyHSD(anova,"fact1")
test2 = dunn.test(class_comparison_mat[,26],fact1,altp=TRUE,method="bh")

labels_tuk = names(test1[["fact1"]][,"p adj"])
labels_dunn = test2$comparisons


table = dunn.test(class_comparison_mat[,22], fact1,altp=TRUE,method = "bh")
table = cbind.data.frame(table$comparisons,table$Z,table$altP.adjusted)
table = table[order(table$`table$comparisons`),]
tes = table$`table$altP.adjusted`
names(tes) = table$`table$comparisons`
tes


test = DunnTest(class_comparison_mat[,1],fact1)
test2 = TukeyCustomized(class_comparison_mat[,1],fact1,returnObject = "MeanComparisons")



res = RandoDiStats::OmicsUnivariateStats(class_comparison_mat = my_data,
                     Factor1 = fact1,
                     Factor2 = NULL,
                     Contrast = F,
                     TukeyReturns = "MeanComparisons",
                     ReturnTukeyPlots = T,
                     TukeyPDFName = "test",
                     marginsTukey = c(6,12,3,3),
                     returnObject = "OmicsTests")









test <- c(summary(glm(my_data[,1]~fact1, "gaussian"))$coef[1:dim(summary(glm(my_data[,1]~fact1,
                                                                     "gaussian"))$coef)[1],
                                                   "Pr(>|t|)"],
          "Heteroscedastic & Non-parametric")




# take the factor and take a vector with data and try to index the vector by the factor levels.
fact1
level_1 = levels(fact1)[1]
for(j in 1:dim(class_comparison_mat)[2]){
  data = class_comparison_mat[,j]
  df = data.frame(fact1,data)
  for (i in 2:length(levels(fact1))){
    level_i = levels(fact1)[i]
    data_level1 = as.numeric(df[df==level_1,][[2]])
    print(data_level1)
    data_level_i = as.numeric(df[df==level_i,][[2]])
    print(data_level_i)
    test = wilcox.test(data_level1,data_level_i,paired=FALSE)
    print(test$p.value)
    
  }
}

fact1
level_1 = levels(fact1)[1]
for(j in 1:dim(class_comparison_mat)[2]){
  data = class_comparison_mat[,j]
  df = data.frame(fact1,data)
  for (i in 2:length(levels(fact1))){
    level_i = levels(fact1)[i]
    data_level1 = as.numeric(df[df==level_1,][[2]])
    #print(data_level1)
    data_level_i = as.numeric(df[df==level_i,][[2]])
    #print(data_level_i)
    data_list = list(data_level1,data_level_i)
    invisible({capture.output({test = dunn.test(data_list,altp = TRUE,method = "none")})})
    if(test$altP <= 0.05){
      print(test$altP.adjusted)
    }
  }
}

var = c(data_level1,data_level_i)
groups = 
  
  
invisible({capture.output({
    
  test = dunn.test(data_list,altp = TRUE,method = "none")
    
    
})})






vec = class_comparison_mat[,2]
normal = rnorm(50,0,1)
gamm = rgamma(n = 50,shape = 0.2,rate = 4)
logis = rlogis(n = 50, location = 0.1, scale = 0.01)
binom = rbinom(n = 50, size = 1, prob = 0.5)
pois = rpois(n = 50, lambda = 0.01)
exp = rexp(n = 50, rate = 20)
rbet1 = rbeta(50,shape1 = 0.3,shape2 = 1)

vecs = list(gamm,logis,binom,pois,rbet1,exp,normal)
for(i in 1:7){
  print(ks.test(vec,vecs[[i]])$p.value)
  
}

library(fitdistrplus)

vec = rgamma(100,shape= 0.1,rate = 10)
plot(vec,main="Randomly generated Gamma vector")
plot(density(vec),main = "Density of the vector")


colours <- c(rep("blue", 1000),
             rep("red", 1000),
             rep("grey", 1000),
             rep("yellow", 1000),
             rep("pink", 1000))



#test1 <- matrix(data = NA, nrow = 100, ncol = 1000)
#for (i in 1:1000) {
#  x <- rgamma(n = 100,shape = 0.1,rate = 10)
#  test1[,i] <- x
#}

plot(y = 6, x = 2,ylim = c(25,0), xlim = c(0,10),xlab="Square of Skewness",ylab="Kurtosis", col = colours[1])

SKEW <- vector(mode = "list", length = 1000)
KURT <- vector(mode = "list", length = 1000)

for (i in 1:1000){
  
  SKEW[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i])), graph = F)$skewness^2
  
  KURT[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i])), graph = F)$kurtosis
  
}

plot(y = KURT , x = SKEW, ylim = c(25,0), xlim = c(0,10),xlab="Square of Skewness",ylab="Kurtosis", col = colours[1])

legend("topright",
       col = c("blue","red","grey","yellow","purple"),
       pch = 18,
       legend = c("gamma","logis","beta","normal","exponential"))

SKEW2 <- vector(mode = "list", length = 1000)
KURT2 <- vector(mode = "list", length = 1000)

for (i in 1:1000) {
  
  SKEW2[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i+1000])), graph = F)$skewness^2
  
  KURT2[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i+1000])), graph = F)$kurtosis
  
}
skew_new = c(SKEW,SKEW2)
kurt_new = c(KURT,KURT2)

plot(y = kurt_new , x = skew_new, ylim = c(25,0), xlim = c(0,10),xlab="Square of Skewness",ylab="Kurtosis", col = c(rep("blue", 1000),
                                                                                                                    rep("red", 1000)))

SKEW3 <- vector(mode = "list", length = 1000)
KURT3 <- vector(mode = "list", length = 1000)

for (i in 1:1000) {
  
  SKEW3[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i+2000])), graph = F)$skewness^2
  
  KURT3[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i+2000])), graph = F)$kurtosis
  
}
skew_new = c(SKEW,SKEW2,SKEW3)
kurt_new = c(KURT,KURT2,KURT3)

plot(y = kurt_new , x = skew_new, ylim = c(25,0), xlim = c(0,10),xlab="Square of Skewness",ylab="Kurtosis", col = c(rep("blue", 1000),
                                                                                                                    rep("red", 1000),
                                                                                                                    rep("grey",1000)))

SKEW4 <- vector(mode = "list", length = 1000)
KURT4 <- vector(mode = "list", length = 1000)

for (i in 1:1000) {
  
  SKEW4[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i+3000])), graph = F)$skewness^2
  
  KURT4[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i+3000])), graph = F)$kurtosis
  
}
skew_new = c(SKEW,SKEW2,SKEW3,SKEW4)
kurt_new = c(KURT,KURT2,KURT3,KURT4)

plot(y = kurt_new , x = skew_new, ylim = c(25,0), xlim = c(0,10),xlab="Square of Skewness",ylab="Kurtosis", col = c(rep("blue", 1000),
                                                                                                                    rep("red", 1000),
                                                                                                                    rep("grey",1000),
                                                                                                                    rep("yellow",1000)))

SKEW5 <- vector(mode = "list", length = 1000)
KURT5 <- vector(mode = "list", length = 1000)

for (i in 1:1000) {
  
  SKEW5[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i+6000])), graph = F)$skewness^2
  
  KURT5[[i]]  <- fitdistrplus::descdist(as.numeric(na.omit(Distribution_test_mat[,i+6000])), graph = F)$kurtosis
  
}
skew_new = c(SKEW,SKEW2,SKEW3,SKEW4,SKEW5)
kurt_new = c(KURT,KURT2,KURT3,KURT4,KURT5)

plot(y = kurt_new , x = skew_new, ylim = c(25,0), xlim = c(0,10),xlab="Square of Skewness",ylab="Kurtosis", col = c(rep("blue", 1000),
                                                                                                                    rep("red", 1000),
                                                                                                                    rep("grey",1000),
                                                                                                                    rep("yellow",1000),
                                                                                                                    rep("purple",1000)))
legend("topright",
       col = c("blue","red","grey","yellow","purple"),
       pch = 18,
       legend = c("gamma","logis","beta","normal","exponential"))


points(0.5,7,col="black",pch=19,cex=2)
skew_new = c(SKEW,SKEW2,SKEW3,SKEW4,SKEW5,0.5)
kurt_new = c(KURT,KURT2,KURT3,KURT4,KURT5,7)

plot(y = kurt_new , x = skew_new, ylim = c(25,0), xlim = c(0,10),xlab="Square of Skewness",ylab="Kurtosis", col = c(rep("blue", 1000),
                                                                                                                    rep("red", 1000),
                                                                                                                    rep("grey",1000),
                                                                                                                    rep("yellow",1000),
                                                                                                                    rep("purple",1000),
                                                                                                                    "black"))
legend("topright",
       col = c("blue","red","grey","yellow","purple","black"),
       pch = 18,
       legend = c("gamma","logis","beta","normal","exponential","metabolite"))


skew_new = c(mean(unlist(SKEW)),mean(unlist(SKEW2)),mean(unlist(SKEW3)),mean(unlist(SKEW4)),mean(unlist(SKEW5)),0.5)
kurt_new = c(mean(unlist(KURT)),mean(unlist(KURT2)),mean(unlist(KURT3)),mean(unlist(KURT4)),mean(unlist(KURT5)),7)

plot(y = kurt_new , x = skew_new, ylim = c(25,0), xlim = c(0,10),xlab="Square of Skewness",ylab="Kurtosis",pch=c(19,19,19,19,19,4),cex=2, col = c("blue","red","grey",
                                                                                                                    "yellow","purple",
                                                                                                                    "black"))

legend("topright",
       col = c("blue","red","grey","yellow","purple","black"),
       pch = 18,
       legend = c("gamma","logis","beta","normal","exponential","metabolite"))











model3 = glm(formula = class_comparison_mat[, 1] ~ fact1, family = quasibinomial)
model4 = glm(formula = class_comparison_mat[, 4] ~ fact1, family = quasibinomial)
model5 = glm(formula = class_comparison_mat[, 2] ~ fact1, family = quasibinomial)
model6 = glm(formula = class_comparison_mat[, 3] ~ fact1, family = quasibinomial)
model.sel(model3, model4,model5,model6,
          rank = QAIC,
          rank.args = list(chat = deviance(model3) / df.residual(model3)))


(chat <- deviance(model3) / df.residual(model3))


model4 = glm(formula = class_comparison_mat[, 9] ~ fact1, family = "quasibinomial")
QAIC_model4 = QAIC(model4,chat=chat,k=2)








# CODE for multi-glm where duplicates are removed!
fact1 = Factor1
for(j in 1:length(levels(Factor1))-2){
  
  print(levels(Factor1))
  test <- summary(glm(vec~Factor1, "gaussian"))$coef[1:dim(summary(glm(vec~Factor1,
                                                                       "gaussian"))$coef)[1],
                                                     "Pr(>|t|)"]
  print(test)
  Factor1 = factor(droplevels(Factor1,exclude = levels(Factor1)[1]))
}
Factor1 = fact1











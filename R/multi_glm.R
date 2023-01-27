
vec = class_comparison_mat[,2]
fact1 = as.factor(c(rep("WT_D",3),rep("WT_L",3),rep("delkaiABC_D",3),rep("delkaiABC_L",3),rep("rpaA_D",3),rep("rpaA_L",3)))
test <- c(summary(glm(vec~fact1, "gaussian"))$coef[1:dim(summary(glm(vec~fact1,
                                                                     "gaussian"))$coef)[1],
                                                   "Pr(>|t|)"],
          "Homoscedastic & Non-parametric")


test_out <- matrix(NA,
                   nrow = dim(class_comparison_mat)[2],
                   ncol = 1 + length(levels(fact1))*length(c(glm(Formula)[["coefficients"]])))
levels(fact1)
#test_vec = vector(mode="numeric",length = ncol(test_out))
test_vec = numeric()
fact = fact1
for (i in 1:length(levels(fact1))){
  fact1 = relevel(fact1,levels(fact1)[i])
  test <- summary(glm(vec~fact1, "gaussian"))$coef[1:dim(summary(glm(vec~fact1,
                                                                    "gaussian"))$coef)[1],
                                                  "Pr(>|t|)"]
  print(levels(fact1))
  #append(test_vec,test,after = length(test_vec))
  test_vec = c(test_vec,test)
  fact1 = fact
}

test_out[i,] = test_vec


levels(fact1)
#test_vec = vector(mode="numeric",length = ncol(test_out))
test_vec = numeric()
for (i in 1:length(levels(fact1))){
  fact1 = relevel(fact1,levels(fact1)[i])
  test <- c(Dunn.Test(variable=vec,factor=fact1,method="none")[1:length(levels(fact1))],
            "Non-parametric")
  print(fact1)
  #append(test_vec,test,after = length(test_vec))
  test_vec = c(test_vec,test)
}







#Procedure for multi-glm . This works, in the omicsunivariate it doesnt!
test_vec = numeric()
fact = fact1
for (i in 1:length(levels(fact1))){
  fact1 = relevel(fact1,levels(fact1)[i])
  test <- c(summary(glm(vec~fact1, "gaussian"))$coef[1:dim(summary(glm(vec~fact1,
                                                                       "gaussian"))$coef)[1],
                                                     "Pr(>|t|)"])
  test_vec = c(test_vec,test)
  fact1 = fact
  print(test)
}
test_vec = c(test_vec,"Heteroscedastic & Non-parametric")
test_out[1,] = test_vec
colnames(test_out) = names(test_vec)



#Procedure for non-parametric multi-glm
test_vec = numeric()
for (i in 1:length(levels(fact1))){
  control = levels(fact1)[i]
  test <- Dunn.Test(vec,fact1,method="none")
  res = which(str_extract(names(test), control) == control)
  res = test[res]
  test = c(0,res)
  print(test_vec)
  test_vec = c(test_vec,test)
  
}
test_vec = c(test_vec,"Non-parametric")


test_res  = Dunn.Test(vec,fact1)
which(str_extract(names(test_res), "delkaiABC_L") == "delkaiABC_L")
test_res



model1 <- glm(vec~fact1, "gaussian")
test <- summary(glm(vec~fact1, "gaussian"))$coef[1:dim(summary(glm(vec~fact1,
                                                                     "gaussian"))$coef)[1],
                                                   "Pr(>|t|)"]
for(i in 1:length(test)){
  test[i] = p.adjust(test[i], method = "fdr")
}
comparisons = emmeans(model1, pairwise ~ fact1, adjust ="fdr")
summary(comparisons$contrasts)$p.value


#Multi-GLM that was not needed!
if (regfamily[i] != "non-parametric" & lawstat::levene.test(y = NA_free_var,
                                                            group = NA_free_factor,
                                                            location = "median",
                                                            bootstrap = F)[["p.value"]] > 0.05 &
    shapiro.test(x = class_comparison_mat[,i])[["p.value"]] > 0.05) {
  
  test_vec = numeric()
  fact = Factor1
  for (j in 1:length(levels(Factor1))){
    Factor1 = relevel(Factor1,levels(Factor1)[j])
    test <- c(summary(lm(Formula))$coef[1:dim(summary(glm(Formula,
                                                          gaussian))$coef)[1],
                                        "Pr(>|t|)"])
    test_vec = c(test_vec,test)
    Factor1 = fact
  }
  test_vec = c(test_vec,"Homoscedastic & Parametric")
  
  # Levene and Shapiro test. Here, heteroscedastic and normally distributed  
} else if (regfamily[i] != "non-parametric" & lawstat::levene.test(y = NA_free_var,
                                                                   group = NA_free_factor,
                                                                   location = "median",
                                                                   bootstrap = F)[["p.value"]] < 0.05 &
           shapiro.test(x = class_comparison_mat[,i])[["p.value"]] > 0.05) {
  
  test_vec = numeric()
  fact = Factor1
  for (j in 1:length(levels(Factor1))){
    Factor1 = relevel(Factor1,levels(Factor1)[j])
    test <- c(summary(lm(Formula))$coef[1:dim(summary(glm(Formula,
                                                          gaussian))$coef)[1],
                                        "Pr(>|t|)"])
    test_vec = c(test_vec,test)
    Factor1 = fact
  }
  test_vec = c(test_vec,"Heteroscedastic & Parametric")
  
  # Levene and Shapiro test. Here, homoscedastic and not normally distributed
  
} else if (regfamily[i] != "non-parametric" & lawstat::levene.test(y = NA_free_var,
                                                                   group = NA_free_factor,
                                                                   location = "median",
                                                                   bootstrap = F)[["p.value"]] > 0.05 &
           shapiro.test(x = class_comparison_mat[,i])[["p.value"]] < 0.05) {
  
  test_vec = numeric()
  fact = Factor1
  for (j in 1:length(levels(Factor1))){
    Factor1 = relevel(Factor1,levels(Factor1)[j])
    test <- c(summary(glm(Formula, regfamily[i]))$coef[1:dim(summary(glm(Formula,
                                                                         regfamily[i]))$coef)[1],
                                                       "Pr(>|t|)"])
    test_vec = c(test_vec,test)
    Factor1 = fact
  }
  test_vec = c(test_vec,"Homoscedastic & Non-parametric")
  
  # Levene and Shapiro test. Here, heteroscedastic and not normally distributed  
} else if (regfamily[i] !="non-parametric" & lawstat::levene.test(y = NA_free_var,
                                                                  group = NA_free_factor,
                                                                  location = "median",
                                                                  bootstrap = F)[["p.value"]] < 0.05 &
           shapiro.test(x = class_comparison_mat[,i])[["p.value"]] < 0.05) {
  
  test_vec = numeric()
  fact = Factor1
  for (j in 1:length(levels(Factor1))){
    Factor1 = relevel(Factor1,levels(Factor1)[j])
    test <- c(summary(glm(Formula, regfamily[i]))$coef[1:dim(summary(glm(Formula,
                                                                         regfamily[i]))$coef)[1],
                                                       "Pr(>|t|)"])
    test_vec = c(test_vec,test)
    Factor1 = fact
  }
  test_vec = c(test_vec,"Heteroscedastic & Non-parametric")
  
} else if (regfamily[i] == "non-parametric") {
  
  test_vec = numeric()
  for (j in 1:length(levels(Factor1))){
    control = levels(Factor1)[j]
    test <- Dunn.Test(vec,Factor1,method="none")
    res = which(str_extract(names(test), control) == control)
    res = test[res]
    test = c(0,res)
    test_vec = c(test_vec,test)
  }
  test_vec = c(test_vec,"Non-parametric")
}

test_out[i,] <- test_vec
}
col_names = character()
fact = Factor1
for (j in 1:length(levels(Factor1))){
  Factor1 = relevel(Factor1,levels(Factor1)[j])
  test <- c(summary(lm(Formula))$coef[1:dim(summary(glm(Formula,
                                                        gaussian))$coef)[1],
                                      "Pr(>|t|)"])
  col_names = c(col_names,names(test))
  Factor1 = fact
}
col_names = c(col_names,"Assumptions_tested")
colnames(test_out) <- col_names


model1 = glm(vec~fact1, "quasibinomial")
comparisons = emmeans(model1, pairwise ~ fact1, adjust ="fdr")
#This has to do the plots like the Tukey Call
return_letters = summary(comparisons$contrasts)$p.value
names(return_letters) = summary(comparisons$contrasts)$contrast
labs <- data.frame(multcompView::multcompLetters(return_letters)['Letters'])

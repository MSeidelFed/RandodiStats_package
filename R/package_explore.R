library(RandoDiStats)
library(readxl)



file = "C:/Users/Dell/Desktop/thesis_prepare/preparation/stats_no_means.xlsx"
my_data = read_excel(file)
my_data = as.data.frame(my_data)
row_names = my_data[,3]
my_data = my_data[,-c(1:3)]
rownames(my_data) = row_names
my_data = t(my_data)
my_data = as.matrix(my_data)
row_names = rownames(my_data)
my_data <- apply(my_data, 2, as.numeric)
rownames(my_data) = row_names
class(my_data[1,1])


my_data[which(is.na(my_data))] <- abs(rnorm(length(which(is.na(my_data))), 0.0000001, 0000001))



#Factor1_eg <- as.factor(c(rep("RED", 200), rep("GREEN", 200), rep("BLACK", 200),rep("WHITE", 200), rep("YELLOW", 200)))
#test_OUS <- OmicsUnivariateStats(Factor1 = Factor1_eg, TukeyReturns = "MeanComparisons",returnObject = "OmicsTests")

fact1 = as.factor(c(rep("WT_D",3),rep(" WT_L",3),rep("delkaiABC_D",3),rep("delkaiABC_L",3),rep("rpaA_D",3),rep("rpaA_L",3)))
res = OmicsUnivariateStats(class_comparison_mat = my_data, Factor1 = fact1, TukeyReturns = "MeanComparisons", returnObject = "OmicsTests",ReturnTukeyPlots = FALSE)
dim(res)



sum(is.na(my_data[4:6,]))

# Load ggplot2
library(ggplot2)


# Based on counts of the first dataset. 
Distributions = c("gamma","logis","beta","normal", "exponential", "non-parametric")
value = c(6,9,5,29,5,2)
data = data.frame(Distributions,value)
colors = c("yellow","pink","blue","red","purple","darkgreen")

# Basic piechart
p = ggplot(data, aes(x="", y=value, fill=Distributions, color=colors)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels 
  p + ggtitle("") + scale_fill_manual(values = colors) +
    theme(legend.key.size = unit(1, 'cm'),legend.text = element_text(size=15),legend.title = element_text(size=15))
  

  
  
  

Distributions = c("gamma","logis","beta","normal", "exponential", "non-parametric")
value = c(12,13,7,22,14,46)
data = data.frame(Distributions,value)

# Basic piechart
p = ggplot(data, aes(x="", y=value, fill=Distributions)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels 
p + ggtitle("") + scale_fill_manual(values = colors) +
  theme(legend.key.size = unit(1, 'cm'),legend.text = element_text(size=15),legend.title = element_text(size=15))



Distributions = c("No changes", "Significant changes")
value = c(114,0)
data = data.frame(Distributions,value)

# Basic piechart
p = ggplot(data, aes(x="", y=value, fill=Distributions)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels 
p  + scale_fill_discrete(name = "")


Distributions = c("No changes", "Significant changes")
value = c(107,7)
data = data.frame(Distributions,value)
# Basic piechart
p = ggplot(data, aes(x="", y=value, fill=Distributions)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels 
p  + scale_fill_discrete(name = "")



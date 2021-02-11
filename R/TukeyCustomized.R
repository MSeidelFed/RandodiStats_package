#' A Tukey HSD visual representation
#'
#' This function allows you to rationalize the clustering method selected.
#' @param variable is a numeric vector outlining a response variable.
#' @param factor is a factor type of vector of the same lenght as variable.
#' @param conf.level Defaults to 0.95 CI.
#' @param MainTitle allows to insert a title for the returned plot.
#' @keywords class discovery, TukeyHSD
#' @export




TukeyCustomized <- function(variable,
                            factor,
                            conf.level = 0.95,
                            MainTitle = "") {
  
  
  if (class(variable) == "numeric" & class(factor) == "factor") {
    
    data <- cbind.data.frame(value = as.numeric(variable), treatment = as.factor(factor))
    
  } else {
    
    stop("Please check the class of the input data (variable must be numeric and factor must be factor)")
    
  }
  
  
  # What is the effect of the treatment on the value ?
  model=lm(data$value ~ data$treatment)
  ANOVA=aov(model)
  
  # Tukey test to study each pair of treatment :
  TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=conf.level)
  
  # Tukey test representation :
  plot(TUKEY , las=1 , col="brown" )
  
  
  # I need to group the treatments that are not different from each other together.
  generate_label_df <- function(TUKEY, variable){
    
    # Extract labels and factor levels from Tukey post-hoc 
    Tukey.levels <- TUKEY[[variable]][,4]
    Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
    
    #I need to put the labels in the same order as in the boxplot :
    Tukey.labels$treatment=rownames(Tukey.labels)
    Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
    return(Tukey.labels)
  }
  
  # Apply the function on my dataset
  LABELS=generate_label_df(TUKEY , "data$treatment")
  
  
  # A panel of colors to draw each group with the same color :
  my_colors=c( rgb(143,199,74,maxColorValue = 255),
               rgb(242,104,34,maxColorValue = 255),
               rgb(111,145,202,maxColorValue = 255),
               rgb(254,188,18,maxColorValue = 255),
               rgb(74,132,54,maxColorValue = 255),
               rgb(236,33,39,maxColorValue = 255),
               rgb(165,103,40,maxColorValue = 255))
  
  # Draw the basic boxplot
  a=boxplot(data$value ~ data$treatment,
            ylim = c(min(data$value), 1.1*max(data$value)),
            col = my_colors[as.numeric(LABELS[,1])],
            ylab = NULL,
            xlab = NULL,
            main = MainTitle, las = 2, cex.axis = 1) # automatic axis = ylim=c(min(data$value) , 1.1*max(data$value))
  
  # I want to write the letter over each box. Over is how high I want to write it.
  over=0.1*max( a$stats[nrow(a$stats),] )
  
  #Add the labels
  text( c(1:nlevels(data$treatment)) , a$stats[nrow(a$stats),]+over ,
        LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )
  
  
  
}
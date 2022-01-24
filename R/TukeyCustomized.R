#' A Tukey HSD visual representation
#'
#' This function allows you to rationalize the clustering method selected.
#' @param variable is a numeric vector outlining a response variable.
#' @param factor is a factor type of vector of the same length as variable.
#' @param conf.level Defaults to 0.95 CI.
#' @param MainTitle allows to insert a title for the returned plot.
#' @param ylabTukeys defaults to NULL. Character vector that defines the text on y-axes from Tukey HSD plots.
#' @param xlabTukeys defaults to NULL. Character vector that defines the text on x-axes from Tukey HSD plots.
#' @keywords class discovery, TukeyHSD
#' @export


TukeyCustomized <- function(variable,
                            factor,
                            conf.level = 0.95,
                            MainTitle = "",
                            returnObject = c("Letters", "MeanComparisons"),
                            ylabTukeys = NULL,
                            xlabTukeys = NULL) {
  
  
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
  
  COMPARISONS <- TUKEY[["data$treatment"]][,"p adj"]
  
  abscissa <- c(min(c(TUKEY$`data$treatment`[,"lwr"], 0)), max(c(TUKEY$`data$treatment`[,"upr"], 0)))
  
  # Tukey test representation :
  plot(TUKEY, las=1 , col="brown", xlim = abscissa)
  
  
  # I need to group the treatments that are not different from each other together.
  generate_label_df <- function(TUKEY){
    
    # Extract labels and factor levels from Tukey post-hoc
    Tukey.levels <- TUKEY[["data$treatment"]][,4]
    
    if (length(Tukey.levels) == 1 & TUKEY[["data$treatment"]][,4][1] < 0.05) {
      
      Tukey.labels <- as.data.frame(cbind(Letters = c("a", "b")))
      
      rownames(Tukey.labels) <- strsplit(rownames(TUKEY[["data$treatment"]]), "-")[[1]]
      
    } else if (length(Tukey.levels) == 1 & TUKEY[["data$treatment"]][,4][1] > 0.05) {
      
      Tukey.labels <- as.data.frame(cbind(Letters = c("a", "a")))
      
      rownames(Tukey.labels) <- strsplit(rownames(TUKEY[["data$treatment"]]), "-")[[1]]
      
    } else {
      
      Tukey.labels <- data.frame(multcompView::multcompLetters(Tukey.levels)['Letters'])
      
    }
    
    #I need to put the labels in the same order as in the boxplot :
    Tukey.labels$treatment=rownames(Tukey.labels)
    Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
    return(Tukey.labels)
  }
  
  # Apply the function on my dataset
  LABELS=generate_label_df(TUKEY = TUKEY)
  
  
  # A panel of colors to draw each group with the same color :
  my_colors2 = cbind(Colors = rainbow(LABELS[,"Letters"]))
  rownames(my_colors2) <- as.character(unique(LABELS[,"Letters"]))
  
  # integrating the colors into the LABELS object
  
   LABELS = cbind(LABELS, Colors = my_colors2[as.character(LABELS$Letters),])
  
  # Reordering data$treatment with the LABELS row order to have the treatments with the correct colors
  
  data$treatment <- factor(data$treatment, levels = rownames(LABELS))
  
  
  if (max(na.omit(data$value)) > 0) {
    
    ylims = c(min(na.omit(data$value)), 1.1*max(na.omit(data$value)))
    
  } else {
    
    ylims = c(1.1*min(na.omit(data$value)), max(na.omit(data$value)))
    
  }
  
  # Draw the basic boxplot
  a=boxplot(data$value ~ data$treatment,
            ylim = ylims,
            col = as.character(LABELS$Colors),
            ylab = ylabTukeys,
            xlab = xlabTukeys,
            main = MainTitle, las = 2, cex.axis = 1)
  
  # I want to write the letter over each box. Over is how high I want to write it.
  over=0.1*max( a$stats[nrow(a$stats),] )
  
  #Add the labels
  text(c(1:nlevels(data$treatment)), a$stats[nrow(a$stats),]+over,
       LABELS[,1], col = as.character(LABELS$Colors))
  
  if(returnObject == "Letters") {
    
    return(LABELS)
    
  } else if(returnObject == "MeanComparisons") {
    
    return(COMPARISONS)
    
  } else {
    
    stop("ERROR: Please input a valid returnObject in the function call")
    
  }
  
}

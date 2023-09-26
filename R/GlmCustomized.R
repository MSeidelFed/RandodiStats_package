
GlmCustomized <- function(variable,
                            factor,
                            mode = c("lm","non-parametric","glm"),
                            regfamily = "",
                            conf.level = 0.95,
                            MainTitle = "",
                            returnObject = c("Letters", "MeanComparisons"),
                            ylabTukeys = NULL,
                            xlabTukeys = NULL) {
  
  generate_label_df <- function(COMPARISONS){
    
    # Extract labels and factor levels from Tukey post-hoc
    Tukey.levels <- COMPARISONS
    
    if (length(Tukey.levels) == 1 & Tukey.levels[1] < 0.05) {
      
      Tukey.labels <- as.data.frame(cbind(Letters = c("a", "b")))
      
      rownames(Tukey.labels) <- strsplit(names(COMPARISONS), "-")[[1]]
      
    } else if (length(Tukey.levels) == 1 & COMPARISONS[1] > 0.05) {
      
      Tukey.labels <- as.data.frame(cbind(Letters = c("a", "a")))
      
      rownames(Tukey.labels) <- strsplit(names(COMPARISONS), "-")[[1]]
      
    } else {
      
      Tukey.labels <- data.frame(multcompView::multcompLetters(Tukey.levels)['Letters'])
      
    }
    
    #I need to put the labels in the same order as in the boxplot :
    Tukey.labels$treatment=rownames(Tukey.labels)
    Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
    return(Tukey.labels)
  }
  
  
  if (class(variable) == "numeric" & class(factor) == "factor") {
    
    # put together the variable and the facter as data.frame
    data <- cbind.data.frame(value = as.numeric(variable), treatment = as.factor(factor))
    
  } else {
    
    stop("Please check the class of the input data (variable must be numeric and factor must be factor)")
    
  }
  
  # I need the colnames to match exactly the Tukey ones
  model=lm(data$value ~ data$treatment)
  ANOVA=aov(model)
  TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=conf.level)
  COMPARISONS <- TUKEY[["data$treatment"]][,"p adj"]
  names_comp = names(COMPARISONS)
  
  if (mode == "lm") {
    
    abscissa <- c(min(c(TUKEY$`data$treatment`[,"lwr"], 0)), max(c(TUKEY$`data$treatment`[,"upr"], 0)))
    # Tukey test representation :
    plot(TUKEY, las=1 , col="brown", xlim = abscissa)

  } else if (mode == "non-parametric") {
    
    COMPARISONS = DunnTest(variable = data$value, factor = data$treatment, method = "fdr")
    names(COMPARISONS) = names_comp
    
  } else if (mode == "glm") {
    
    if (regfamily == "quasibinomial") {
      regfamily = quasibinomial(link="identity")
      fact = as.factor(data$treatment)
    } else if (regfamily == "Gamma") {
      regfamily = Gamma(link="log")
      fact = as.factor(data$treatment)
    } else {
      fact = as.factor(data$treatment)
    }
    #here a change is required .. Gamma has to be rescaled to former values
    COMPARISONS = glm(data$value~fact, family = regfamily$family) %>% emmeans::emmeans(pairwise ~ fact, adjust = "Tukey")
    COMPARISONS = summary(COMPARISONS$contrasts)$p.value
    names(COMPARISONS) = names_comp
    
  } else {
    stop("ERROR: Please call function with valid mode")
  }
  
  LABELS=generate_label_df(COMPARISONS = COMPARISONS)
  # A panel of colors to draw each group with the same color :
  my_colors2 = cbind(Colors = rainbow(n = length(as.character(unique(LABELS[,"Letters"])))))
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
       LABELS[,1], cex=1, col = as.character(LABELS$Colors))
  
  if(returnObject == "Letters") {
    
    return(LABELS)
    
  } else if(returnObject == "MeanComparisons") {
    
    return(COMPARISONS)
    
  } else {
    
    stop("ERROR: Please input a valid returnObject in the function call")
    
  }
  
}

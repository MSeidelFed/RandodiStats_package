#' A clustering function
#'
#' This function allows you to rationalize the clustering method selected.
#' @param inMat Defaults to a randomly generated matrix from the distribution_test_mat function.
#' @param Km must be a result object from the function KmeansPlus().
#' @param returnValuesPlot if TRUE, returns all relevant PCA stats to manually build the plots.
#' @param returnTreatmentValues if TRUE, returns treatment values, if FALSE, returns features values.
#' @keywords class discovery, PCA, prcomp
#' @export
#' @examples
#' test <- PlusPCA()



PlusPCA <- function(inMat = distribution_test_mat(),
                    Km = NULL,
                    returnValuesPlot = T,
                    returnTreatmentValues = T,
                    pamClust = T,
                    Kpam) {
  
  
  in_data_imputed <- inMat
  
  # using Km object
  
  if (!is.null(Km)){
    
    out_df <- matrix(NA, nrow = 0, ncol = 3)
    
    for (i in 1:dim(Km)[2]) {
      
      runner_col <- rand_color(1)
      
      out_df <- rbind(out_df, cbind(rep(paste0("ClustNo.", i),
                                        length(na.omit(Km[,i]))),
                                    rep(runner_col,
                                        length(na.omit(Km[,i]))),
                                    as.character(na.omit(Km[,i]))))
      
    }
    
    ColorVector = out_df[,1]
    
    in_data_imputed <- cbind(in_data_imputed, ColorVector)
    
    colnames(in_data_imputed)[dim(in_data_imputed)[2]] <- "Clusters"
  }
    
  
  # PCA
  
  ## on features
  
  FeatPrComp <- prcomp(as.matrix(inMat))
  
  pdf("PCA_all.pdf")
  
  if (pamClust == T) {
    
    print(autoplot(pam(apply(in_data_imputed[,-c(dim(in_data_imputed)[2])], 2, as.numeric), Kpam), frame = TRUE, frame.type = 'norm'))
    
  } else {
    
    print(autoplot(FeatPrComp, data = in_data_imputed, colour = 'Clusters',
                   loadings = TRUE, loadings.colour = "yellow", loadings.label = T,
                   loadings.label.size = 3)) 
    
  }
  
  plot(pcaMethods::pca(as.matrix(inMat), nPcs = 10), main = "PCA on features")
  
  PCA_features <- pcaMethods::pca(as.matrix(inMat), nPcs = 10)
  
  ## on samples
  
  SamPrComp <- prcomp(as.matrix(t(inMat)))
  
  print(autoplot(SamPrComp, label = T, label.size = 3, shape = F, loadings = T,
           loadings.colour = "yellow",
           loadings.label = F,loadings.label.size = 3, loadings.label.colour = "grey")) 

  plot(pcaMethods::pca(as.matrix(t(inMat)), nPcs = 10), main = "PCA on treatments")
  
  dev.off()
  
  PCA_treatments <- pcaMethods::pca(as.matrix(t(inMat)), nPcs = 10)
  
  ## return manual values for plot building
  
  if(returnValuesPlot == T) {
    
    if(returnTreatmentValues == T) {
      
      retunrMat <- as.matrix(t(inMat))
      
    } else {
      
      retunrMat <- as.matrix(inMat)
      
    }
    
    ### PCA treatments
    
    PCA_stored <- prcomp(retunrMat)
    
    write.table(PCA_stored[["sdev"]], "sdev.txt",
                sep = "\t", col.names = T, row.names = T, quote = F)
    
    write.table(PCA_stored[["rotation"]], "rotation.txt",
                sep = "\t", col.names = T, row.names = T, quote = F)
    
    write.table(PCA_stored[["center"]], "center.txt",
                sep = "\t", col.names = T, row.names = T, quote = F)
    
    write.table(PCA_stored[["x"]], "coordsPCs.txt",
                sep = "\t", col.names = T, row.names = T, quote = F)
    
  }
  
  return(list(Treatments = PCA_treatments,
              Features = PCA_features))
  
}

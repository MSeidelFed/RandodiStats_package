#' A k-means clustering function
#'
#' This function allows you to apply k-means clustering algorithm, return a graphical heatmap output, group change trends and features belonging to each k-group.
#' @param DataDir Defaults to a randomly generated matrix from the distribution_test_mat function.
#' @param returnClustMeanPlots Defaults to True, allows to return mean plots for each k group.
#' @param returnClustDendrogram Defaults to True, allows to generate a PDF containing the dendrogram with the k-means partition.
#' @param returnClustFeatures Defaults to True, allows to return the names from features inside the k-groups.
#' @param k Defaults to 5, is the number of k-partitions.
#' @param nboots Defaults to 100, is the number of iterations in the k-means clustering to get a consensus.
#' @param AU_p_value Defaults to 0.95 or in other words the classic P value 0.05 treshold to define a significant cluster.
#' @param ClustMethod Defaults to average, inherits from the ComplexHeatmap package.
#' @param ClustDist Defaults to pearson, inherits from the ComplexHeatmap package.
#' @keywords class discovery, clustering, ComplexHeatmap
#' @export
#' @examples
#' test <- KmeansPlus()




KmeansPlus <- function(DataDir = distribution_test_mat(),
                       returnClustMeanPlots = T,
                       returnClustDendrogram = NULL,
                       returnClustFeatures = T,
                       k = 5,
                       n_boot = 100,
                       ClustMethod = "average",
                       ClustDist = "pearson") {

  ## necessary functions

  list2df <- function(x)
  {
    MAX.LEN <- max(sapply(x, length), na.rm = TRUE)
    DF <- data.frame(lapply(x, function(x) c(x, rep(NA, MAX.LEN - length(x)))))
    colnames(DF) <- paste("V", seq(ncol(DF)), sep = "")
    DF
  }

  ## data

  if (class(DataDir) == "matrix") {

    data <- DataDir

    if (is.null(colnames(DataDir))) {

      ## two artificial factors in case a trial matrix without colnames is inputted

      colnames(data) <- c(paste0(rep("X", ((dim(data)[2])/2)), ".", c(1:((dim(data)[2])/2))),
                          paste0(rep("Y", ((dim(data)[2])/2)), ".", c(1:((dim(data)[2])/2))))

    }

    if (is.null(rownames(DataDir))) {

      ## two artificial factors in case a trial matrix without colnames is inputted

      rownames(data) <- c(paste0(rep("A", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))),
                          paste0(rep("B", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))),
                          paste0(rep("C", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))),
                          paste0(rep("D", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))),
                          paste0(rep("E", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))),
                          paste0(rep("F", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))),
                          paste0(rep("G", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))),
                          paste0(rep("H", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))),
                          paste0(rep("E", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))),
                          paste0(rep("F", ((dim(data)[1])/10)), ".", c(1:((dim(data)[1])/10))))

    }



  } else if (class(DataDir) == "character") {

    data <- read.table(file = DataDir, header = T, sep = "\t", row.names = 1)

  } else {

    cat("You can only feed me either a matrix or a directory path to one tab delimited matrix file")

  }

  data_mat <- as.matrix(data)

  ## main

  ### treatments must be well defined in the first row as column identifiers

  Treatment_vector <- list2df(strsplit(colnames(data), split = "\\."))[1,]

  type = gsub("s\\d+_", "", lapply(Treatment_vector, FUN = as.character))

  ha = HeatmapAnnotation(df = data.frame(type = type))

  ## test maximum values in your data

  cat("\n")
  print(summary(melt(as.matrix(data))[,"value"]))

  cat("\n")
  return_decision <- readline(prompt="Do you want me to scale matrix ? (Y/N): ")
  cat("\n")

  if (return_decision == "Y") {

    scaled_mat <- (data_mat - rowMeans(data_mat)) / rowSds(data_mat)

    col = c(-2.5, -1, 0, 1, 2.5)

    ## test AGAIN maximum values in your data

    "\n"
    cat("Scaled data summary: ", "\n")

    print(summary(melt(scaled_mat)[,"value"]))

    ## Data should be now between with a lower interval.

  } else {

    scaled_mat = data_mat

    col = as.numeric(summary(melt(as.matrix(data))[,"value"])[-3])

  }

  cat("\n")
  cat("Iterating Kmean clustering to get consensus...", "\n")
  cat("\n")

  rowOrder     <- row_order(Heatmap(matrix = scaled_mat,
                            name = "Intensities",
                            km = k, ## five K-means
                            row_km_repeats = n_boot, ## repeats to get consensus
                            col = colorRamp2(col,
                                             c("white",
                                               "yellow",
                                               "darkgoldenrod1",
                                               "violet",
                                               "purple")),
                            top_annotation = ha ,
                            show_column_names = F,
                            show_row_names = F,
                            cluster_columns = F,
                            cluster_rows = T,
                            clustering_method_rows = ClustMethod,
                            clustering_distance_rows = ClustDist))

  #rowOrder <- row_order(Scal_Heatmap)

  ## output 1 = metabolites per treatment

  out_list <- list()

  clust_ID <- c()

  for (i in 1:length(rowOrder)) {

    clust_ID[i] <- paste0("Cluster ", i)

    out_list[[i]] <- rownames(scaled_mat)[rowOrder[[i]]]

  }

  out_mat <- list2df(out_list)
  colnames(out_mat) <- clust_ID

  if (returnClustMeanPlots == T) {

    pdf("ClustMeanSEplots.pdf")
    par(mar = c(10,5,1,1))

    for (i in 1:length(rowOrder)) {

      cat("Producing Plot # ", i, "\n")

      cluster_K_mat <- scaled_mat[rowOrder[[i]],]

      if (is.null(dim(cluster_K_mat))) {

        boxplot(cluster_K_mat, las = 2, cex.axis = 0.5, main = paste0("Cluster No.", i))

      } else {

        boxplot(cluster_K_mat, las = 2, cex.axis = 0.5, main = paste0("Cluster No.", i))

        Cluster_Melted2 <- melt(cluster_K_mat)

        Cluster_Melted2[,"Var2"] <- as.numeric(Cluster_Melted2$Var2)

        c <- ggplot(Cluster_Melted2, aes(x = Var2 , y = value))

        print(c +
                geom_smooth() +
                geom_point(aes(colour = melt(cluster_K_mat)[,2])) +
                ggtitle(paste0("Cluster No.", i)))

      }

    }

    dev.off()

  }

  #if (returnClustDendrogram == T) {

   # png("ClustDendrogram.png")
    #print(Scal_Heatmap)
    #dev.off

  #}

  if (returnClustFeatures == T) {

    return(out_mat)

  }
}

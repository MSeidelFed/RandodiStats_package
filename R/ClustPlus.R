#' A clustering function
#'
#' This function allows you to rationalize the clustering method selected.
#' @param mat Defaults to a randomly generated matrix from the distribution_test_mat function.
#' @param autoscale_mat Defaults to True, allows to center and scale the input matrix.
#' @param center_fun Defaults to colMeans, allows to select any function that can be applied to columns as condition to center the matrix.
#' @param scale_fun Defaults to colSds, allows to select any function that can be applied to columns as condition to scale the matrix.
#' @param color_clust_Nr Defaults to 5, is the number of colored-partitions that the decision dendrograms will have.
#' @param nboots Defaults to 100, is the number of bootstrapps using the package pvclust.
#' @param AU_p_value Defaults to 0.95 or in other words the classic P value 0.05 treshold to define a significant cluster.
#' @param out_PDF Defaults to TRUE, if TRUE generates the respective bootstrapped-dendrogram plots.
#' @param out_PDF_name Defaults to test, name of the PDF file that will contain the plots.
#' @keywords class discovery, clustering, bootstrapping, pvclust
#' @export
#' @examples
#' test <- ClustPlus()


ClustPlus <- function(mat = distribution_test_mat(nrow_x = 50,
                                                  n_random_distributions = 100,
                                                  class_method = "discovery"),
                      autoscale_mat = T,
                      center_fun = colMeans,
                      scale_fun = colSds,
                      color_clust_Nr = 5,
                      nboots = 100,
                      AU_p_value = 0.95,
                      out_PDF = T,
                      out_PDF_name = "test"){


  ## Functions

  autoscale <- function(mat = distribution_test_mat(),
                        center_fun = colMeans,
                        scale_fun = colSds) {

    scaled_mat <- scale(mat, center = center_fun(mat), scale = scale_fun(mat))

    return(scaled_mat)

  }


  ## Main

  if (autoscale_mat == T) {

    mat = autoscale(mat)

  }

  ### decision method

  dend_E <- as.dendrogram(hclust(dist(mat)))
  d_E1<-color_labels(dend_E, k = color_clust_Nr) %>% color_branches(dend_E, k = color_clust_Nr)
  plot(d_E1, main = "Euclidean Distance")

  dend_C <- as.dendrogram(hclust(as.dist(1-cor(t(mat)))))
  d_C1<-color_labels(dend_C, k = color_clust_Nr) %>% color_branches(dend_C, k = color_clust_Nr)
  plot(d_C1, main = "Correlation")

  clust_method_t <- readline(prompt="Enter clustering method treatments (single, complete, average,...): ")
  clust_distance_t <- readline(prompt="Enter clustering distance treatment (correlation, uncentered,...): ")

  dend_E_M <- as.dendrogram(hclust(dist(t(mat))))
  d_E1_M<-color_labels(dend_E_M, k = color_clust_Nr) %>% color_branches(dend_E_M, k = color_clust_Nr)
  plot(d_E1_M, main = "Euclidean Distance")

  dend_C_M <- as.dendrogram(hclust(as.dist(1-cor(mat))))
  d_C1_M<-color_labels(dend_C_M, k = color_clust_Nr) %>% color_branches(dend_C_M, k = color_clust_Nr)
  plot(d_C1_M, main = "Correlation")

  clust_method_M <- readline(prompt="Enter clustering method variables (single, complete, average,...): ")
  clust_distance_M <- readline(prompt="Enter clustering distance variables (correlation, uncentered,...): ")

  return_decision_dendrograms <- readline(prompt="Do you want me to return the decision dendrograms? (Y/N): ")

  if (return_decision_dendrograms == "Y") {

    pdf(file = paste(out_PDF_name,"DD.pdf", sep = ""))

    par(mfrow=c(2,1))
    plot(d_E1, main = "Euclidean Distance")
    plot(d_C1, main = "Correlation")
    plot(d_E1_M, main = "Euclidean Distance")
    plot(d_C1_M, main = "Correlation")
    dev.off()

  }

  ## bootstrapped clustering

  ### treatments

  HCA_boot_t <- pvclust::pvclust(t(mat),
                        method.hclust = clust_method_t,
                        method.dist = clust_distance_t,
                        nboot = nboots)

  table_t <- pvclust::pvpick(HCA_boot_t, alpha = AU_p_value)

  clusters_t <- sapply(1:length(as.matrix(table_t$clusters)),
                       function(j) as.matrix(table_t$clusters)[[j]][1:300])

  ### variables

  HCA_boot_m <- pvclust::pvclust(mat,
                        method.hclust = clust_method_M,
                        method.dist = clust_distance_M,
                        nboot = nboots)

  table_m <- pvclust::pvpick(HCA_boot_m, alpha = AU_p_value)

  clusters_m <- sapply(1:length(as.matrix(table_m$clusters)),
                     function(j) as.matrix(table_m$clusters)[[j]][1:300])

  ## output

  if (out_PDF == T) {

  pdf(file = paste(out_PDF_name,".pdf", sep = ""))

  par(mfrow=c(1,1))

  plot(HCA_boot_t)
  pvclust::pvrect(HCA_boot_t, alpha = AU_p_value)

  plot(HCA_boot_m)
  pvclust::pvrect(HCA_boot_m, alpha = AU_p_value)

  pvclust::seplot(HCA_boot_t)

  pvclust::seplot(HCA_boot_m)

  dev.off()

  }

  if (class(clusters_t)[1] == "list" & class(clusters_m)[1] == "list") {

    print("no significant clusters available")

  } else if (class(clusters_t)[1] == "matrix" & class(clusters_m)[1] == "list") {

    return(clusters_t)

  } else if (class(clusters_t)[1] == "list" & class(clusters_m)[1] == "matrix") {

    return(clusters_m)

  } else if (class(clusters_t)[1] == "matrix" & class(clusters_m)[1] == "matrix") {

    return(list(clusters_t, clusters_m))

  }

}



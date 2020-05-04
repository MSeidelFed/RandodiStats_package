#' A Random distributions Function
#'
#' This function allows you to plot distributions from a matrix of response variables.
#' @param test_mat Defaults to a matrix built with distribution_test_mat().
#' @param vector_colors Defaults to grey.
#' @param transparency Defaults to 90%.
#' @param variable_name Defaults to test_mat.
#' @param ylim_plot Defaults to minimum and maximum kurtosis of the provided response variables.
#' @param xlim_plot Defaults to minimum and maximum square of skewness of the provided response variables.
#' @keywords distribution plot
#' @export
#' @examples
#' plotting_distributions()

plotting_distributions <- function(test_mat = distribution_test_mat(),
                                   vector_colors = "grey",
                                   transparency = 90,
                                   variable_name = "test_mat",
                                   ylim_plot = c(max(Variables2Shapes(test_mat)[,2]),
                                                 min(Variables2Shapes(test_mat)[,2])),
                                   xlim_plot = c(min(Variables2Shapes(test_mat)[,1]),
                                                 max(Variables2Shapes(test_mat)[,1]))) {

  ## necessary functions

  ##### Transparent colors - Mark Gardener 2015 - www.dataanalytics.org.uk

  t_col <- function(color, percent = 50, name = NULL) {
     #      color = color name
     #    percent = % transparency
     #       name = an optional name for the color

     ## Get RGB values for named color
     rgb.val <- col2rgb(color)
     ## Make new color using input color as base and alpha set by transparency
     t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                  max = 255,
                  alpha = (100 - percent) * 255 / 100,
                  names = name)

    ## Save the color
    invisible(t.col)
  }

  ## setting colors for plot
  colours <- c(rep("blue", 1000),
               rep("red", 1000),
               rep("black", 1000),
               rep("yellow", 1000),
               rep("pink", 1000),
               rep("orange", 1000),
               rep("purple", 1000),
               rep(t_col(color = vector_colors, percent = transparency),
                   dim(Variables2Shapes(test_mat))[1]))

  ## setting mat for plot
  complete_mat <- rbind(Variables2Shapes(Distribution_test_mat = distribution_test_mat()),
                        Variables2Shapes(test_mat))
  ## plot
  plot(complete_mat,
       ylim = ylim_plot,
       xlim = xlim_plot,
       col = colours)

  legend("topright",
         col = c("blue","red","black","yellow","pink","orange","purple","green", vector_colors),
         pch = 18,
         legend = c("gamma","logis","beta","normal","binomial","poisson","exponential", variable_name))

}

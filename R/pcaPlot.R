#' Plotting PCA via a prcomp object
#'
#' @param pcaObj prcomp object
#' @param obsData a data.frame matching the number of rows of the PCA data
#' @param colVar Character vector corresponding to one of the columns of obsData
#' @param ellipse Should ellipse be plotted based on colVar
#' @param topFeatures Number of original variables most associated with each of the PCs
#' @param arrows Should arrows be plotted
#' @param label.size Size of the label for the arrows
#' @import ggplot2
#' @import dplyr
#' @return A ggplot
#'
#'
#'
pcaPlot = function(pcaObj, obsData, colVar, ellipse = F,
                   topFeatures = 5, arrows = F, label.size = 2){
  # pcaObj = prcomp(x = data, center = T, scale. = TRUE)
  # evecs = reshape2::melt(pcaObj$rotation[,1:2],
  #                        varnames = c("originalVars", "PCs"),
  #                        value.name = "value")


  evecs = data.frame(pcaObj$rotation[,1:2]) %>%
    tibble::rownames_to_column(var = "originalVars") %>%
    dplyr::mutate(topPC1 = order(abs(PC1), decreasing = T) <= topFeatures ,
           topPC2 = order(abs(PC2), decreasing = T) <= topFeatures ) %>%
    dplyr::filter(topPC1|topPC2)

  # evecs

  plotdf = dplyr::mutate(obsData,
                  PC1 = pcaObj$x[,1],
                  PC2 = pcaObj$x[,2])


  biplot = ggplot2::ggplot(plotdf, aes_string(x = "PC1",
                                y = "PC2",
                                colour = colVar)) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "bottom")




  if(ellipse){
    biplot = biplot + ggplot2::stat_ellipse(type = "norm")
  }


  if(arrows){
    biplot = biplot +
      ggplot2::geom_segment(data = evecs,
                   aes(x = 0, xend = PC1,
                       y = 0, yend = PC2),
                   arrow = arrow(length = unit(1/2, 'picas')),
                   colour = "darkred") +
      ggplot2::geom_text(data = evecs, aes(label = originalVars),
                colour = "darkred", size = label.size)
  }

  return(biplot)
}

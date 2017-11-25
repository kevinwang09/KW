#' Add together two numbers
#'
#' @param RtsneObj Rtsne object
#' @param obsData a data.frame matching the number of rows of the PCA data
#' @param colVar Character vector corresponding to one of the columns of obsData
#' @param ellipse Should ellipse be plotted based on colVar
#'
tsnePlot = function(RtsneObj, obsData, colVar,
                    ellipse = T){

  plotdf = dplyr::mutate(obsData,
                  tsne1 = RtsneObj$Y[,1],
                  tsne2 = RtsneObj$Y[,2])

  g = ggplot2::ggplot(plotdf, aes_string(x = "tsne1",
                                         y = "tsne2",
                                         colour = colVar)) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = paste0("Perplexity = ", RtsneObj$perplexity) )


  if(ellipse){
    g = g + ggplot2::stat_ellipse(type = "norm")
  }


  return(g)
}

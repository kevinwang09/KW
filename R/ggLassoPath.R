#' @param lassoObj is a glmnet object from the `glmnet` package
#' @param colour is a vector that is of the same length as the number of variables fed into `lassoObj`
#' @title ggplot for lasso path plot
    ggLassoPath = function(lassoObj, colour){

    stopifnot(length(colour) == nrow(lassoObj$beta))
    stopifnot(is.logical(colour))

    lassoPlotdf = base::data.frame(lassoObj$beta %>% as.matrix,
                                    colour = colour) %>%
      tibble::rownames_to_column(var = "variables") %>%
      tidyr::gather(key = sVar,
                    value = coef,
                    -variables, -colour) %>%
      dplyr::left_join(data.frame(sVar = names(lassoObj$a0),
                                  lambda = lassoObj$lambda),
                       by = "sVar")

    lassoPlot = lassoPlotdf %>%
      ggplot2::ggplot(aes(x = lambda,
                 y = coef,
                 colour = colour)) +
      ggplot2::geom_line(ggplot2::aes(group = variables))


    return(lassoPlot)
    }

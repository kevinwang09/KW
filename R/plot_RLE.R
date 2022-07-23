#' RLE plot
#' @param exprs_matrix a matrix
#' @author Kevin Wang
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom stats median quantile
#' @export
#' @examples
#' n = 30
#' p = 1000
#' exprs_matrix = cbind(
#' matrix(rnorm((n - 3)*p), nrow = p),
#' matrix(rnorm(3*p, mean = 5), nrow = p)
#' )
#' boxplot(exprs_matrix)
#' plot_RLE(exprs_matrix)
plot_RLE = function(exprs_matrix){

  if(is.null(colnames(exprs_matrix))){
    colnames(exprs_matrix) = paste0("sample_", 1:ncol(exprs_matrix))
  }

  sample_median = apply(exprs_matrix, 2, median, na.rm = TRUE)
  sample_q1 = apply(exprs_matrix, 2, quantile, 0.25, na.rm = TRUE)
  sample_q3 = apply(exprs_matrix, 2, quantile, 0.75, na.rm = TRUE)

  rle_plotdf = tibble::tibble(
    sample_id = colnames(exprs_matrix) %>% forcats::as_factor(),
    sample_median = sample_median,
    sample_q1 = sample_q1,
    sample_q3 = sample_q3
  )


  rle_plot = rle_plotdf %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$sample_id,
                                 y = .data$sample_median,
                                 label = .data$sample_id)) +
    ggplot2::geom_point() +
    geom_errorbar(ggplot2::aes(ymin = .data$sample_q1, ymax = .data$sample_q3), width = 0) +
    ggplot2::geom_hline(yintercept = 0, colour = "red") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = element_text(angle = 90))


  result = list(
    rle_plotdf = rle_plotdf,
    rle_plot = rle_plot
  )
  return(result)
}

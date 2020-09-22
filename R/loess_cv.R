#' @title Calculate loess span parameter using cross validation
#' @param x a vector
#' @param y a vector
#' @importFrom rsample vfold_cv analysis assessment
#' @importFrom purrr map map2_dbl
#' @importFrom dplyr mutate select
#' @importFrom tidyr unnest
#' @importFrom broom augment
#' @examples
#' data(economics, package="ggplot2")  # load data
#' economics$index <- 1:nrow(economics)  # create index variable
#' data <- economics[1:80, ]  # retail 80rows for better graphical understanding
#' lresult = loess_cv(uempmed ~ index, data = data,
#' span = c(0.1, 0.2, 0.3, 0.4, 0.5), v = 5, repeats = 20)
#'
#' lresult %>%
#' group_by(span) %>%
#' summarise(sse = mean(resid2))
#'
#' lresult %>%
#' ggplot(aes(x = factor(span), y = resid2)) +
#' geom_boxplot()

loess_cv = function(formula, data, span = seq(0.1, 1, by = 0.1), v = 5, repeats = 2){
  cv_tbl = rsample::vfold_cv(data = data, v = v, repeats = repeats) %>%
    dplyr::mutate(span = list(span)) %>%
    tidyr::unnest(span)

  resid2 = purrr::map2_dbl(
    .x = cv_tbl$splits,
    .y = cv_tbl$span,
    .f = function(d, s){
      train = rsample::analysis(d)
      test = rsample::assessment(d)
      model = try(loess(formula = formula, data = train, span = s), silent = TRUE)
      aug = broom::augment(model, newdata = test)
      return(mean(aug$.resid^2, na.rm = TRUE))
    })

  result = dplyr::mutate(cv_tbl, resid2) %>%
    dplyr::select(-splits)

  return(result)
}


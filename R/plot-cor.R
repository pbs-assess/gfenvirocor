#' Plot correlations
#' hack found on stackoverflow https://stackoverflow.com/questions/37889222/change-colors-in-ggpairs-now-that-params-is-deprecated
#'
#' @param data dataframe of multiple indices produced using extract_enviro_var() function
#'
#' @export
check_correlations <- function(data){
  library(GGally)
  dw <- data |> select(year, type, value) |>
    pivot_wider(names_from = type, values_from = value)

  ggpairs(dw, columns = c(2:ncol(dw)),
          upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 ='))),
          progress = FALSE)
}

#' @param data
#' @param mapping
#' @param method
#' @param symbol
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
cor_func <- function(data, mapping, method, symbol, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)

  corr <- cor(x, y, method=method, use='complete.obs')

  colFn <- colorRampPalette(c("brown1", "white", "dodgerblue"),
                            interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length = 100))]

  ggally_text(
    label = paste(symbol, as.character(round(corr, 2))),
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    color = 'black',
    ...) +
    theme_void() +
    theme(panel.background = element_rect(fill = fill))
}


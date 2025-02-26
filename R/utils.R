#' Get environmental means for a specific set of months and standardize
#'
#' @param data Any monthly environmental index with numeric month and year columns
#' @param months Which months to include as an average
#' @param type_label
#'
#' @return
#' @export
#'
#' @examples
filter_months <- function(data, months, type_label){
  data |>
    filter(month %in% months) |>
    group_by(year) |>
    summarise(value = mean(value, na.rm = TRUE)) |>
    filter(!is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           mean = mean(value),
           sd = sd(value),
           value = (value - mean(value))/ sd(value),
           type = type_label)
}

# get_model_table <- function(model){
#   .m <- summary(model)
#   .t <- as.data.frame(round(.m$coefficients$cond, 3))
#   df <- tibble::rownames_to_column(.t, "Parameter")
#   df
# }

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

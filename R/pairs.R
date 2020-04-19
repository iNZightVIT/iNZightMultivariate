#' @export
#' @importFrom magrittr "%>%"
inzight.ggpairs <- function(data, vars = NULL) {
  vars_used <- vars
  if (is.character(data)) {
    data_name <- rlang::sym(data)
  } else if (!is.name(data)) {
    data_name <- match.call()[["data"]]
  } else {
    data_name <- data
  }

  if (!is.null(vars_used)) {
    subset_data <- rlang::expr((!!data_name)[complete.cases(!!data_name), !!vars_used, drop = FALSE])
  } else {
    subset_data <- rlang::expr(Filter(is.numeric, !!data_name))
  }

  plot_expr <- rlang::expr(
    GGally::ggpairs(!!subset_data)
  )

  list(
    plot = plot_expr
  )
}

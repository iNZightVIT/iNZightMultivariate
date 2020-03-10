#' @export
#' @importFrom magrittr "%>%"
inz.parcoord <- function(data.name, vars = NULL, group = NULL, show.points = TRUE, scale.fun = scale, alpha = 0.25) {
  if (is.character(data.name)) {
    data.name <- rlang::sym(data.name)
  } else if (!is.name(data.name)) {
    data.name <- match.call()[["data.name"]]
  }

  if (is.null(vars)) {
    mutate_expr <- rlang::expr(dplyr::mutate_if(is.numeric, !!rlang::enexpr(scale.fun)))
    pivot_expr <- rlang::expr(tidyr::pivot_longer(cols = names(dplyr::select_if(!!data.name, is.numeric)), values_to = "value"))
    mutate_expr2 <- rlang::expr(dplyr::mutate(name = factor(name)))
  } else {
    mutate_expr <- rlang::expr(dplyr::mutate_at(dplyr::vars(!!!rlang::syms(vars)), !!rlang::enexpr(scale.fun)))
    pivot_expr <- rlang::expr(tidyr::pivot_longer(cols = !!rlang::enexpr(vars), values_to = "value"))
    mutate_expr2 <- rlang::expr(dplyr::mutate(name = factor(name, levels = !!rlang::enexpr(vars))))
  }

  data.expr <- rlang::expr(
    data_long <- !!data.name %>%
      dplyr::mutate(parcoord.id = factor(1:dplyr::n())) %>%
      !!mutate_expr %>%
      !!pivot_expr %>%
      !!mutate_expr2
  )

  if (!is.null(group)) {
    plot.expr <- rlang::expr(
      ggplot2::ggplot(data_long, ggplot2::aes(x = name, y = value, group = parcoord.id, colour = !!rlang::sym(group))) +
        ggplot2::geom_line(alpha = !!alpha) +
        ggplot2::labs(x = "Variable", y = "Scaled Value")
    )
  } else {
    plot.expr <- rlang::expr(
      ggplot2::ggplot(data_long, ggplot2::aes(x = name, y = value, group = parcoord.id)) +
        ggplot2::geom_line(alpha = !!alpha) +
        ggplot2::labs(x = "Variable", y = "Scaled Value")
    )
  }

  if (show.points) {
    plot.expr <- rlang::expr(
      !!rlang::enexpr(plot.expr) +
        ggplot2::geom_point(alpha = !!alpha)
    )
  }

  list(
    data = data.expr,
    plot = plot.expr
  )
}

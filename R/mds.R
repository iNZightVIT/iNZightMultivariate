#' @export
#' @importFrom magrittr "%>%"
#' @importFrom stats cmdscale dist cor
inzight.mds <- function(data, vars = NULL, colour = NULL, x = 1, y = 2, k = NULL) {
  vars_used <- vars
  if (is.character(data)) {
    data_name <- rlang::sym(data)
  } else if (!is.name(data)) {
    data_name <- match.call()[["data"]]
  } else {
    data_name <- data
  }

  if (!is.null(vars_used)) {
    subset_data <- rlang::expr((!!data_name)[complete.cases(!!data_name), !!vars_used])
    if (is.null(k)) {
      k <- 2
    }
  } else {
    subset_data <- rlang::expr(Filter(is.numeric, !!data_name))
  }

  pca_name <- rlang::sym(sprintf("%s.mds", as.character(data_name)))

  analysis_expr <- rlang::expr(
    !!pca_name <- cmdscale(dist(!!subset_data), k = !!k, eig = TRUE)
  )



  list(
    analysis = analysis_expr
  )
}

#' @export
plot_inzight.mds <- function(mds.object, data, colour = NULL, shape = NULL, x = 1, y = 2) {
  if (is.character(mds.object)) {
    mds_name <- rlang::sym(mds.object)
  } else if (!is.name(data)) {
    mds_name <- match.call()[["mds.object"]]
  } else {
    mds_name <- mds.object
  }

  x <- rlang::sym(sprintf("X%d", x))
  y <- rlang::sym(sprintf("X%d", y))

  if (!is.null(colour) && colour != "" || !is.null(shape) && shape != "") {
    if (is.character(data)) {
      data_name <- rlang::sym(data)
    } else if (!is.name(data)) {
      data_name <- match.call()[["data"]]
    } else {
      data_name <- data
    }

    data_expr <- rlang::expr(
      to_plot <- cbind((!!data_name)[complete.cases(!!data_name), ], data.frame((!!mds_name)$points))
    )

    if (!is.null(colour) && colour != "" && !is.null(shape) && shape != "") {
      plot_expr <- rlang::expr(
        ggplot2::ggplot(to_plot, ggplot2::aes(x = !!x, y = !!y, colour = !!rlang::sym(colour), shape = !!rlang::sym(shape))) +
          ggplot2::geom_point()
      )
    } else if (!is.null(shape) && shape != "") {
      plot_expr <- rlang::expr(
        ggplot2::ggplot(to_plot, ggplot2::aes(x = !!x, y = !!y, shape = !!rlang::sym(shape))) +
          ggplot2::geom_point()
      )
    } else {
      plot_expr <- rlang::expr(
        ggplot2::ggplot(to_plot, ggplot2::aes(x = !!x, y = !!y, colour = !!rlang::sym(colour))) +
          ggplot2::geom_point()
      )
    }

    list(
      data = data_expr,
      plot = plot_expr
    )
  } else {
    plot_expr <- rlang::expr(
      ggplot2::ggplot(data.frame((!!mds_name)$points), ggplot2::aes(x = !!x, y = !!y)) +
        ggplot2::geom_point()
    )

    list(
      plot = plot_expr
    )
  }
}

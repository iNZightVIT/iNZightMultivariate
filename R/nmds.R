#' @export
#' @importFrom magrittr "%>%"
inzight.nmds <- function(data, vars = NULL, colour = NULL, x = 1, y = 2, k = NULL) {
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

  pca_name <- rlang::sym(sprintf("%s.nmds", as.character(data_name)))

  analysis_expr <- rlang::expr(
    !!pca_name <- vegan::metaMDS(!!subset_data, k = !!k)
  )

  # if (!is.null(colour) && colour != "") {
  #   data_expr <- rlang::expr(
  #     to_plot <- cbind((!!data_name)[complete.cases(!!data_name), ], data.frame((!!pca_name)$points))
  #   )
  #
  #   plot_expr <- rlang::expr(
  #     ggplot2::ggplot(to_plot, ggplot2::aes(x = MDS1, y = MDS2, colour = !!rlang::sym(colour))) +
  #       ggplot2::geom_point() +
  #       ggplot2::labs(x = "MDS1", y = "MDS2")
  #   )
  #
  #   list(
  #     analysis = analysis_expr,
  #     data = data_expr,
  #     plot = plot_expr
  #   )
  # } else {
  #   plot_expr <- rlang::expr(
  #     ggplot2::ggplot(data.frame((!!pca_name)$points), ggplot2::aes(x = MDS1, y = MDS2)) +
  #       ggplot2::geom_point() +
  #       ggplot2::labs(x = "MDS1", y = "MDS2")
  #   )
  #
  #   list(
  #     analysis = analysis_expr,
  #     plot = plot_expr
  #   )
  # }

  list(
    analysis = analysis_expr
  )
}

#' @export
plot_inzight.nmds <- function(nmds.object, data, colour = NULL, x = 1, y = 2) {
  if (is.character(nmds.object)) {
    nmds_name <- rlang::sym(nmds.object)
  } else if (!is.name(data)) {
    nmds_name <- match.call()[["nmds.object"]]
  } else {
    nmds_name <- nmds.object
  }

  if (!is.null(colour) && colour != "") {
    if (is.character(data)) {
      data_name <- rlang::sym(data)
    } else if (!is.name(data)) {
      data_name <- match.call()[["data"]]
    } else {
      data_name <- data
    }

    data_expr <- rlang::expr(
      to_plot <- cbind((!!data_name)[complete.cases(!!data_name), ], data.frame((!!nmds_name)$points))
    )

    plot_expr <- rlang::expr(
      ggplot2::ggplot(to_plot, ggplot2::aes(x = MDS1, y = MDS2, colour = !!rlang::sym(colour))) +
        ggplot2::geom_point() +
        ggplot2::labs(x = "MDS1", y = "MDS2")
    )

    list(
      data = data_expr,
      plot = plot_expr
    )

  } else {
    plot_expr <- rlang::expr(
      ggplot2::ggplot(data.frame((!!nmds_name)$points), ggplot2::aes(x = MDS1, y = MDS2)) +
        ggplot2::geom_point() +
        ggplot2::labs(x = "MDS1", y = "MDS2")
    )

    list(
      plot = plot_expr
    )

  }
}

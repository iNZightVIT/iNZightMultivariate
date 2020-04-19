#' @export
#' @importFrom magrittr "%>%"
#' @importFrom stats prcomp complete.cases
inzight.pca <- function(data, vars = NULL, colour = NULL, x = 1, y = 2) {
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
  } else {
    subset_data <- rlang::expr(Filter(is.numeric, !!data_name))
  }

  pca_name <- rlang::sym(sprintf("%s.pca", as.character(data_name)))

  analysis_expr <- rlang::expr(
    !!pca_name <- prcomp(!!subset_data, scale = TRUE)
  )

  list(
    analysis = analysis_expr
  )
}

#' @export
  plot_inzight.pca <- function(pca.object, data, colour = NULL, shape = NULL, x = 1, y = 2) {
  if (is.character(pca.object)) {
    pca_name <- rlang::sym(pca.object)
  } else if (!is.name(data)) {
    pca_name <- match.call()[["pca.object"]]
  } else {
    pca_name <- pca.object
  }

  if (!is.null(colour) && colour != "" || !is.null(shape) && shape != "") {
    if (is.character(data)) {
      data_name <- rlang::sym(data)
    } else if (!is.name(data)) {
      data_name <- match.call()[["data"]]
    } else {
      data_name <- data
    }

    if (!is.null(colour) && colour != "" && !is.null(shape) && shape != "") {
      plot_expr <- rlang::expr(
        print(autoplot(!!pca_name, data = (!!data_name)[complete.cases(!!data_name), ], colour = !!colour, shape = !!shape, x = !!x, y = !!y))
      )
    } else if (!is.null(shape) && shape != "") {
      plot_expr <- rlang::expr(
        print(autoplot(!!pca_name, data = (!!data_name)[complete.cases(!!data_name), ], shape = !!shape, x = !!x, y = !!y))
      )
    } else {
      plot_expr <- rlang::expr(
        print(autoplot(!!pca_name, data = (!!data_name)[complete.cases(!!data_name), ], colour = !!colour, x = !!x, y = !!y))
      )
    }
  } else {
    plot_expr <- rlang::expr(
      autoplot(!!pca_name, x = !!x, y = !!y)
    )
  }

  list(
    package = rlang::expr(library(ggfortify)),
    plot = plot_expr
  )
}

#' @importFrom utils capture.output
#' @export
summary_inzight.pca <- function(pca) {
  n.pcs <- min(length(pca$sdev), 5)
  n.pcs.note <- ifelse(n.pcs == length(pca$sdev), "", sprintf("Results for leading 5 principal components\n"))
  c(
    n.pcs.note,

    "Loadings (eigenvectors): ",
    capture.output(signif(pca$rotation[, 1:n.pcs, drop = FALSE], 3)),

    "\nStandard deviations: ",
    paste0(signif(pca$sdev[1:n.pcs], 4), collapse = " "),

    # "\nExplained variance: ",
    "\nProportion of explained variance: ",
    paste0(format(pca$sdev[1:n.pcs] / sum(pca$sdev), digits = 3), collapse = " "),

    "\nCumulative proportion of explained variance: ",
    paste0(format(cumsum(pca$sdev[1:n.pcs]) / sum(pca$sdev), digits = 3), collapse = " ")
  )
}

inzight.screeplot <- function(data.prcomp, kaiser = FALSE) {
  data_name <- rlang::sym(as.character(match.call()$data.prcomp))

  data_expr <- rlang::expr(
    variance.df <- data.frame(
      PC = 1:length((!!data_name)$sdev),
      Variance = (!!data_name)$sdev^2
    )
  )

  plot_expr <- rlang::expr(
    ggplot2::ggplot(variance.df, ggplot2::aes(x = PC, y = Variance)) +
      ggplot2::geom_line() +
      ggplot2::geom_point()
  )

  if (kaiser) {
    plot_expr <- rlang::expr(
      !!plot_expr + ggplot2::geom_hline(yintercept = 1, lty = "dashed")
    )
  }

  list(
    data = data_expr,
    plot = plot_expr
  )
}

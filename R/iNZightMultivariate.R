#' @export
iNZightMultivariate <- function(data, method, ...) {
  extra_args <- list(...)

  if (!check_args(method, extra_args)) {
    stop("Missing arguments")
  }

  used.fun <- list(
    pcp = inz.parcoord,
    pca = inzight.pca,
    scree = inzight.screeplot
  )

  result.expr <- do.call(used.fun[[method]], c(data = as.character(match.call()[["data"]]), extra_args))

  result <- lapply(result.expr, eval)[[length(result.expr)]]
  attr(result, "code") <- unname(unlist(lapply(result.expr, rlang::expr_text)))

  result
}

iNZightMV_analysis <- function(data, method, ...) {
  extra_args <- list(...)

  if (!check_args(method, extra_args)) {
    stop("Missing arguments")
  }

  used.fun <- list(
    pcp = inz.parcoord,
    pca = inzight.pca,
    scree = inzight.screeplot
  )

  result.expr <- do.call(used.fun[[method]], c(data = as.character(match.call()[["data"]]), extra_args))

  result <- lapply(result.expr, eval)[[length(result.expr)]]
  attr(result, "code") <- unname(unlist(lapply(result.expr, rlang::expr_text)))

  result
}

iNZightMV_plot <- function(mv.object, ...) {
  if ("prcomp" %in% class(mv.object)) {

  }
}

iNZightMV_summary <- function(mv.object, ...) {

}

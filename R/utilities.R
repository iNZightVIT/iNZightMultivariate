#' re-export magrittr pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

required_args <- list(
  pcp = c("vars")
)

check_args <- function(method, extra_args) {
  matches <- required_args[[method]] %in% names(extra_args)

  sum(matches) == length(required_args[[method]])
}

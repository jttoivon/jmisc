
#' A function to check whether a numeric vector contains no decimals.
#'
#' @description
#' Check whether the numeric vector can be converted to type integer
#' without loosing information.
#'
#' @param x A numeric vector
#' @param tol The tolerance allowed
#'
#' @returns A single logical value
#' @seealso [is.integer()] which only checks the type
#' @export
#'
#' @examples
#' is_whole_number(c(0.0, 1, NA))
is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
  all(abs(x - round(x)) < tol, na.rm = TRUE)
}

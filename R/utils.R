#' @importFrom rlang :=
NULL

#' @importFrom rlang .data
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @importFrom grid grobName
#' @export
grid:::grobName

#' Check class of object.
#'
#' @param x Object to be tested.
#' @return TRUE or FALSE and warning message.
#' @name check_class
#' @examples
#' x <- 1
#' y <- "text"
#' check_text(x)
#' check_text(y)
#' check_numeric(x)
#' check_numeric(y)
#'
#' @export
check_text <- function(x) {
  if (!is.null(x)) {
    if (!is.character(x)) {
      warning("text MUST be character!")
      return(FALSE)
    }
  }
  return(TRUE)
}

#' @rdname check_class
#' @export
check_numeric <- function(x) {
  if (!is.null(x)) {
    if (!is.numeric(x)) {
      warning("x MUST be numeric!")
      return(FALSE)
    }
  }
  return(TRUE)
}

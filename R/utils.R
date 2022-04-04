#' Convert inputs into a list.
#' @param ... Vectors or a list.
#' @export
dots2list <- function(...){
  res <- list(...)
  res[sapply(res, is.null)] <- NULL # remove NULL
  if (length(res) == 1) res <- res[[1]]
  return(res)
}

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

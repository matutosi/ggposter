#' Same Argument Manipulation to Evaluate
#'
#' @param fun A function.
#' @param ... Vectors or a list.
#' @export
shoot <- function(fun, ...){
  # another name example: sluice, flow, divert
  # do_same <- function(fun, ...){
  env <- rlang::caller_env()
  do.call(fun, eval(same(...), env))
}

#' @rdname shoot
#' @export
same <- function(...){
  res <- stringr::str_c("list(", as_same(...), ")")
  parse(text=res)
}

#' @rdname shoot
#' @export
as_same <- function(...){
  rlang::exprs(...) %>%
    purrr::map(rep_args) %>%
    as.character() %>%
    stringr::str_c(collapse = ", ")
}

#' @rdname shoot
#' @param x A string.
#' @export
rep_args <- function(x) stringr::str_c(x, " = ", x)

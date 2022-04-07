#' Same Argument Manipulation to Evaluate
#'
#' better to change function name into another name
#'      such as: sluice, flow, shoot, divert
#'
#' @param fun A function.
#' @param ... Vectors or a list.
#' @export
do_same <- function(fun, ...){
  env <- rlang::caller_env()
  do.call(fun, eval(same(...), env))
}

#' @rdname do_same
#' @export
same <- function(...){
  res <- stringr::str_c("list(", as_same(...), ")")
  parse(text=res)
}

#' @rdname do_same
#' @export
as_same <- function(...){
  rlang::exprs(...) %>%
    purrr::map(rep_args) %>%
    as.character() %>%
    stringr::str_c(collapse = ", ")
}

#' @rdname do_same
#' @param x A string.
#' @export
rep_args <- function(x) stringr::str_c(x, " = ", x)

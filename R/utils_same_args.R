#' @rdname same
#' @export
do_same <- function(fun, ...){
  env <- rlang::caller_env()
  do.call(fun, eval(same(...), env))
}

#' Same Argument Manipulation to Evaluate
#' @param ... Vectors or a list.
#' @export
same <- function(...){
  res <- stringr::str_c("list(", as_same(...), ")")
  parse(text=res)
}

#' @rdname same
#' @export
as_same <- function(...){
  rlang::exprs(...) %>%
    purrr::map(rep_args) %>%
    as.character() %>%
    stringr::str_c(collapse = ", ")
}

#' @rdname same
#' @param x A string.
#' @export
rep_args <- function(x) stringr::str_c(x, " = ", x)

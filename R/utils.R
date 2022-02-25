  #' Check class of object. 
  #' 
  #' @param x Object to be coerced or tested.
  #' @return Returns nothing. When error, only error massage. 
  #' 
  #' @examples
  #' x <- 1
  #' y <- "text"
  #' check_text(x)
  #' check_text(y)
  #' check_numeric(x)
  #' check_numeric(y)
  #' 
  #' @export
check_text <- function(x){
  if(!is.null(x)) if(!is.character(x)) stop("text MUST be character!")
}
check_numeric <- function(x){
  if(!is.null(x)) if(!is.numeric(x)) stop("x MUST be numeric!")
}

  #' Check class of object. 
  #' 
  #' @param ... Any object. 
  #' @return String of value name. 
  #' 
  #' @examples
  #' x <- 1
  #' y <- "text"
  #' z <- NULL
  #' as_names_dots(x, y, z)
  #' 
  #' @export
as_names_dots <- function(...){
 as.character(pryr::named_dots(a, b, d))
}

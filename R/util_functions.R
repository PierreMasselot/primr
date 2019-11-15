#' Construct an objective function from a user given one.
#'
#' @param fun The function given to \code{obj.fun} in the 
#'    main function.
#'
#' @details The purpose is to create an objective function
#'    that takes two arguments: \code{y} for the response
#'    vector and \code{inbox} a logical vector indicating
#'    if each observation lies in the box for which the
#'    objective function is computed.
construct_objfun <- function(fun){
  fun <- match.fun(fun)
  arguments <- names(formals(fun))
  if (!all(c("y", "x", "inbox") %in% arguments)){
    if (!all(c("y", "x") %in% arguments)){
      objfun <- function(y, x = NULL, inbox = T){
        y <- y[inbox]
        do.call(fun, list(y))
      }
    } else {
      objfun <- function(y, x = NULL, inbox = T){
        y <- y[inbox]
        x <- x[inbox,]
        do.call(fun, list(y, x))
      }
    }
  } else {
    objfun <- fun
  }
  return(objfun)
}
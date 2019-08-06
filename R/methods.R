###############################################################################
#
#                         Methods for the boxes                                   
#
###############################################################################

#' Priority to npeel, then support, then yfun
#' If there is no matching box, the last one is returned
extract.box <- function(object, npeel = NULL, support = NULL, yfun = NULL)
# Extract first box with smaller support than 'support' argument
# boxes: result from peeling.sequence
{
    if (is.null(npeel)){
      if (is.null(support)){
        if (is.null(yfun)){
          warning("'npeel', 'support' and 'yfun' are all NULL. Last box is returned")
          box.ind <- object$npeel + 1
        } else{
          box.ind <- sapply(yfun, function(x){
            Position(function(y) y >= x, object$yfun, 
              nomatch = object$npeel + 1)
          })          
        }
      } else {
        box.ind <- sapply(support, function(x){
          Position(function(y) y <= x, object$support, 
            nomatch = object$npeel + 1)
        })  
      }
    } else {
      box.ind <- pmin(npeel + 1, object$npeel + 1)
    }
    final.box <- list(limits = object$limits[box.ind], npeel = box.ind - 1, 
      yfun = object$yfun[box.ind], support = object$support[box.ind])
    return(final.box)
}

in.box <- function(x, limits, y = NULL, numeric.vars = NULL){
# If y non NULL, its values of observations are returned. If NULL, a logical vector is returned where TRUE means the observation is inside the box
    x <- as.data.frame(x)
    if(is.null(numeric.vars)) numeric.vars <- sapply(x, is.numeric)
    p <- ncol(x)
    n <- nrow(x)
    bool.mat <- matrix(FALSE, n, p)
    for (j in 1:p){
      if (numeric.vars[j]){
        bool.mat[,j] <- (x[,j] >= limits[[j]][1]) & (x[,j] <= limits[[j]][2])
      } else {
        bool.mat[,j] <- x[,j] %in% limits[[j]]
      }
    }
    inbox <- apply(bool.mat, 1, all)
    if (is.null(y)){
      res <- inbox
    } else {
      res <- y[inbox]
    }
    return(res)
}

#' For each observation in \code{x}, check if it is in each box of 
#'    \code{object}.
#' @param y A vector of responses corresponding to the observations in 
#'    \code{x}. If provided, the function returns the evaluation of \code{y}
#'    for each of the boxes in \code{object}. 
predict.prim <- function(object, x, y = NULL, obj.fun = mean)
{
    x <- as.data.frame(x)
    n <- nrow(x)
    nboxes <- length(object$limits)
    inbox <- sapply(object$limits, in.box, x = x, simplify = "matrix")
    support <- apply(inbox, 2, mean)
    out <- list(inbox = inbox, support = support)
    if (!is.null(y)){
      out$yfun <- apply(inbox, 2, function(bool){
        do.call(obj.fun, list(x = y[bool]))
      })
    }    
    return(out)
}

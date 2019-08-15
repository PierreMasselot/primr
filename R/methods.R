###############################################################################
#
#                         Methods for the boxes                                   
#
###############################################################################

#' Priority to npeel, then support, then yfun
#' If there is no matching box, the last one is returned
extract.box <- function(object, npeel = NULL, support = NULL, yfun = NULL,
  npaste = NULL)
# Extract first box with smaller support than 'support' argument
# boxes: result from peeling.sequence
{  
  inds.npeel <- pmin(npeel + 1, object$npeel + 1)
  inds.support <- sapply(support, function(x){
    Position(function(y) y <= x, object$support, 
      nomatch = object$npeel + 1)
  })  
  inds.yfun <- sapply(yfun, function(x){
    Position(function(y) y >= x, object$yfun, 
      nomatch = object$npeel + 1)
  })  
  inds.npaste <- pmin(npaste, object$npaste) + object$npeel + 1
  box.ind <- c(inds.npeel, inds.support, inds.yfun, inds.npaste)
  box.ind <- unlist(box.ind)
  if (length(box.ind) == 0){
    warning("No box matches the arguments. Last box is returned")
    box.ind <- with(object, npeel + npaste + 1)
  }
  box.ind <- sort(unique(box.ind))  
  final.box <- list(limits = object$limits[box.ind], 
    npeel = pmin(box.ind - 1, object$npeel), 
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

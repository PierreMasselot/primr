###############################################################################
#
#                         Methods for the boxes                                   
#
###############################################################################

#' Extract boxes
#'
#' Extracts one or several boxes from a \code{prim} object.
#' 
#' @param object \code{prim} object.
#' @param npeel Integer vector indicating which boxes to choose in \code{object} 
#'    through the number of peeling iteration.
#' @param support Numeric vector with values between 0 and 1 indicating the 
#'    support of boxes to choose in \code{object}.
#' @param yfun Numeric vector indicating the value of the objective function
#'    for the chosen boxes in \code{object}.
#' @param npaste Integer vector indicating which boxes to choose in 
#'    \code{object} through the number of pasting iteration.
#'
#' @details Returns one or several boxes from \code{object}. The boxes can be
#'    given through the number of peeling iterations (argument \code{npeel}), 
#'    pasting iterations (argument \code{npaste}), closest support 
#'    (argument \code{support}) or closest objective function value
#'    (argument \code{yfun}). Note that several of these arguments can be
#'    used at once to return several boxes. If no box matches the arguments
#'    or if they are all \code{NULL}, the last box in \code{object} is
#'    returned.
#'
#' @return A list with elements:
#'    \item{limits}{A list giving the limits of extracted boxes. Each limit is 
#'      a list with one element per input variable.}
#'    \item{npeel}{A vector giving the number of peeling iterations leading to 
#'      each extracted box.}
#'    \item{yfun}{A vector giving the objective function value of each 
#'      extracted box.}
#'    \item{support}{A vector giving the support of each extracted box.}
#'
#' @seealso \code{peeling} to perform the peeling and create a \code{prim}
#'    object.
#'
#' @examples
#'    # A simple bump
#'    set.seed(12345)
#'    x <- matrix(runif(2000), ncol = 2, dimnames = list(NULL, c("x1", "x2")))
#'    y <- 2 * x[,1] + 5 * x[,2] + 10 * (x[,1] >= .8 & x[,2] >= .5) + rnorm(1000)
#'    # Peeling step
#'    peel_res <- peeling(y, x)
#'    # Extracting a single box
#'    single_box <- extract.box(peel_res, support = 0.1)
#'    # Extracting all boxes from a prim object
#'    all_boxes <- extract.box(peel_res, npeel = 0:peel_res$npeel)
#'
#' @export
extract.box <- function(object, npeel = NULL, support = NULL, 
  yfun = NULL, npaste = NULL)
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

#' Predict method for a \code{prim} object
#'
#' For each observation in \code{newx}, check if it is in selected boxes of 
#'    \code{object}. Also gives the objective function values of each box
#'    based on the \code{newy} values.
#'
#' @param object A \code{prim} object.
#' @param newx A data.frame of new input values. If missing, uses
#'    \code{object$x}.
#' @param newy A vector of new responses values corresponding to the  
#'    inputs in \code{newx}. If missing, uses
#'    \code{object$y}.
#' @param npeel Integer vector indicating which boxes in \code{object} 
#'    to use for prediction, through the number of peeling iteration.
#' @param support Numeric vector with values between 0 and 1 indicating the 
#'    support of boxes in \code{object} to use for prediction.
#' @param yfun Numeric vector indicating the value of the objective function
#'    for the boxes in \code{object} to use for prediction.
#' @param npaste Integer vector indicating which boxes in \code{object} 
#'    to use for prediction, through the number of pasting iteration.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A list with elements:
#'    \item{inbox}{A logical matrix of dimension n x nboxes where n is the
#'      number of observations in \code{newx}. For each observation and each 
#'      returns \code{TRUE} if the observation is inside the box.}
#'    \item{support}{A numeric vector giving, for each box, the proportion
#'      of observations in \code{newx} lying inside the box.}
#'    \item{yfun}{A numeric vector giving, for each box, the objective function
#'      value computed on \code{newy}.}
#'
#' @seealso \code{peeling} and \code{pasting} for creating \code{prim}
#'    objects.
#'
#' @examples
#'    set.seed(12345)
#'    x <- matrix(runif(2000), ncol = 2, dimnames = list(NULL, c("x1", "x2")))
#'    y <- 2 * x[,1] + 5 * x[,2] + 10 * (x[,1] >= .8 & x[,2] >= .5) + rnorm(1000)
#'    # Peeling step
#'    peel_res <- peeling(y, x)
#'    # Prediction
#'    predict(peel_res, newx = data.frame(x1 = 1:10/11, x2 = 1:10/11),
#'      newy = 1:10, support = c(1, 0.25))
#'
#' @export
predict.prim <- function(object, newx, newy, npeel = NULL, support = NULL, 
  yfun = NULL, npaste = NULL, ...)
{    
  if (missing(newx)){
    newx <- object$x
  } else {
    newx <- as.data.frame(newx)
    if (ncol(newx) != ncol(object$x)){
      stop("Inconsistent number of variables in 'newx'")
    }
  }
  if (missing(newy)){
    newy <- object$y
  }
  boxes <- extract.box(object, npeel = npeel, support = support, 
    yfun = yfun, npaste = npaste)
  inbox <- sapply(boxes$limits, in.box, x = newx, simplify = "matrix")
  support <- apply(inbox, 2, mean)
  yfun <- apply(inbox, 2, function(bool){
    do.call(object$obj.fun, list(y = newy, x = x, inbox = bool))
  })
  out <- list(inbox = inbox, support = support, yfun = yfun)  
  return(out)
}

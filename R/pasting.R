###############################################################################
#
#                   Pasting functions                             
#
###############################################################################

#' Bottom up pasting
#'
#' Refines the edges of a chosen box in a \code{prim} object by pasting.
#'
#' @param object \code{prim} object resulting from a call to 
#'    \code{\link{peeling}}.
#' @param npeel Numeric value indicating which box to choose in \code{object} 
#'    through the number of peeling iteration.
#' @param support Numeric value between 0 and 1 indicating the support of the
#'    box to choose in \code{object}.
#' @param yfun Numeric value indicating the value of the objective function
#'    for the chosen box in \code{object}.
#' @param alpha The proportion of observations to add at each pasting iteration.
#'    Usually equal to the peeling fraction used in \code{\link{peeling}}.
#' @param obj.fun The objective function to maximize by pasting.
#' @param peeling.side Constraints on the pasting side. 
#'    -1 indicates pasting only on the 'left' of the box
#'    (i.e. moving the lower limit only), 1 indicate pasting only on the
#'    'right' and 0 for no constraint.
#'
#' @details The function takes a \code{prim} object and choose one of its boxes
#'    as a starting point for pasting. Bottom-up pasting is the reverse of
#'    of the top down peeling. It starts from the result of the peeling and
#'    refines its edges by iteratively adding \code{alpha} times N 
#'    observations at each iteration, where N is the number of observations
#'    in the current box.   
#'
#'    The best box after the peeling should be chosen by analyzing the
#'    peeling trajectory (see \code{\link{plot_trajectory}}). 
#'    It is given by one of: number of peeling iteration leading to the box 
#'    (argument \code{npeel}), the closest support (argument \code{support}), 
#'    or the closest objective function value (argument \code{yfun}). 
#'
#'    Although it is possible to use different algorithm parameters 
#'    (\code{alpha}, \code{obj.fun}, \code{peeling.side}) than the peeling
#'     step, it is advised to keep the same values (the default).
#'
#' @return A \code{prim} object which is a list with the following elements:
#'    \item{npeel}{The number of peeling iteration performed.}
#'    \item{support}{A vector of length \code{npeel + npaste + 1} 
#'      containing the support 
#'      of each successivepeeled box.}
#'    \item{yfun}{A vector of length \code{npeel + npaste + 1} containing 
#'      the objective function value of each successive peeled box.}
#'    \item{limits}{A list of length \code{npeel + npaste + 1} containing the 
#'      limits of each successive box. Each limit is a list with one element per 
#'      input variable.}
#'    \item{x,y}{The input and response data used in the algorithm.}
#'    \item{numeric.vars}{A logical vector indicating, for each input variable,
#'      if it was considered as a numeric variable.}
#'    \item{alpha, peeling.side, obj.fun}{The value of the arguments used for
#'      peeling. Useful for prim methods.}
#'    \item{npaste}{Number of pasting iteration performed.}
#'
#' @seealso \code{\link{extract.box}} to extract information about a 
#'    particular box in a \code{prim} object. \code{\link{plot_box}} to 
#'    visualize boxes. \code{\link{predict.prim}} to predict if new data
#'    falls into particular boxes.
#'
#' @references
#'    Friedman, J.H., Fisher, N.I., 1999. Bump hunting in high-dimensional data.
#'      Statistics and Computing 9, 123-143. 
#'      https://doi.org/10.1023/A:1008894516817
#'
#' @examples
#'    # A simple bump
#'    set.seed(12345)
#'    x <- matrix(runif(2000), ncol = 2, dimnames = list(NULL, c("x1", "x2")))
#'    y <- 2 * x[,1] + 5 * x[,2] + 10 * (x[,1] >= .8 & x[,2] >= .5) + rnorm(1000)
#'    # Peeling step
#'    peel_res <- peeling(y, x)
#'    # Pasting from the box with support 0.01
#'    paste_res <- pasting(peel_res, support = 0.01)
#'    # Visualize the peeled box and pasted one (npaste 0 and 2)
#'    plot_box(paste_res, pch = 16, ypalette = hcl.colors(10), npaste = c(0, 2), 
#'      box.args = list(lwd = 2, border = c("grey", "black"), lty = 1:2))
#'
#' @export
pasting <- function(object, npeel = NULL, support = NULL, 
  yfun = NULL, alpha = object$alpha, obj.fun = object$obj.fun, 
  peeling.side = object$peeling.side)
{
    if (!inherits(object, "prim")){
      warning("'object' should be of class 'prim'")
    }
    x <- object$x
    y <- object$y
    numeric.vars <- object$numeric.vars
    nnumeric <- sum(numeric.vars)
    peeling.side <- rep_len(peeling.side, nnumeric)
    npeel <- npeel[1]
    support <- support[1]
    yfun <- yfun[1]
    small.box <- extract.box(object, npeel = npeel, support = support, 
      yfun = yfun)
    if (length(c(npeel, support, yfun)) > 1){
      warning("Only one of 'npeel', 'support' or 'yfun' should be used.
        The one with highest npeel has been used.")
      small.box <- extract.box(object, npeel = max(small.box$npeel))
    }
    insmall <- in.box(x, small.box$limits[[1]])
    cur.sup <- small.box$support
    nstep <- ceiling(-log(cur.sup) / log(1 + alpha))
    object$support <- c(object$support[1:(small.box$npeel + 1)], 
      numeric(nstep))
    object$yfun <- c(object$yfun[1:(small.box$npeel + 1)], 
      numeric(nstep))
    object$limits <- c(object$limits[1:(small.box$npeel + 1)], 
      vector("list", nstep))
    object$npeel <- small.box$npeel
    npaste <- 1
    repeat{
        new.box <- paste.one(y, x, object$limits[[small.box$npeel + npaste]], 
          alpha, obj.fun, numeric.vars = numeric.vars, peeling.side)
        if (new.box$yfun > object$yfun[small.box$npeel + npaste]){
            npaste <- npaste + 1
            object$yfun[small.box$npeel + npaste] <- new.box$yfun
            object$limits[[small.box$npeel + npaste]] <- new.box$limits
            object$support[small.box$npeel + npaste] <- 
              length(new.box$y) / length(y)
        } else {
            break
        }
    }
    object$yfun <- object$yfun[1:(small.box$npeel + npaste)]
    object$support <- object$support[1:(small.box$npeel + npaste)]
    object$limits <- object$limits[1:(small.box$npeel + npaste)]
    object$npaste <- npaste - 1
    return(object)
}


paste.one <- function(y, x, small.box, alpha = 0.05, obj.fun = mean, 
  numeric.vars = rep(TRUE, ncol(x)), peeling.side = rep(0,sum(numeric.vars)))
{
    p <- ncol(x)
    n <- nrow(x)
    numinds <- which(numeric.vars)
    insmall <- finalobs <- in.box(x, small.box)
    nsmall <- sum(insmall)
    sup.small <- nsmall/n
    small.fun <- do.call(obj.fun, list(x = y[insmall]))
    yfun <- -Inf
    limits <- small.box
    for (j in 1:p){
        if (numeric.vars[j]){
          newn <- ceiling(nsmall * alpha)
          if (p > 1){
            eligible.obs <- in.box(x[,-j], small.box[-j])
          } else {
            eligible.obs <- rep(TRUE, n)
          }
          rankmin <- rank(x[eligible.obs,j], ties.method = "min")
          rankmax <- rank(x[eligible.obs,j], ties.method = "max")
          ranklims <- c(min(rankmin[insmall[eligible.obs]]), 
            max(rankmax[insmall[eligible.obs]]))
          newobs <- list(
            rankmax >= (ranklims[1] - newn) & rankmax <= ranklims[2], 
            rankmin >= ranklims[1] & rankmin <= (ranklims[2] + newn)
          )
          newobs <- lapply(newobs, function(obs){
            out <- insmall
            out[eligible.obs] <- obs
            return(out)
          })
          newobs <- newobs[peeling.side[numinds == j] != c(1, -1)]
          # Discard same boxes (when only one side can be extended)
          newobs <- newobs[sapply(newobs, sum) > nsmall] 
        } else {
          newvals <- setdiff(unique(x[,j]), small.box[[j]])
          newobs <- lapply(newvals, 
            function(v) x[,j] == v | insmall)
        }
        if (length(newobs) == 0) next
        for (k in 1:length(newobs)){
          jyfun <- do.call(obj.fun, list(x = y[newobs[[k]]]))
          if (jyfun > yfun){
            yfun <- jyfun
            finalobs <- newobs[[k]]
            limits <- small.box
            if (numeric.vars[j]){
              limits[[j]] <- range(x[finalobs, j])
            } else {
              limits[[j]] <- unique(x[finalobs, j])
            }    
          }
        }
    }       
    return(list(limits = limits, yfun = yfun, y = y[finalobs], 
      x = x[finalobs,,drop=F]))
}

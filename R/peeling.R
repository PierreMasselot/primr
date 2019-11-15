###############################################################################
#
#                   Peeling functions                             
#
###############################################################################

#' Top-down peeling
#'
#' Iteratively peels a dataset for bump hunting.
#'
#' @param y Numeric vector of response values.
#' @param x Numeric or categorical data.frame of input values.
#' @param alpha The peeling fraction of the algorithm. A value between 0 and 1
#'    giving the proportion of peeled observations at each step.
#' @param beta.stop The stopping support of the algorithm. A value 
#'    between 0 and 1 giving the proportion of remaining data below
#'    which the algorithm stops.
#' @param obj.fun The function of \code{y} to be maximized. Can be a user 
#'    defined function (see details).
#' @param peeling.side A numeric vector for side constraints on the peeling of 
#'    each input variable. -1 indicates peeling only the 'left' of the box
#'    (i.e. increasing the lower limit only), 1 indicate peeling only the
#'    'right' and 0 for no constraint.
#'
#' @details The function \code{peeling} carries out the top-down peeling 
#'    which is the first step of the PRIM algorithm. At each iteration 
#'    it peels a proportion \code{alpha} of data from one side of the domain
#'    in order to increase the value of the function \code{obj.fun} applied
#'    to the response \code{y}. The algorithm iterates the peeling until 
#'    the support of the box (i.e. the proportion of remaining observations)
#'    is below the value \code{beta.stop}.
#'
#'    Many function can be used in \code{obj.fun} including user defined 
#'    functions. User defined function should take two arguments: \code{y}
#'    and \code{x} representing corresponding variables 
#'    and \code{inbox} which is a boolean
#'    vector indicating the observations inside the current box.
#'    Note that a classical function can also be passed to \code{obj.fun}
#'    such as \code{mean}, \code{var} or \code{median}. In this case
#'    the function is created internally to fit the above structure. 
#'    For more functions more complicated than the basic ones,
#'    it is recommended that the user set its own function as stated
#'    above.  
#'    
#'    The function also allows directed peeling, i.e. to contraint the peeling
#'    occuring on a single side of some input variables. Thus when 
#'    \code{peeling.side = -1}, only the lower part of the variable is peeled
#'    (the "left" of the domain) and when \code{peeling.side = 1}, only the
#'    upper part of the variable is peeled. Note that a vector can be passed,
#'    thus applying different constraints to the input variables.
#'
#' @return A \code{prim} object which is a list with the following elements:
#'    \item{npeel}{The number of peeling iteration performed.}
#'    \item{support}{A vector of length \code{npeel + 1} containing the support 
#'      of each successivepeeled box.}
#'    \item{yfun}{A vector of length \code{npeel + 1} containing the objective 
#'      function value of each successive peeled box.}
#'    \item{limits}{A list of length \code{npeel + 1} containing the limits 
#'      of each successive box. Each limit is a list with one element per input 
#'      variable.}
#'    \item{x,y}{The input and response data used in the algorithm.}
#'    \item{numeric.vars}{A logical vector indicating, for each input variable,
#'      if it was considered as a numeric variable.}
#'    \item{alpha, peeling.side, obj.fun}{The value of the arguments used for
#'      peeling. Useful for prim methods.}
#'    \item{npaste}{Number of pasting iteration performed. Should be 0 here,
#'      but useful for \code{\link{pasting}}.}
#'
#'    Note that the first box in a \code{prim} object is the starting box
#'      containing the whole dataset. This is why the \code{limits},
#'      \code{yfun} and \code{support} elements have length \code{npeel + 1}.
#'
#' @seealso \code{\link{extract.box}} to extract information about a 
#'    particular box in a \code{prim} object. 
#'    \code{\link{plot_trajectory}} and \code{\link{plot_box}} to explore
#'    the peeling trajectory. \code{\link{jump.prim}} to automatically
#'    choose the best box. \code{\link{predict.prim}} to predict if new data
#'    falls into particular boxes. \code{\link{pasting}} to carry out the
#'    pasting refining the edges of the chosen box.
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
#'    y <- 2 * x[,1] + 5 * x[,2] + 10 * (x[,1] >= .8 & x[,2] >= .5) + 
#'      rnorm(1000)
#'    # Peeling with alpha = 0.05 and beta.stop = 0.05
#'    peel_res <- peeling(y, x, beta.stop = 0.05)
#'    # Automatically choose the best box
#'    chosen <- jump.prim(peel_res)
#'    # Plot the resulting box
#'    plot_box(peel_res, pch = 16, ypalette = hcl.colors(10), 
#'      support = chosen$final.box$support, box.args = list(lwd = 2))
#'
#'    # Examples of directed peeling
#'    set.seed(12345)
#'    x <- matrix(runif(2000), ncol = 2, dimnames = list(NULL, c("x1", "x2")))
#'    y <- 10 * (x[,1] <= .2 & x[,2] <= .2) + 10 * (x[,1] >= .8 & x[,2] >= .8) +
#'      rnorm(1000)
#'    # Left peeling
#'    peel_left <- peeling(y, x, peeling.side = -1)
#'    chosen <- jump.prim(peel_left)
#'    plot_box(peel_left, pch = 16, ypalette = hcl.colors(10), 
#'      support = chosen$final.box$support, box.args = list(lwd = 2),
#'      main = "Left peeling")
#'    # Right peeling
#'    peel_right <- peeling(y, x, peeling.side = 1)
#'    chosen <- jump.prim(peel_right)
#'    plot_box(peel_right, pch = 16, ypalette = hcl.colors(10), 
#'      support = chosen$final.box$support, box.args = list(lwd = 2),
#'      main = "Right peeling")
#'
#'    # User-defined objective function to minimize the mean
#'    set.seed(3333)
#'    x <- matrix(runif(2000), ncol = 2, dimnames = list(NULL, c("x1", "x2")))
#'    y <- - 10 * (x[,1] <= .2 & x[,2] <= .2) + 10 * (x[,1] >= .8 & x[,2] >= .8) +
#'      rnorm(1000)
#'    peel_res <- peeling(y, x, obj.fun = function(x) -mean(x))
#'    chosen <- jump.prim(peel_res)
#'    plot_box(peel_res, pch = 16, ypalette = hcl.colors(10), 
#'      support = chosen$final.box$support, box.args = list(lwd = 2))
#'
#'    # User-defined function maximizing the slope of a linear regression
#'    set.seed(5555)
#'    x <- runif(500)
#'    ym <- 0.5 * x + 5 * (x - 0.7) * (x >= 0.7)
#'    y <- ym + rnorm(500, sd = 0.1)    
#'    peel_res <- peeling(y, x, beta.stop = 0.1, 
#'      obj.fun = function(y, x, inbox){
#'        dat <- data.frame(y, x)
#'        coef(lm(y ~ x, data = dat[inbox,]))[2]
#'    })   
#'    par(mfrow = c(1,2))
#'    plot_trajectory(peel_res, type = "b", pch = 16, col = "cornflowerblue", 
#'      support = 0.3, abline.pars = list(lwd = 2, col = "indianred"))
#'    plot_box(peel_res, pch = 16, ypalette = hcl.colors(10), 
#'      support = 0.3, box.args = list(lwd = 2))
#'    lines(sort(x), ym[order(x)], col = "red", lwd = 2)
#'    
#' @export
peeling <- function(y, x, alpha = 0.05, beta.stop = 0.01, 
  obj.fun = mean, peeling.side = 0)  
{
    x <- as.data.frame(x)
    p <- ncol(x)
    n <- nrow(x)
    numeric.vars <- sapply(x, is.numeric)
    nnumeric <- sum(numeric.vars)
    peeling.side <- rep_len(peeling.side, nnumeric)
    nstep <- ceiling(log(beta.stop)/log(1-alpha))
    support <- yfun <- numeric(nstep + 1)
    limits <- vector("list", nstep+1)
    support[1] <- 1    
    obj.fun <- construct_objfun(obj.fun)
    yfun[1] <- do.call(obj.fun, list(y = y, x = x, inbox = rep(T, n)))
    limits[[1]] <- vector("list", p)
    limits[[1]][numeric.vars] <- lapply(x[,numeric.vars, drop = F], range)
    limits[[1]][!numeric.vars] <- lapply(x[,!numeric.vars, drop = F], levels)
    count <- 1
    while (support[count] > beta.stop){
      new.peel <- peel(y = y, x = x, alpha = alpha, obj.fun = obj.fun, 
        peeling.side = peeling.side, limits = limits[[count]], 
        numeric.vars = numeric.vars)
      count <- count + 1
      support[count] <- new.peel$support
      yfun[count] <- new.peel$yfun
      limits[[count]] <- new.peel$limits
      if (count == (nstep + 1) || 
        length(unique(y[in.box(x, limits[[count]])])) == 1){ 
          break
      }
    }
    if (count < nstep){
      irem <- count:nstep + 1
      support <- support[-irem]
      yfun <- yfun[-irem]
      limits <- limits[-irem]
    }  
    out <- list(npeel = count - 1, support = support, yfun = yfun, 
      limits = limits, x = x, y = y, numeric.vars = numeric.vars, alpha = alpha, 
      peeling.side = peeling.side, obj.fun = obj.fun,
      npaste = 0)
    class(out) <- "prim"
    return(out)
}

peel <- function(y, x, alpha = 0.05, obj.fun = mean, limits, 
  numeric.vars = rep(TRUE, ncol(x)), peeling.side = rep(0,sum(numeric.vars)))
{
    p <- ncol(x)
    n <- nrow(x)
    yfun <- -Inf
    nnumeric <- sum(numeric.vars)
    numinds <- which(numeric.vars)
    inbox <- in.box(x, limits)
    yin <- y[inbox]
    xin <- x[inbox,, drop = FALSE]
    for (j in 1:p){
      if (numeric.vars[j]){
        boxes <- list(c(alpha, 1), c(0, 1 - alpha))
        boxes <- boxes[peeling.side[numinds == j] != c(1, -1)]
      } else {
        if (length(limits[[j]]) > 1){
          boxes <- limits[[j]]
        } else {
          next
        }
      }            
      for (k in 1:length(boxes)){
        if (numeric.vars[j]){
          newlims <- stats::quantile(xin[,j], boxes[[k]])
          movinglim <- which(boxes[[k]] %in% c(alpha,1 - alpha))
          if (newlims[movinglim] == limits[[j]][movinglim]){ 
          # To manage the case in which there are many ties 
            newlims[movinglim] <- sort(unique(x[,j]), 
              decreasing = as.logical(movinglim - 1))[2]
          }
          inboxk <- x[,j] >= newlims[1] & x[,j] <= newlims[2]
        } else {
          inboxk <- x[,j] != boxes[k]
          newlims <- limits[[j]][-k]
        }
        jyfun <- do.call(obj.fun, list(y = y, x = x, inbox = inbox & inboxk))
        if (jyfun > yfun){
           finbox <- inbox & inboxk
           yfun <- jyfun
        }
      }      
    }
    # Final readjustment of limits
    final.x <- x[finbox,,drop = FALSE] 
    new.limits <- Map(function(xj, numj){
      if (numj){
        out <- range(xj)
      } else {
        out <- unique(xj)
      }
    }, final.x, numeric.vars)
    return(list(limits = new.limits, yfun = yfun, support = mean(finbox)))
}


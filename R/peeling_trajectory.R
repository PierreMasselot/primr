###############################################################################
#
#               Functions for peeling trajectory analysis                    
#
###############################################################################

#' Peeling trajectory jump
#'
#' Identifies a jump in the peeling trajectory of \code{object}.
#'
#' @param object A \code{prim} object resulting from a call to \code{peeling}.
#' @param rel.support Logical indicating if the trajectory difference 
#'    should be relative to the support for finding the jump (default to TRUE).
#'
#' @details Computes the (relative) trajectory differences of \code{object}:
#'    \deqn{\frac{yfun[k] - yfun[k - 1]}{support[k - 1] - support[k]}}{(yfun[k] - yfun[k - 1])/(support[k - 1] - support[k])}
#'    and returns its maximum value. The rationale is that the biggest jump
#'    in peeling trajectory gives a good cut-off point for the peeling 
#'    algorithm. 
#'    If \code{rel.support = FALSE}, the denominator is not used in the
#'    differences calculation.
#'
#' @return A list with elements:
#'    \item{trajectory.difference}{Numeric vector of the computed (relative) 
#'      differences.}
#'    \item{npeel.opt}{Integer giving the npeel value of the highest 
#'      difference.}
#'    \item{final.box}{The extracted box corresponding to \code{npeel.opt}.
#'      See \code{\link{extract.box}}.}
#' @references
#'    Masselot P., Chebana F., Campagna C., Lavigne E., Ouarda T.B.M.J., 
#'      Gosselin P. On threshold identification for weather-health warning 
#'      systems. \emph{Submitted}.
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
#'
#' @export
jump.prim <- function(object, rel.support = TRUE)
{
  traj.change <- diff(object$yfun)
  if (rel.support) traj.change <- traj.change / -diff(object$support)  
  opt.sup <- which.max(traj.change)
  optimal.box <- extract.box(object, npeel = opt.sup)
  optimal.box$limits <- optimal.box$limits[[1]]
  return(list(trajectory.difference = traj.change, npeel.opt = opt.sup,
    final.box = optimal.box))
}



#' Cross-validated peeling trajectory
#'
#' Performs k-fold cross-validation on peeling for choosing the stopping 
#'    criterion.
#'
#' @param y Numeric vector of response values.
#' @param x Numeric or categorical data.frame of input values.
#' @param beta.stop.grid Vector of stopping supports
#'    for peeling trajectory prediction.
#'    If NULL (the default), an initial peeling is carried out on the 
#'    whole data and its \code{support} element is used.
#' @param folds An integer vector giving the fold index of each observation.
#'    If NULL (the default)
#'    \code{nfolds} folds are randomly generated. Directly using \code{folds}
#'    is useful for nonstandard folds such as blocks. Note that \code{folds} is 
#'    recycled if necessary.
#' @param nfolds Integer giving the number of folds to create if \code{folds}
#'    is NULL.
#' @param ... Additional arguments to be passed to \code{\link{peeling}}.
#'
#' @details The \code{cv.trajectory} function splits the provided data into
#'    \code{nfolds} several folds. The peeling is carried out on 
#'    \code{nfolds - 1} folds and the objective function is computed on the
#'    remaining fold. This process is repeated excluding each fold successively
#'    and the resulting trajectories are averaged at each value in 
#'    \code{beta.stop.grid}.
#'
#'    Folds can be given either directly through the argument \code{folds} or 
#'    randomly generated using the argument \code{nfolds}. 
#'
#' @return A \code{cv.prim} object that can be used in methods for \code{prim}
#'    objects (e.g. \code{\link{plot_trajectory}}). Contains the elements:
#'    \item{support}{The support grid provided in the argument 
#'      \code{beta.stop.grid} or generated if the latter is \code{NULL}.}
#'    \item{yfun}{The cross-validated objective function values at each
#'      \code{support} value.}
#'    \item{se.yfun}{Cross-validation standard errors associated with 
#'      \code{yfun} values.}
#'    \item{x,y}{The input and response data used.}
#'    \item{numeric.vars}{A logical vector indicating, for each input variable,
#'      if it was considered as a numeric variable.}
#'    \item{alpha, peeling.side, obj.fun}{The value of the arguments used for
#'      peeling. Useful for prim methods.}
#' 
#' @seealso \code{\link{peeling}} for the peeling algorithm used in the
#'    function. \code{\link{plot_trajectory}} to analyse the cross-validated
#'    trajectory.
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
#'    
#'    # 10-fold cross-validation
#'    cv_res <- cv.trajectory(y, x)
#'    
#'    # Display the cross-validated trajectory
#'    plot_trajectory(cv_res, type = "b", pch = 16, col = "cornflowerblue", 
#'      support = 0.1, npeel = which.max(cv_res$yfun), 
#'      abline.pars = list(lwd = 2, col = "indianred"), 
#'      xlab = "", xlim = c(0, 0.2), ylim = c(10, 18))
#'
#' @export
cv.trajectory <- function(y, x, beta.stop.grid = NULL, 
  folds = NULL, nfolds = 10, ...)
{
  x <- as.data.frame(x)
  p <- ncol(x)
  n <- nrow(x)
  if (is.null(beta.stop.grid)){
    all_peel <- peeling(y, x, ...)
    beta.stop.grid <- all_peel$support
  }
  nsup <- length(beta.stop.grid)
  if (is.null(folds)){
    folds <- sample(rep_len(1:nfolds, n))
  } else {
    folds <- rep_len(folds, n)
  }
  fun.mat <- matrix(NA, nrow = nsup, ncol = nfolds)
  for (i in 1:nfolds){
      train_ind <- folds != i
      test_ind <- folds == i
      peeli <- peeling(y[train_ind], x[train_ind,], 
        beta.stop = min(beta.stop.grid), ...)
      traji <- predict.prim(peeli, newy = y[test_ind], newx = x[test_ind,],
        npeel = 0:peeli$npeel)        
      sup.inds <- sapply(beta.stop.grid, function(b){ 
        max(which(peeli$support >= b))
      })
      fun.mat[,i] <- traji$yfun[sup.inds]
  }
  final.traj <- apply(fun.mat, 1, mean, na.rm=T)
  n.traj <- apply(fun.mat, 1, function(x) sum(!is.na(x)))
  sd.traj <- apply(fun.mat, 1, stats::sd, na.rm=T)
  out <- list(support = beta.stop.grid, yfun = final.traj, se.yfun = sd.traj,
    npeel = nsup, x = x, y = y, alpha = peeli$alpha, 
    peeling.side = peeli$peeling.side, numeric.vars = peeli$numeric.vars,
    obj.fun = peeli$obj.fun)
  class(out) <- "cv.prim"
  return(out)
}

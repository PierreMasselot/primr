###############################################################################
#
#                      Plot functions                    
#
###############################################################################

#' Plot a box
#'
#' Plots a bidimensional projection of the data and chosen boxes.
#'
#' @param object A 'prim' object.
#' @param select A vector of length 1 or 2 indicating the input variables(s) 
#'    to plot. Default to the two first ones. 
#'    If a single variable is selected, plot it against \code{y}.
#' @param npeel Integer vector indicating the number of peeling iteration
#'    of boxes to plot.
#' @param support Numeric vector with values between 0 and 1 indicating the 
#'    support of boxes to plot.
#' @param yfun Numeric vector indicating the value of the objective function
#'    of the boxes to plot.
#' @param npaste Integer vector indicating the number of pasting iteration
#'    of boxes to plot.
#' @param ypalette A palette of colors for representing the \code{y} value
#'    associated to each point.
#' @param box.args A list of arguments to be passed to 
#'    \code{\link[graphics]{rect}} for drawing boxes. All arguments can be 
#'    given as vectors to specify different values for each drawn box. 
#' @param ... Additional graphical parameters for 
#'    \code{\link[graphics]{plot}}.
#'
#' @details Several boxes can be displayed at once, selected by one of the
#'    arguments \code{npeel}, \code{support}, \code{yfun} or \code{npaste}
#'    (when relevant). Note that the arguments in \code{box.args} allow
#'    giving different parameters to each displayed box.
#'
#' @seealso \code{\link{peeling}} for peeling trajectories. 
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
#' @export
plot_box <- function(object, select = 1:2, npeel = NULL, support = NULL, 
  yfun = NULL, npaste = NULL, ypalette = NULL, box.args = list(), ...)
{
  dots <- list(...)
  if (!inherits(object, "prim")){
    warning("'object' should be of class 'prim'")
  } 
  n <- nrow(object$x)
  p <- ncol(object$x)
  # Select the x to plot
  select <- select[1:pmin(length(select), 2, p)]
  xplot <- object$x[,select, drop = FALSE]
  labs <- colnames(xplot)
  # Extract boxes
  boxes <- extract.box(object, npeel = npeel, support = support, 
    yfun = yfun, npaste = npaste)
  nboxes <- length(boxes$yfun)
  boxlimits <- lapply(boxes$limits, "[", select)
  # If a single dimension is to be plotted
  if (length(select) == 1){
    xplot <- cbind(xplot, object$y, deparse.level = 0)
    labs[2] <- "y"
    ylims <- range(object$y) + diff(range(object$y)) * c(-0.02, 0.02)
    boxlimits <- lapply(boxlimits, append, list(ylims))
  }
  # Preparing default parameters
  if (!is.null(ypalette)){
    cols <- ypalette[cut(object$y, length(ypalette))]
  } else {
    cols <- "black"
  }
  dots.def <- list(xlab = labs[1], ylab = labs[2], col = cols)
  plot.args <- c(list(x = xplot), dots, 
    dots.def[!names(dots.def) %in% names(dots)])
  do.call(graphics::plot, plot.args)
  # Add boxes
  box.args <- lapply(box.args, rep_len, nboxes)
  box.args$xleft <- sapply(boxlimits, function(x) x[[1]][1])
  box.args$xright <- sapply(boxlimits, function(x) x[[1]][2])
  box.args$ybottom <- sapply(boxlimits, function(x) x[[2]][1])
  box.args$ytop <- sapply(boxlimits, function(x) x[[2]][2])
  box.args$f <- graphics::rect
  invisible(do.call(Map, box.args)) 
}



#' Plot a peeling trajectory
#'
#' Displays the peeling trajectory of a \code{prim} object with chosen
#'    cut-points.
#'
#' @param object A \code{prim} object.
#' @param xtype A character indicating the how to display the x axis of
#'    the plot. 'support' (the default) for the support of the boxes or
#'    'nobs' for the number of observations inside the box.
#' @param ytype A character indicating which trajectory to plot. 'yfun'
#'    (the default) for the objective function value, 'diff' for the
#'    difference between successive boxes and 'rel.diff' for the relative
#'    difference. See \code{\link{jump.prim}} for details.
#' @param npeel Integer vector. If not \code{NULL}, draw a vertical line 
#'    at the corresponding peeling iterations of the trajectory.
#' @param support Numeric vector. If not \code{NULL}, draw a vertical line 
#'    at the corresponding supports of the trajectory.
#' @param yfun Numeric vector. If not \code{NULL}, draw a vertical line 
#'    at the boxes with the closest \code{yfun} value.
#' @param npaste Integer vector. If not \code{NULL}, draw a vertical line 
#'    at the corresponding pasting iterations of the trajectory.
#' @param abline.pars List of parameters to be passed to
#'    \code{\link[graphics]{abline}} for the vertical lines.
#' @param se Logical indicating if standard error bars should be drawn.
#'    Works only for \code{cv.prim} objects and \code{ytype = "yfun"}.
#' @param se.pars Parameters to be passed to \code{arrows} for drawing
#'    strandard error bars.
#' @param ... Additional graphical parameters for 
#'    \code{\link[graphics]{plot}}.
#'
#' @seealso \code{\link{peeling}} for peeling trajectories. 
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
#'
#'    # Display the peeling trajectory
#'    plot_trajectory(peel_res, type = "b", pch = 16, col = "cornflowerblue", 
#'      support = 0.11, abline.pars = list(lwd = 2, col = "indianred"), 
#'      xlab = "")
#'
#' @export
plot_trajectory <- function(object, xtype = c("support", "nobs"), 
  ytype = c("yfun", "diff", "rel.diff"), npeel = NULL, support = NULL, 
  yfun = NULL, npaste = NULL, abline.pars = list(), 
  se = TRUE, se.pars = list(), ...)
{
  dots <- list(...)
  if (!inherits(object, c("prim", "cv.prim"))){
    warning("'object' should be of class 'prim' or 'cv.prim'")
  }
  n <- nrow(object$x)
  nbox <- object$npeel + 1
  dots.def <- list()
  if (length(c(npeel, support, yfun, npaste)) > 0){
    boxes <- extract.box(object, npeel = npeel, support = support, 
      yfun = yfun, npaste = npaste)
  } else {
    boxes <- NULL
  }
  ab.def <- list(v = boxes$support, lty = 2)
  xtype <- match.arg(xtype) 
  switch(xtype, 
    support = {
      x <- object$support
      dots.def$xlab <- "Support"
      ab.def$v <- boxes$support
    },
    nobs = {
      x <- round(object$support * n)
      dots.def$xlab <- "Number of observations"
      ab.def$v <- round(boxes$support * n)
    }
  )
  ytype <- match.arg(ytype)
  switch(ytype, 
    yfun = {
      y <- object$yfun
      dots.def$ylab <- object$obj.fun
    },
    diff = {
      y <- jump.prim(object, rel.support = F)$trajectory.difference
      x <- x[-1]
      dots.def$ylab <- "Difference"
    },
    rel.diff = {
      y <- jump.prim(object, rel.support = T)$trajectory.difference
      x <- x[-1]
      dots.def$ylab <- "Relative difference"
    }
  )
  plot.args <- c(list(x = x, y = y), dots, 
    dots.def[!names(dots.def) %in% names(dots)])
  do.call(graphics::plot, plot.args)
  if (se && inherits(object, "cv.prim")){
    se.def <- list(length = 0, col = "grey")
    se.pars <- c(list(x0 = x, y0 = y, 
      y1 = c(y + object$se.yfun, y - object$se.yfun)), 
      se.pars, se.def[!names(se.def) %in% names(se.pars)])
    do.call(graphics::arrows, se.pars)
  }
  if (length(c(npeel, support, yfun)) > 0){
    abline.pars <- c(abline.pars, 
      ab.def[!names(ab.def) %in% names(abline.pars)])
    do.call(graphics::abline, abline.pars)
  }  
}


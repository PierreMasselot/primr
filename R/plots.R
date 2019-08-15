
#' Plots a bidimensional projection of the data and the box.
#'
#' @param object A 'prim' object
#' @param select A vector of length 1 or 2 indicating the dimension(s) to plot.
#' @param ypalette A vector of color to plot according to the value of \code{y}.
#' @param box.args A list of arguments for the boxes, see 
#'    \code{\link[graphics]{rect}}. All arguments can be passed as vectors
#'    to have different boxes. 
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
  select <- select[1:pmin(length(select), 2)]
  xplot <- object$x[,select]
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
    ylims <- range(y) + diff(range(y)) * c(-0.02, 0.02)
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
  do.call(plot, plot.args)
  # Add boxes
  box.args <- lapply(box.args, rep_len, nboxes)
  box.args$xleft <- sapply(boxlimits, function(x) x[[1]][1])
  box.args$xright <- sapply(boxlimits, function(x) x[[1]][2])
  box.args$ybottom <- sapply(boxlimits, function(x) x[[2]][1])
  box.args$ytop <- sapply(boxlimits, function(x) x[[2]][2])
  box.args$f <- rect
  invisible(do.call(Map, box.args)) 
  
 #! ADD YFUN VALUE  
}


plot_trajectory <- function(object, xtype = c("support", "nobs"), 
  ytype = c("yfun", "diff", "rel.diff"), npeel = NULL, support = NULL, 
  yfun = NULL, npaste = NULL, abline.pars = list(), ...)
{
  dots <- list(...)
  if (!inherits(object, "prim")){
    warning("'object' should be of class 'prim'")
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
  do.call(plot, plot.args)
  if (length(c(npeel, support, yfun)) > 0){
    abline.pars <- c(abline.pars, 
      ab.def[!names(ab.def) %in% names(abline.pars)])
    do.call(abline, abline.pars)
  }  
}
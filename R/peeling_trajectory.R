#' @param rel.support Divide the trajectory difference by the relative change 
#'  in support?
jump.prim <- function(object, rel.support = T)
{
  traj.change <- diff(object$yfun)
  if (rel.support) traj.change <- traj.change / -diff(object$support)  
  opt.sup <- which.max(traj.change)
  optimal.box <- extract.box(object, npeel = opt.sup)
  return(list(trajectory.difference = traj.change, npeel.opt = opt.sup,
    final.box = optimal.box))
}

#' @param beta.stop.grid All support for which cross-validation computes a
#'    prediction. If NULL (the default), a peeling is carried out on the 
#'    whole data and the support sequence is used as \code{beta.stop.grid}.
#' @param folds An integer vector giving the fold index each iteration falls
#'    on. If NULL (the default)
#'    \code{nfolds} folds are randomly generated. Directly using \code{folds}
#'    is useful for nonstandard folds such as blocks. Note that provided 
#'    folds are recycled if the length of \code{folds} is different than
#'    the length of \code{y}.
#'
#' @return se.yfun is the standard error of mean yfun which is 
#'    sd(yfun) / #(nonNA)
cv.trajectory <- function(y, x, beta.stop.grid = NULL, 
  folds = NULL, nfolds = 10, ...)
# support.grid: list of support for which a value of yfun is wanted
{
  x <- as.data.frame(x)
  p <- ncol(x)
  n <- nrow(x)
  if (is.null(beta.stop.grid)){
    beta.stop.grid <- peeling.sequence(y, x, ...)$support
  }
  nsup <- length(beta.stop.grid)
  if (is.null(folds)){
    folds <- sample(rep_len(1:nfolds, n))
  } else {
    if (length(folds) != n) warning
  }   
  fun.mat <- matrix(NA, nrow = nsup, ncol = nfolds)
  for (i in 1:nfolds){
      train_ind <- folds != i
      test_ind <- folds == i
      peeli <- peeling.sequence(y[train_ind], x[train_ind,], 
        beta.stop = min(beta.stop.grid))#, ...)
      traji <- predict.prim(peeli, y = y[test_ind], x = x[test_ind,])        
      sup.inds <- sapply(beta.stop.grid, function(b){ 
        max(which(peeli$support >= b))
      })
      fun.mat[,i] <- traji$yfun[sup.inds]
  }
  final.traj <- apply(fun.mat, 1, mean, na.rm=T)
  n.traj <- apply(fun.mat, 1, function(x) sum(!is.na(x)))
  sd.traj <- apply(fun.mat, 1, sd, na.rm=T) / n.traj
  out <- list(beta.stop = beta.stop.grid, mean.yfun = final.traj, 
    se.yfun = sd.traj)
  class(out) <- "cv.prim"
  return(out)
}

#' @param trajectories A list of peeling trajectories from 
#'    \code{peeling.sequence}
thinning <- function(trajectories)
{
  ntr <- length(trajectories)
  for (i in 1:ntr){
    if (!inherits(trajectories[[i]], "prim")){
      warning(sprintf("Element %i in 'trajectory' is not a prim object", i))
    } 
  }
  nvec <- sapply(trajectories, "[[", "npeel") + 1
  nboxes <- sum(nvec)
  all.traj <- list(yfun = unlist(lapply(trajectories, "[[", "yfun")),
    support = unlist(lapply(trajectories, "[[", "support")),
    indtraj = rep(1:ntr, nvec), indbox = unlist(sapply(nvec, seq_len)))
  dominated <- with(all.traj, sapply(1:nboxes, function(i){
    any((all.yfun[-i] > all.yfun[i] & all.support[-i] >= all.support[i]) | 
      (all.yfun[-i] >= all.yfun[i] & all.support[-i] > all.support[i])
    )
  }))
  thinned <- lapply(all.traj, "[", !dominated)
  dup <- with(thinned, duplicated(cbind(yfun, support)))
  thinned <- lapply(thinned, "[", !dup)    
  ord <- order(thinned$support, decreasing = TRUE)
  thinned <- lapply(thinned, "[", ord)
  out <- c(thinned[1:2], list(Reduce(cbind, thinned[3:4])))
  colnames(out[[3]]) <- c("trajectory", "box.index")
  return(out)
}
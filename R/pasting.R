###############################################################################
#
#                   Pasting functions                             
#
###############################################################################

paste.one <- function(y, x, small.box, alpha = 0.05, obj.fun = mean, 
  numeric.vars = rep(TRUE, ncol(x)), peeling.side = rep(0,sum(numeric.vars)))
{
    p <- ncol(x)
    n <- nrow(x)
    numinds <- which(numeric.vars)
    insmall <- in.box(x, small.box)
    nsmall <- sum(insmall)
    sup.small <- nsmall/n
    small.fun <- do.call(obj.fun, list(x = y[insmall]))
    yfun <- min(y)
    for (j in 1:p){
        if (numeric.vars[j]){
          newn <- ceiling(nsmall * alpha)
          if (p > 1){
            eligible.obs <- in.box(x[,-j], small.box[-j])
          } else {
            eligible.obs <- rep(TRUE, n)
          }
          ranks <- rank(x[eligible.obs,j]) 
          ranklims <- range(ranks[insmall[eligible.obs]])
          newobs <- list(ranks >= (ranklims[1] - newn) & ranks <= ranklims[2], 
            ranks >= ranklims[1] & ranks <= (ranklims[2] + newn)
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

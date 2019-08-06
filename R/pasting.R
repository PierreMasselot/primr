###############################################################################
#
#                   Pasting functions                             
#
###############################################################################

pasting <- function(y, x, small.box, alpha = 0.05, obj.fun = mean, 
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
          ranks <- rank(x[,j]) 
          ranklims <- range(ranks[insmall])
          newobs <- list(ranks >= (ranklims[1] - newn) & ranks <= ranklims[2], 
            ranks >= ranklims[1] & ranks <= (ranklims[2] + newn)
          )
          newobs <- newobs[peeling.side[numinds == j] != c(1, -1)]
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

pasting.sequence <- function(y, x, small.box, alpha = 0.05, obj.fun = mean, 
  peeling.side = 0)
{
    x <- as.data.frame(x)
    p <- ncol(x)
    n <- nrow(x)
    numeric.vars <- sapply(x, is.numeric)
    nnumeric <- sum(numeric.vars)
    peeling.side <- rep_len(peeling.side, nnumeric)
    insmall <- in.box(x, small.box)
    cur.sup <- mean(insmall)
    nstep <- ceiling(-log(cur.sup) / log(1 + alpha))
    support <- yfun <- numeric(nstep)
    limits <- vector("list", nstep)
    support[1] <- cur.sup
    yfun[1] <- do.call(obj.fun, list(x = y[insmall]))
    limits[[1]] <- small.box
    npaste <- 1
    repeat{
        new.box <- pasting(y, x, limits[[npaste]], alpha, obj.fun, 
          numeric.vars = numeric.vars, peeling.side)
        if (new.box$yfun > yfun[npaste]){
            npaste <- npaste + 1
            yfun[npaste] <- new.box$yfun
            limits[[npaste]] <- new.box$limits
            support[npaste] <- length(new.box$y) / length(y)
        } else {
            break
        }
    }
    return(list(limits = limits[1:npaste], npaste = npaste - 1, 
      yfun = yfun[1:npaste], support =support[1:npaste]))
}

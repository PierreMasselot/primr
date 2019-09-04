###############################################################################
#
#                   Peeling functions                             
#
###############################################################################

# À ajouter:
#   - Several boxes (whole function)

#' Peels a box
#'
#' Peels one side of a dataset in order to maximize the objective function
#'  inside the resulting box. 
#'
#' @param y A vector containing the response variable.
#' @param x Data.frame -> higher level functions will make sure it is.
#' @param peeling.side -1 for left, 1 for right and 0 for both -> higher level functions will make sure it is a vector.
peel <- function(y, x, alpha = 0.05, obj.fun = mean, limits, 
  numeric.vars = rep(TRUE, ncol(x)), peeling.side = rep(0,sum(numeric.vars)))
# y: response
# x: covariates
# alpha: peeling parameter, proportion of observations removed each step
# obj.fun: the objective function on y to maximize
# peeling.side: which side of variable the peeling is allowed. "left" for left side only, "right" for right side only and "both" for both sides
{
    p <- ncol(x)
    n <- nrow(x)
    yfun <- -Inf
    nnumeric <- sum(numeric.vars)
    numinds <- which(numeric.vars)
    numeric_sublims <- c(alpha, 1 - alpha)
    for (j in 1:p){
      if (numeric.vars[j]){
        boxes <- list(c(0, 1 - alpha), c(alpha, 1))
        boxes <- boxes[peeling.side[numinds == j] != c(-1, 1)]
      } else {
        boxes <- limits[[j]]
      }            
      for (k in 1:length(boxes)){
        if (numeric.vars[j]){
          newlims <- quantile(x[,j], boxes[[k]])
          inbox <- x[,j] >= newlims[1] & x[,j] <= newlims[2]
        } else {
          inbox <- x[,j] != boxes[k]
          newlims <- limits[[j]][-k]
        }
        jyfun <- do.call(obj.fun, list(x = y[inbox]))
        if (jyfun > yfun){
           finbox <- inbox
           yfun <- jyfun
        }
      }      
    }
    # Final readjustment of limits
    final.x <- x[finbox,,drop=F] 
    new.limits <- Map(function(xj, numj){
      if (numj){
        out <- range(xj)
      } else {
        out <- unique(xj)
      }
    }, final.x, numeric.vars)
    return(list(limits = new.limits, yfun = yfun,
       y = y[finbox], x = final.x))
}

#' @param x Data.frame -> higher level functions will make sure it is.
peeling <- function(y, x, alpha = 0.05, beta.stop = 0.01, 
  obj.fun = mean, peeling.side = 0)
# y: response
# x: covariates
# alpha: peeling parameter, proportion of observations removed each step
# beta.stop: stopping criterion, support of the final box
# obj.fun: the objective function on y to maximize
# peeling.side: which side of variable the peeling is allowed. "left" for left side only, "right" for right side only and "both" for both sides
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
    yfun[1] <- do.call(obj.fun, list(x = y))
    limits[[1]] <- vector("list", p)
    limits[[1]][numeric.vars] <- lapply(x[,numeric.vars, drop = F], range)
    limits[[1]][!numeric.vars] <- lapply(x[,!numeric.vars, drop = F], levels)
    newx <- x
    newy <- y
    count <- 1
    while (support[count] > beta.stop){
        new.peel <- peel(y = newy, x = newx, alpha = alpha, obj.fun = obj.fun, 
          peeling.side = peeling.side, limits = limits[[count]], 
          numeric.vars = numeric.vars)
        newx <- new.peel$x
        newy <- new.peel$y
        count <- count + 1
        support[count] <- length(newy)/n
        yfun[count] <- new.peel$yfun
        limits[[count]] <- new.peel$limits
        if (count == (nstep + 1) || length(unique(newy)) == 1) break
    }
    irem <- count:nstep+1
    support <- support[-irem]
    yfun <- yfun[-irem]
    limits <- limits[-irem]
    out <- list(support = support, yfun = yfun, limits = limits, 
      npeel = count - 1, x = x, y = y, alpha = alpha, 
      peeling.side = peeling.side, numeric.vars = numeric.vars,
      npaste = 0, obj.fun = deparse(substitute(obj.fun)))
    class(out) <- "prim"
    return(out)
}

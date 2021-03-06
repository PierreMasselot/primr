% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peeling_trajectory.R
\name{jump.prim}
\alias{jump.prim}
\title{Peeling trajectory jump}
\usage{
jump.prim(object, rel.support = TRUE)
}
\arguments{
\item{object}{A \code{prim} object resulting from a call to \code{peeling}.}

\item{rel.support}{Logical indicating if the trajectory difference 
should be relative to the support for finding the jump (default to TRUE).}
}
\value{
A list with elements:
   \item{trajectory.difference}{Numeric vector of the computed (relative) 
     differences.}
   \item{npeel.opt}{Integer giving the npeel value of the highest 
     difference.}
   \item{final.box}{The extracted box corresponding to \code{npeel.opt}.
     See \code{\link{extract.box}}.}
}
\description{
Identifies a jump in the peeling trajectory of \code{object}.
}
\details{
Computes the (relative) trajectory differences of \code{object}:
   \deqn{\frac{yfun[k] - yfun[k - 1]}{support[k - 1] - support[k]}}{(yfun[k] - yfun[k - 1])/(support[k - 1] - support[k])}
   and returns its maximum value. The rationale is that the biggest jump
   in peeling trajectory gives a good cut-off point for the peeling 
   algorithm. 
   If \code{rel.support = FALSE}, the denominator is not used in the
   differences calculation.
}
\examples{
   # A simple bump
   set.seed(12345)
   x <- matrix(runif(2000), ncol = 2, dimnames = list(NULL, c("x1", "x2")))
   y <- 2 * x[,1] + 5 * x[,2] + 10 * (x[,1] >= .8 & x[,2] >= .5) + 
     rnorm(1000)
   # Peeling with alpha = 0.05 and beta.stop = 0.05
   peel_res <- peeling(y, x, beta.stop = 0.05)
   # Automatically choose the best box
   chosen <- jump.prim(peel_res)

}
\references{
Masselot P., Chebana F., Campagna C., Lavigne E., Ouarda T.B.M.J., 
     Gosselin P. On threshold identification for weather-health warning 
     systems. \emph{Submitted}.
}

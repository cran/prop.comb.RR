\name{summary.prop.RR}
\alias{summary.prop.RR}
\title{Summary for a prop.RR object}
\description{
Provides results for an object of class \code{prop.RR}. It shows the contingency table with observed data, the parameter of interest and its estimated value. It provides confidence intervals (lower limit, upper limit) and p-values of the test (with z-values of statistics) for methods mentioned in references. It allows to calculate coverage probabilities for each method. It also indicates the alternative hypothesis and the recommendation about the optimal method for that particular case.
}
\usage{
\method{summary}{prop.RR}(object, ...)
}

\arguments{ 
\item{object}{an object of class \code{prop.RR}.} 
\item{...}{ Further arguments for summary. }
}

\author{Maria Alvarez Hernandez and Javier Roca Pardinas}

\seealso{\code{\link{prop.RR}}}

\examples{
A <-prop.RR(x=c(11, 48), n=c(46, 102), conf.level=0.99)
summary(A)

B <-prop.RR(x=c(7, 14), n=c(114, 116), rho=2)
summary(B)
}
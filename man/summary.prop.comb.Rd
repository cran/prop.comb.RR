\name{summary.prop.comb}
\alias{summary.prop.comb}
\title{Summary for a prop.comb object}
\description{
Provides results for an object of class \code{prop.comb}. It shows the contingency table with observed data, the parameter of interest and its estimated value. It provides confidence intervals (lower limit, upper limit) and p-values of the test (with z-values of statistics) for methods mentioned in references. It also indicates the alternative hypothesis and the recommendation about the optimal method for that particular case.
}

\usage{
\method{summary}{prop.comb}(object, ...)
}

\arguments{ 
\item{object}{an object of class \code{prop.comb}.}
  \item{...}{ Further arguments for summary. }
 }

\author{Maria Alvarez Hernandez and Javier Roca Pardinas}

\seealso{\code{\link{prop.comb}}}

\examples{
# One proportion
A <-prop.comb(x=16, n=109, alternative="two.sided", conf.level=0.95)
summary(A)

# Difference of proportions
B <-prop.comb(x=c(83,69), n=c(88,76), alternative="two.sided", conf.level=0.95)
summary(B)

# Linear combination of two proportions
C <-prop.comb(x=c(44, 3), n=c(54, 58), a=c(-0.4, 0.6), alternative="two.sided", 
conf.level=0.95)
summary(C)

# Linear combination of k>2 proportions
D <-prop.comb(x=c(73,32,44,34,104), n=c(158,107,175,92,143), 
a=c(158/675,107/675,175/675,92/675,143/675))
summary(D)
}
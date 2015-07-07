\name{prop.RR}
\alias{prop.RR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{prop.RR}
\description{Main function for making inferences (confidence intervals and tests) about the relative risk using optimal methods from the literature and score method.}
\usage{
prop.RR(x, n, rho = NULL, alternative = c("two.sided", "less", "greater"),
 conf.level = 0.95)
}
\arguments{
  \item{x}{a vector of counts of successes.}
  \item{n}{a vector of counts of samples sizes.}
  \item{rho}{hypothesized true value of the relative risk.}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".}
  \item{conf.level}{confidence level of the returned confidence interval.}
}
\value{
Returns a list with the following components:
\item{estimate }{a vector with the sample proportions x/n.}
\item{RR }{estimated relative risk.}
\item{inference }{confidence intervals (lower limit, upper limit) and p-values of the test (with z-values of statistics.}
\item{alternative }{a character string describing the alternative hypothesis.}
\item{rho }{hypothesized true value of the relative risk.}
\item{x }{number of successes.}
\item{n }{number of trials.}
\item{conf.level }{confidence level of the confidence interval.}
\item{recommendation }{recommended method by references.}
}


\references{
Woolf, B. (1955). "On estimating the realtion between blood group disease." Annals  of Human Genetics 19, 25-352.

Martin Andres, A. & Alvarez Hernandez, M. (2014). "Two-tailed approximate  confidence intervals for the ratio of proportions." Statistics and Computing 24, 65  - 75.  

Alvarez Hernandez, M. &  Martin Andress, A. (2015). "New asymptotic inferences  about the difference, ratio and linear combination of two independent proportions."  Communications in Statistics - Simulation and Computation (in press).
}

\author{Maria Alvarez Hernandez and Javier Roca Pardinas}


\seealso{\code{\link{prop.comb}} for inferences about a linear combination of K  proportions}


\examples{
# The Relative Risk was used by Maxwell (1961) for the following data related to 
# the rate of occurrence of virus infection among the group of the non-inoculated 
# and the group of the inoculated. The objetive is to obtain an approximate 
# confidence interval for RR.

prop.RR(x=c(11, 48), n=c(46, 102), conf.level=0.99)

# Price and Bonnet (2008) reviewed a study in which the aim is to prove if
# the effect of the beta-blocker could be highly beneficial or slightly detrimental.

x=c(7, 14); n=c(114, 116); prop.RR(x, n, rho=2)
}







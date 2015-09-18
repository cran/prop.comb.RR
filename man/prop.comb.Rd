\name{prop.comb}
\alias{prop.comb}
\title{prop.comb}
\description{Main function for making inferences (confidence intervals and tests) about a linear combination of proportions using optimal methods from the literature and score method.}
\usage{prop.comb(x, n, p = NULL, a = NULL, 
alternative = c("two.sided", "less", "greater"), conf.level = 0.95)}

\arguments{
  \item{x}{a vector of counts of successes.}
  \item{n}{a vector of counts of samples sizes.}
  \item{p}{hypothesized true value of the interest parameter.} 
  \item{a}{coefficients of the linear combination of proportions.}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".}
  \item{conf.level}{confidence level of the returned confidence interval.} 
}
\details{
Counts of successes must be nonnegative and hence not greater than the corresponding numbers of trials which must be positive. All finite counts should be integers. 


It checks how many samples there are (\code{k}). When \code{k=1}, it will be the case of one proporcion. When \code{k=2} and \code{a=(-1,1)}, we will have the difference of proportions. In another case, the parameter of interest will be a linear combination of \code{k} proportions.


When \code{k=1}, automatically \code{a=1} and the main function will assign \code{p=0.5} if \code{p=NULL}. If \code{k=2} and \code{a=NULL}, automatically \code{a=c(-1,+1)} and the main function will assign \code{p=0} if \code{p=NULL}, i.e. test of homogeneity of proportions.


If \code{alternative} is \code{NULL}, the main function will automatically assign \code{alternative="two.sided"}.
If \code{conf.level} is \code{NULL}, the main function will automatically assign \code{conf.level=0.95}.
}
\value{
Returns a list with the following components:

\item{estimate }{a vector with the sample proportions x/n.}
\item{a }{coefficients of the linear combination of proportions.}
\item{L }{estimated value of the interest parameter. \code{L=difference} for the d case.}
\item{inference }{confidence intervals (lower limit, upper limit) and p-values of the test (with z-values of statistics.)}
\item{alternative }{a character string describing the alternative hypothesis.}
\item{lambda }{hypothesized true value of the interest parameter.}
\item{k }{number of samples.}
\item{x }{number of successes.}
\item{n }{number of trials.}
\item{p }{same value of \code{lambda}}
\item{conf.level }{confidence level of the confidence interval.}
\item{recommendation }{recommended method by references.}
}
\references{
Wilson, E. (1927). "Probable inference, the law of succession, and statistical inference." Journal of the American Statistical Association, 22, 209 - 212

Agresti, A. & Coull, B. A. (1998). "Approximate is better than "exact" for interval estimation of binomial proportions." The American Statistician, 52, 119 - 126

Herranz, I. & Martin, A. (2008). "A numerical comparison of several unconditional exact tests in problems of equivalence based on the difference of proportions." Journal of Statistical Computation and Simulation, 78, 969 - 981

Martin, A.; Alvarez, M. & Herranz, I. (2011). "Inferences about a linear combination of proportions." Statistical Methods in Medical Research, 2011, 20, 369 - 387

Martin, A.; Herranz, I. & Alvarez, M. (2012). "The optimal method to make inferences about a lineal combination of proportions." Journal of Statistical Computation and Simulation, 82, 123 - 135

Martin, A.; Alvarez, M. & Herranz, I. (2012). "Asymptotic two-tailed confidence intervals for the difference of proportions." Journal of Applied Statistics, 39, 1423 - 1435

Martin, A. & Alvarez, M. (2013). "Optimal method for realizing two-sided inferences about a linear combination of two proportions." Communications in Statistics - Simulation and Computation, 42, 327 - 343

Martin, A. & Alvarez, M. (2014). "Two-tailed asymptotic inferences for a proportion." Journal of Applied Statistics, 41, 1516 - 1529

Yu, W.; Guo, X. & Xu, W. (2014). "An improved score interval with a modified midpoint for a binomial proportion." Journal of Statistical Computation and Simulation, 84, 1022 - 1038

Alvarez, M. & Martin, A. (2015). "New asymptotic inferences about the difference, ratio and linear combination of two independent proportions." Communications in Statistics - Simulation and Computation (in press).

}

\author{Maria Alvarez Hernandez and Javier Roca Pardinas}

\seealso{\code{\link{prop.RR}} for inferences about the relative risk}


\examples{
# ONE PROPORTION
# Yu et al (2014) use a data set, which describes characteristics of some 
# American bridges. The objetive is to construct CIs for the proportion 
# of wood bridges.

prop.comb(x=16, n=109, alternative="two.sided", conf.level=0.95)

# DIFFERENCE OF TWO PROPORTION
# Rodary et al. (1989) studied the response to chemotherapy and radiation 
# therapy in a randomized clinical trial on nephroblastoma. 
# The main objetive is to contruct CIs about the difference of the response 
# and to contrast the homogeneity of proportions.

prop.comb(x=c(83,69), n=c(88,76), alternative="two.sided", conf.level=0.95)

# COMBINATION OF TWO PROPORTIONS
# Martin and Alvarez (2013) construct CIs about the parameter "loss associated
# with a diagnostic test" in the context of comparing two diagnostic tests 
# (Bloch, 1997) and use a data set by Hanley and McNeil (1983). 

prop.comb(x=c(44, 3), n=c(54, 58), a=c(-0.4, 0.6), 
alternative="two.sided", conf.level=0.95)

# Tebbs and Roths (2008) refer to data about a multicenter clinical trial whose
# aim was to evaluate the efficacy of a lowered salt regim in the treatment of male 
# infants with acute diarrhea. The objetive it to obtain a confidence interval about 
# the proportion of fever cases in South American area. 

prop.comb(x=c(32, 34), n=c(107, 92), a=c(107/199, 92/199), 
alternative="two.sided", conf.level=0.95)


# LINEAR COMBINATION OF K>3 PROPORTIONS
# Price and Bonett (2004) refer to a study by Cohen et al. (1991) in which 120 rats 
# were randomly assigned four diets (high or low fat and with or without fiber).
# The absence or presence of a tumor was recorded for each rat.
# The contrast of interest will evaluate the effects of study's variables.

# Data of diet and tumor study:
x <-c(20, 14, 27, 19); n <-c(30, 30, 30, 30)
a1 <-c(+1, -1, -1, +1); a2 <-c(+1, +1, -1, -1); a3 <-c(+1, -1, +1, -1)

prop.comb(x, n, a=a1); prop.comb(x, n, a=a2); prop.comb(x, n, a=a3)
}



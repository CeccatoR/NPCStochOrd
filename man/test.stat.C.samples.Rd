\name{test.stat.C.samples}
\alias{test.stat.C.samples}
\title{C-sample NPC-based test}
\usage{
test.stat.C.samples(y, ord, statistic = "dm", B = 2000, comb.fun = "F", seed = 1234)
}
\description{
Performs a NonParametric Combination (NPC) methodology-based test to address a \emph{C}-sample stochastic ordering problem in the presence of univariate and multivariate outcomes.
}
\arguments{
\item{y}{data.frame containing group labels (in the first column) and \eqn{V \ge 1} outcome variables.}
\item{ord}{vector containing the specific order of the group labels we want to test. c("A","B","C") stands for \eqn{A \ge B \ge C} (with at least a strict inequality).}
\item{statistic}{test statistic. "dm" (default) for difference in mean, "ad" for Anderson-Darling test statistic.}
\item{B}{number of permutations (\eqn{B \ge 2000} is recommended).}
\item{comb.fun}{combining functio. "F" (default) for Fisher's, "T" for Tippet's, or "L" for Liptak's.}
\item{seed}{the seed to be set for reproducibility.}
}
\value{
\item{P.comb}{global p-value.}
\item{P.part}{partial p-values.}
}
\references{
Pesarin, F., & Salmaso, L. (2010). \emph{Permutation tests for complex data: theory, applications and software}. John Wiley & Sons.

Arboretti, R., Ceccato, R., & Salmaso, L. (2021). Permutation testing for goodness-of-fit and stochastic ordering with multivariate mixed variables. \emph{Journal of Statistical Computation and Simulation}, 91(5), 876-896.
}
\author{
Riccardo Ceccato, Luigi Salmaso
}
\seealso{
\code{\link{test.stat.2.samples}}
}
\examples{
data("iris")
dt <- iris[,c(5,1:4)]

#Test virginica > versicolor > setosa
test.stat.C.samples(dt, ord = c("virginica", "versicolor", "setosa"), statistic = "ad", B = 2000, comb.fun = "F", seed = 1234)
}

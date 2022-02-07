\name{test.stat.2.samples}
\alias{test.stat.2.samples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute p-values to solve a 2-sample stochastic ordering problem
}
\description{
Compute p-values to solve a 2-sample stochastic ordering problem using the NonParametric Combination methodology.
}
\usage{
test.stat.2.samples(y, statistic = "dm", alternative = 1, B = 2000, comb.fun = "F")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{y}{data.frame containing group labels (in the first column) and V outcome variables.}
\item{statistic}{test statistic. "dm" (default) for difference in mean, "ad" for Anderson-Darling test statistic.}
\item{alternative}{an integer specifying the alternative hypothesis, must be 1 ("greater") or -1 ("less"). Default: 1.)}
\item{B}{number of permutations (B > 2000 is recommended).}
\item{comb.fun}{combining function to be used. "F" (default) for Fisher's, "T" for Tippet's, or "L" for Liptak's.}
\item{seed}{the seed to be set for reproducibility.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{test.stat.C.samples}}
}
\examples{
library(mvtnorm)

set.seed(125)
ni <- 30
Sigma <- matrix(0.25,ncol=3,nrow=3)
diag(Sigma) <- 1
muA <- c(10,7,5)
muB <- c(8,7,2)
dt <- data.frame(group = rep(c("A","B"),each = ni),
                 rbind(rmvnorm(ni,mean=muA,sigma=Sigma),
                       rmvnorm(ni,mean=muB,sigma=Sigma)))
#A>B
test.stat.2.samples(dt, statistic = "dm", alternative = 1, B = 2000, comb.fun = "F")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

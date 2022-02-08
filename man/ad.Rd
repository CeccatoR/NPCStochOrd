\name{ad}
\alias{ad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Anderson-Darling test statistic
}
\description{
Computes Anderson-Darling test statistic when the alternative hypothesis involves assuming that Group 1 stochastically dominates Group 2.
}
\usage{
ad(x,label)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{outcome variable.}
\item{label}{group labels. 1 and 2 need to be used to indicate the first and the second group respectively.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Test statistic.
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
\code{\link{dm}}
}
\examples{
library(mvtnorm)

set.seed(125)

xA<-rnorm(20,10,1)
xB<-rnorm(20,6,1)

#A>B
ad(c(xA,xB),c(rep(1,length(xA)),rep(2,length(xB))))
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

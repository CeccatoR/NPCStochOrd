\name{comb}
\alias{comb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine partial p-values
}
\description{
Combine partial p-values.
}
\usage{
comb(pv, fcomb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pv}{
array of partial p-values}
  \item{fcomb}{
combining function to be used. "F"=Fisher, "L"=Liptak, or "T"=Tippet.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Combined p-values.
}
\references{
%% ~put references to the literature/web site here ~
}
\references{
Pesarin, F., & Salmaso, L. (2010). \emph{Permutation tests for complex data: theory, applications and software}. John Wiley & Sons.
}
\author{
Riccardo Ceccato, Luigi Salmaso
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (pv, fcomb)
{
    if (fcomb == "F") {
        pc <- apply(pv, 1, function(x) -2 * log(prod(x, na.rm = TRUE)))
    }
    if (fcomb == "L") {
        pc <- apply(pv, 1, function(x) sum(qnorm(1 - x), na.rm = TRUE))
    }
    if (fcomb == "T") {
        pc <- apply(pv, 1, function(x) max((1 - x), na.rm = TRUE))
    }
    return(pc)
  }
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

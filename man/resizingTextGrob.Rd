\name{resizingTextGrob}
\alias{resizingTextGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
draw scalable text in grid context
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
resizingTextGrob(..., scale.fact = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{scale.fact}{
%%     ~~Describe \code{scale.fact} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (..., scale.fact = 1) 
{
    grob(tg = textGrob(...), cl = "resizingTextGrob", scale.fact = scale.fact)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

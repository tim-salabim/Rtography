\name{gridNorthArrow}
\alias{gridNorthArrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
draw a north arrow
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gridNorthArrow(..., scale.fact = 1, rot = 0, l.width = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{scale.fact}{
%%     ~~Describe \code{scale.fact} here~~
}
  \item{rot}{
%%     ~~Describe \code{rot} here~~
}
  \item{l.width}{
%%     ~~Describe \code{l.width} here~~
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
function (..., scale.fact = 1, rot = 0, l.width = 2) 
{
    library(gridExtra)
    library(scales)
    resizingTextGrob <- function(..., scale.fact = 1) {
        grob(tg = textGrob(...), cl = "resizingTextGrob", scale.fact = scale.fact)
    }
    drawDetails.resizingTextGrob <- function(x, scale.fact, recording = TRUE) {
        grid.draw(x$tg)
    }
    preDrawDetails.resizingTextGrob <- function(x, ...) {
        h <- convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)
        fs <- rescale(h, to = c(80, 15), from = c(120, 20)) * 
            x$scale.fact
        pushViewport(viewport(gp = gpar(fontsize = fs)))
    }
    postDrawDetails.resizingTextGrob <- function(x) popViewport()
    pushViewport(viewport(angle = -rot))
    grid.polyline(x = c(0.5, 0.5, 0.4, 0.6), y = c(0.1, 0.9, 
        0.55, 0.55), gp = gpar(lwd = l.width, lineend = "butt", 
        linejoin = "mitre"))
    arrow.n <- resizingTextGrob("N", x = 0.5, y = 0.4, scale.fact = 1)
    grid.draw(arrow.n)
    popViewport()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

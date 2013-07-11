\name{gridMapLegend}
\alias{gridMapLegend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
produce a map legend
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gridMapLegend(labs, clrs = NULL, type = "circles")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labs}{
%%     ~~Describe \code{labs} here~~
}
  \item{clrs}{
%%     ~~Describe \code{clrs} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (labs, clrs = NULL, type = "circles") 
{
    library(grid)
    grid.rect(gp = gpar(fill = "white", alpha = 0.7, col = "black"))
    n <- length(unique(labs))
    if (is.null(clrs)) 
        clrs <- hcl(h = seq(0, 315, length.out = n), c = 60, 
            l = 50, fixup = TRUE)
    if (n == 1) 
        ypos <- 0.5
    else ypos <- seq(0.95, 0.05, length.out = n + 2)
    if (n == 1) 
        ypos <- ypos
    else ypos <- ypos[-c(1, length(ypos))]
    xpos.pts <- unit(0.1, "npc")
    size.pts <- 0.5/n
    for (i in 1:n) {
        vp <- viewport(x = xpos.pts, y = ypos[i], height = size.pts, 
            width = 0.1, just = c("left", "centre"))
        pushViewport(vp)
        switch(type, circles = grid.circle(gp = gpar(fill = clrs[i], 
            col = "black")), rectangles = grid.rect(gp = gpar(fill = clrs[i], 
            col = "black")), lines = grid.lines(gp = gpar(col = clrs[i], 
            lwd = 5)))
        upViewport()
    }
    xpos.txt <- unit(0.25, "npc")
    width.txt <- 0.7
    for (i in 1:n) {
        vp <- viewport(x = xpos.txt, y = ypos[i], height = size.pts, 
            width = width.txt, just = c("left", "centre"))
        pushViewport(vp)
        txt <- resizingTextGrob(x = 0, sort(unique(labs))[i], 
            scale.fact = 2.5, just = "left")
        grid.draw(txt)
        popViewport()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

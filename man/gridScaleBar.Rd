\name{gridScaleBar}
\alias{gridScaleBar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
draw a scale bar based on lattice plot object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gridScaleBar(lattice.obj, addParams = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lattice.obj}{
%%     ~~Describe \code{lattice.obj} here~~
}
  \item{addParams}{
%%     ~~Describe \code{addParams} here~~
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
function (lattice.obj, addParams = list()) 
{
    library(gridExtra)
    library(scales)
    library(plyr)
    createBoxPolygon <- function(llcorner, width, height) {
        relativeCoords <- data.frame(c(0, 0, width, width, 0), 
            c(0, height, height, 0, 0))
        names(relativeCoords) = names(llcorner)
        return(t(apply(relativeCoords, 1, function(x) llcorner + 
            x)))
    }
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
    cvp <- current.viewport()
    addParamsDefaults = list(noBins = 5, unit = "meters", placement = "centre", 
        vpwidth = as.numeric(cvp$width), vpheight = 0.1, sbHeightvsWidth = 1/10)
    addParams <- modifyList(addParamsDefaults, addParams)
    range_x_dim <- nchar(as.integer(abs(diff(lattice.obj$x.limits)))) - 
        1
    range_y_dim <- nchar(as.integer(abs(diff(lattice.obj$y.limits)))) - 
        1
    range_x_nice <- list()
    range_x_nice$up <- round_any(abs(diff(lattice.obj$x.limits)), 
        10^range_x_dim, ceiling)
    range_x_nice$dwn <- round_any(abs(diff(lattice.obj$x.limits)), 
        10^range_x_dim, floor)
    rnd <- vector("numeric", 2)
    rnd[1] <- range_x_nice$up/abs(diff(lattice.obj$x.limits))
    rnd[2] <- range_x_nice$dwn/abs(diff(lattice.obj$x.limits))
    ind <- which(abs(1 - rnd) == min(abs(1 - rnd)))
    adj <- rnd[[ind]]
    length.scalebar <- 0.8 * adj
    range_x <- range_x_nice[[ind]] * length.scalebar * addParams[["vpwidth"]]/adj
    widthBin <- length.scalebar/addParams[["noBins"]]
    heightBin <- length.scalebar * addParams[["sbHeightvsWidth"]]
    lower.left.corner.scaleBar <- c(x = unit(0.5 - length.scalebar * 
        0.5, "npc"), y = unit(0.5, "npc"))
    scale.bar.polygon <- do.call("rbind", lapply(0:(addParams[["noBins"]] - 
        1), function(n) {
        dum <- data.frame(createBoxPolygon(lower.left.corner.scaleBar + 
            c((n * widthBin), 0), widthBin, heightBin))
        if (!(n + 1)\%\%2 == 0) 
            dum$cat = "odd"
        else dum$cat = "even"
        return(dum)
    }))
    grid.polygon(scale.bar.polygon$x, scale.bar.polygon$y, id.lengths = rep(5, 
        addParams[["noBins"]]), gp = gpar(fill = c("black", "white"), 
        lwd = 1))
    scale.labs <- resizingTextGrob(c(0, round(cumsum(rep(range_x/addParams[["noBins"]], 
        addParams[["noBins"]])))), x = unique(round(scale.bar.polygon$x, 
        7)), y = unit(0.4, "npc"), just = "top")
    scale.main <- resizingTextGrob(addParams[["unit"]], x = 0.5, 
        y = 0.75)
    grid.draw(scale.labs)
    grid.draw(scale.main)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

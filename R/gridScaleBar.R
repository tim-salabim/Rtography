gridScaleBar <- function(lattice.obj, addParams = list(), ...) {

  library(gridExtra)
  library(plyr)
  
  ### some helper functions especially for the grid layout
  ### resizingTextGrob from 
  ### http://ryouready.wordpress.com/2012/08/01/
  createBoxPolygon <- function(llcorner, width, height) {
    
    relativeCoords <- data.frame(c(0, 0, width, width, 0), 
                                 c(0, height, height, 0, 0))
    
    names(relativeCoords) = names(llcorner)
    
    return(t(apply(relativeCoords, 1, function(x) llcorner + x)))
    
  }

  cvp <- current.viewport()
  
  addParamsDefaults = list(noBins = 5, unit = "meters", 
                           placement = "centre", 
                           vpwidth = as.numeric(cvp$width), 
                           vpheight = 0.1, sbHeightvsWidth = 1/10)
  
  addParams <- modifyList(addParamsDefaults, addParams)
  
  range_x_dim <- nchar(as.integer(abs(diff(lattice.obj$x.limits)))) - 1
  range_y_dim <- nchar(as.integer(abs(diff(lattice.obj$y.limits)))) - 1
  
  range_x_nice <- list()
  range_x_nice$up <- round_any(abs(diff(lattice.obj$x.limits)),
                               10^range_x_dim, ceiling)
  range_x_nice$dwn <- round_any(abs(diff(lattice.obj$x.limits)),
                                10^range_x_dim, floor)
  
  rnd <- vector("numeric", 2)
  rnd[1] <- range_x_nice$up / abs(diff(lattice.obj$x.limits))
  rnd[2] <- range_x_nice$dwn / abs(diff(lattice.obj$x.limits))
  
  ind <- which(abs(1 - rnd) == min(abs(1 - rnd)))
  adj <- rnd[[ind]]
  
  length.scalebar <- 0.8 * adj
  
  range_x <- range_x_nice[[ind]] * length.scalebar * 
    addParams[["vpwidth"]] / adj
  
  widthBin <- length.scalebar / addParams[["noBins"]]
  heightBin <- length.scalebar * addParams[["sbHeightvsWidth"]]
  lower.left.corner.scaleBar <- c(x = unit(0.5 - length.scalebar * 0.5, 
                                           "npc"),
                                  y = unit(0.5, "npc"))
  
  scale.bar.polygon <- do.call(
    "rbind", lapply(0:(addParams[["noBins"]] - 1), 
                    function(n) {
                      dum <-  data.frame(createBoxPolygon(
                        lower.left.corner.scaleBar + c((n * widthBin), 0), 
                        widthBin, heightBin))
                      if(!(n + 1) %% 2 == 0) dum$cat = "odd" else 
                        dum$cat = "even"
                      return(dum)
                      }))
  
  grid.polygon(scale.bar.polygon$x, scale.bar.polygon$y, 
               id.lengths = rep(5, addParams[["noBins"]]), 
               gp = gpar(fill = c("black", "white"), lwd = 1))
  
  scale.labs <- resizingTextGrob(
    c(0, round(cumsum(rep(range_x / addParams[["noBins"]], 
                          addParams[["noBins"]])))), 
    x = unique(round(scale.bar.polygon$x, 7)), 
    y = unit(0.4, "npc"), just = "top")
  
  scale.main <- resizingTextGrob(addParams[["unit"]], 
                                 x = 0.5, y = 0.75, ...)
  
  grid.draw(scale.labs)
  grid.draw(scale.main)
  
}

gridMapLegend <- function(labs, clrs = NULL, 
                          type = c("circles", "rectangles", "lines"),
                          ...) {
  
  library(grid)
  
  type <- type[1]
  
  grid.rect(gp = gpar(fill = "white", alpha = 0.7, col = "black"))
  
  n <- length(unique(labs))
  
  if(is.null(clrs)) clrs <- hcl(h = seq(0, 315, length.out = n), 
                                c = 60, l = 50, fixup = TRUE)
  
  if(n == 1) ypos <- 0.5 else ypos <- seq(0.95, 0.05, length.out = n + 2)
  if(n == 1) ypos <- ypos else ypos <- ypos[-c(1, length(ypos))]
  xpos.pts <- unit(0.1, "npc")
  size.pts <- 0.5 / n
  
  for (i in 1:n) {
    
    vp <- viewport(x = xpos.pts, y = ypos[i], 
                   height = size.pts, width = 0.1,
                   just = c("left", "centre"))
    
    pushViewport(vp)
    
    switch(type,
           "circles" = grid.circle(gp = gpar(fill = clrs[i], 
                                             col = "black")),
           "rectangles" = grid.rect(gp = gpar(fill = clrs[i], 
                                              col = "black")),
           "lines" = grid.lines(gp = gpar(col = clrs[i], lwd = 5)))
    
    upViewport()
    
  }
  
  xpos.txt <- unit(0.25, "npc")
  width.txt <- 0.7
  
  for (i in 1:n) {
    
    vp <- viewport(x = xpos.txt, y = ypos[i], 
                   height = size.pts, width = width.txt,
                   just = c("left", "centre"))
  
    pushViewport(vp)
    
    txt <- resizingTextGrob(x = 0, sort(unique(labs))[i],
                            just = "left")
    
    grid.draw(txt)
    
    popViewport()
    
  }
  
}
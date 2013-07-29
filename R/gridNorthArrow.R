gridNorthArrow <- function(..., rot = 0, l.width = 2,
                           scale.fact = 1) {
  
  library(gridExtra)
      
  pushViewport(viewport(angle = -rot))
  
  grid.polyline(x = c(0.5, 0.5, 0.4, 0.6), 
                y = c(0.1, 0.9, 0.55, 0.55),
                gp = gpar(lwd = l.width, 
                          lineend = "butt", 
                          linejoin = "mitre"))
  
  arrow.n <- resizingTextGrob("N", x = 0.5, y = 0.4, 
                              scale.fact = scale.fact)
  
  grid.draw(arrow.n)
  
  popViewport()
  
}
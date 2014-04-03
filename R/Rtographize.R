Rtographize <- function(spplotObj, type = c("p", "l"), 
                        border.col = "black",
                        point.type = c("circles", "rectangles"), 
                        draw.scales = TRUE, 
                        point.size = 1.5, ...) { 
  
  typ <- type[1]
  p.type = point.type[1]
  
  p.char <- switch(p.type,
                   "circles" = 21L,
                   "rectangles" = 22L)
  p.char <- rep(p.char, length(spplotObj$panel.args[[1]]$x))
  
  update(spplotObj, type = typ, legend = NULL, 
         axs = "i", cex = rep(point.size, length(p.char)),
         scales = list(draw = draw.scales, y = list(rot = c(90)), 
                       alternating = 3), 
         pch = p.char, col = rep(border.col, length(p.char)),
         ...)

}
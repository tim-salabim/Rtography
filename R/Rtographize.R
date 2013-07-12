Rtographize <- function(spplotObj, type = c("p", "l"), 
                        point.type = c("circles", "rectangles"), ...) { 
  
  typ <- type[1]
  p.type = point.type[1]
  
  p.char <- switch(p.type,
                   "circles" = 21L,
                   "rectangles" = 22L)
  
  tmp <- update(spplotObj, type = typ,
                auto.key = FALSE, axs = "i", cex = 1.5,
                scales = list(draw = TRUE, y = list(rot = c(90)), 
                              alternating = 3), 
                pch = p.char, edge.col = "black")
  
  return(tmp)
  
}
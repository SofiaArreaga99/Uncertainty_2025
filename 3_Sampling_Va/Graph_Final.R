
# Function for error bars
adderrorbars <- function(x,y,SD,direction,barlen=0.04,lwd=1.5,...){
  
  if(length(direction)>1)stop("direction must be of length one.")
  if(direction == "rightleft" | direction == "leftright")direction <- c("left","right")
  
  if("up" %in% direction)
    arrows(x0=x, x1=x, y0=y, y1=y+SD, code=3, angle=90, length=barlen, lwd=lwd,...)
  if("down" %in% direction) 
    arrows(x0=x, x1=x, y0=y, y1=y-SD, code=3, angle=90, length=barlen, lwd=lwd,...)
  if("updown" %in% direction) 
    arrows(x0=x, x1=x, y0=y+SD, y1=y-SD, code=3, angle=90, length=barlen, lwd=lwd,...)
  if("left" %in% direction) 
    arrows(x0=x, x1=x-SD, y0=y, y1=y, code=3, angle=90, length=barlen, lwd=lwd,...)
  if("right" %in% direction)
    arrows(x0=x, x1=x+SD, y0=y, y1=y, code=3, angle=90, length=barlen, lwd=lwd,...)  
}

#Source John Drake







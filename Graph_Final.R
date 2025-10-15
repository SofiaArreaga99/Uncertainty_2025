# Tus datos
datos <- data.frame(
  Year = c(1970, 1981, 1991, 2001, 2010, 2020),
  MgC = c(79.31, 71.24, 76.40, 83.99, 90.89, 94),
  MinError = c(77.81, 69.92, 74.76, 82.12, 88.72, 91.91),
  MaxError = c(80.81, 72.56, 78.04, 85.86, 93.06, 96.06),
  SD = c(1.5, 1.32, 1.64, 1.87, 2.17, 2.09)
)

# Calculamos el "SE" como la diferencia entre el valor máximo y el valor central


# Función que agregaste
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

# Hacemos un gráfico básico
plot(datos$Year, datos$MgC, type="p", pch=19, ylim=c(65,100),
     xlab="Year", ylab="Biomass (MgC)", main="Biomass with Error Bars")
# Agregamos barras de error verticales (arriba y abajo)
adderrorbars(datos$Year, datos$MgC, datos$SD, direction="updown")


#Grafico con los valores de Monteith

#Load data 







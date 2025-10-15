#Grafico de resultados primarios, incertidumbre alometrica vs error de muestreo
#results graph, allometric uncertainty vs. sampling error

# My data 
datos <- data.frame(
  Year = c(1970, 1981, 1991, 2001, 2010, 2020),
  MgC = c(79.31, 71.24, 76.40, 83.99, 90.89, 94),
  SD = c(1.5, 1.32, 1.64, 1.87, 2.17, 2.09), # allometric uncertainty
  SD_SV <- c(1.77, 2.04, 2.08, 1.91, 1.79, 1.79) #Sampling variance 
)

# Hacemos un gráfico básico - Creation of the graphic 

plot(datos$Year, datos$MgC, type="p", pch=19, cex= 1.2, col="black", ylim=c(65,100),
     xlab="Year", ylab="Biomass (MgC)", main="Biomass with Error Bars")

lines(datos$Year, datos$MgC, lwd=1, col="black",lty=2)

# Barras de error para la incertidumbre alometrica - Error bars for allometric uncertainty
adderrorbars(datos$Year-0.8, datos$MgC, datos$SD, direction="updown", col="#D55E00")

# Barras de error para el error de muestreo - Error bars from the sampling variance
adderrorbars(datos$Year+0.8, datos$MgC, datos$SD_SV, direction="updown", col="#0000FF")

#Leyenda - Legend

legend("topleft", legend = c("Allometric Uncertainty Sd", "Sampling Variance Sd"),
       col = c("#D55E00" , "#0000FF"), lty = 1, lwd = 1, pch = 19, cex = 0.7, xpd = TRUE, bty = "n")


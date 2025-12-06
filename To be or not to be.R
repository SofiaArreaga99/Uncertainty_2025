#Creation fo 10, 30 and 50 inches dbh 

library(readr)
coefficients_1 <- read_csv("1_Coefficients_Species_Uncertainty/coefficients_1.csv")
View(coefficients_1)


data.frame<-(coefficients_1)

#Estimation of the biomass 

data.frame$biomass10 <-data.frame$aHacerSa+data.frame$bHacerSa* 254 +
  data.frame$cHacerSa * (254^2) #10 inches

data.frame$biomass30 <-data.frame$aHacerSa+data.frame$bHacerSa* 762 +
  data.frame$cHacerSa * (762^2) #30 inches 

data.frame$biomass50 <-data.frame$aHacerSa+data.frame$bHacerSa* 1270 +
  data.frame$cHacerSa * (1270^2) #50 inches

#SD per diameter class

SD10<-sd(data.frame$biomass10, na.rm = TRUE)

SD30<-sd(data.frame$biomass30, na.rm = TRUE)

SD50<-sd(data.frame$biomass50, na.rm = TRUE)

# Mean per diameter class

Mean10<- mean(data.frame$biomass10, na.rm = TRUE)
Mean30<- mean(data.frame$biomass30, na.rm = TRUE)
Mean50<- mean(data.frame$biomass50, na.rm = TRUE)

# Values from the original coefficients

sugar10<- 5.248+(-0.366)* 254 +
  0.008 * (254^2) 
sugar30<- 5.248+(-0.366)* 762 +
  0.008 * (762^2) 
sugar50<- 5.248+(-0.366)* 1270 +
  0.008 * (1270^2) 

# Creation of the data frame 

Sugar_maple_UN<-data.frame(
  diameter = c(10, 30, 50),
  mean = c(Mean10, Mean30, Mean50),
  sd = c(SD10, SD30, SD50),
  Monteith=c(sugar10, sugar30, sugar50)
)

View(Sugar_maple_UN)

### Graphic 

library(ggplot2)


plot(Sugar_maple_UN$diameter, Sugar_maple_UN$mean, type="p", pch=19, ylim=c(0,15000),
     xlab="Diameter in", ylab="Biomass (Kg)", main="Sugar Maple")
points(
  Sugar_maple_UN$diameter,      # mismo eje X
  Sugar_maple_UN$Monteith,      # columna con valores Monteith
  pch = 2,                     # triángulo para diferenciar
  col = "red",                 # color distinto
  cex = 1                     # tamaño de puntos
)


# Agregamos barras de error verticales (arriba y abajo)

lines(Sugar_maple_UN$diameter, Sugar_maple_UN$mean, lwd=1, col="black",lty=2)

# Barras de error para la incertidumbre alometrica - Error bars for allometric uncertainty
adderrorbars(Sugar_maple_UN$diameter-0.8,  Sugar_maple_UN$mean, Sugar_maple_UN$sd, direction="updown", col="blue")

legend(
  "topleft",
  legend = c("Biomass mean (from coefficients)", "Monteith biomass mean"),
  col = c("black", "red"),
  pch = c(19, 2),     # símbolo de cada serie
  bty = "n"
)





#
#


# Real Data #
sugar_maple <- data.frame(
  location = "New York",
  source   = "Monteith 1979",
  dbh      = seq(from = 2.5, to = 55, by = 2.5),
  kg       = c(2, 3, 9, 21, 38, 61, 89, 122, 160, 204,
               254, 308, 368, 434, 504, 581, 662, 749,
               841, 938, 1041, 1150)
)






# pseudo data (One graph per diameter)

plot(data.frame$correlative, data.frame$biomass10,
     pch = 16,
     col = rgb(0.2, 0.4, 0.6, 0.5),   # azul semi-transparente
     xlab = "Correlative", 
     ylab = "Biomass (kg)", 
     main = "Sugar Maple 10 inches",
     cex = 1.2)
grid()

#Trying to accomodate the data 

##############################################################################

library(tidyr)
library(dplyr)

df_long <- data.frame %>%
  pivot_longer(
    cols = c(biomass10, biomass30, biomass50),
    names_to = "diametro",
    values_to = "biomasa"
  )

#Extracting the diameters 10,30,50

df_long$diametro_num <- as.numeric(gsub("biomass", "", df_long$diametro))

head(df_long)

# Graphic


library(ggplot2)

ggplot(df_long, aes(x = diametro_num, y = biomasa)) +
  geom_point(alpha = 0.3, size = 1) +         # transparencia para 30k puntos
  theme_minimal() +
  labs(
    x = "Diameter (in)",
    y = "Biomass Kg",
    title = "Simulated biomass for sugar maple"
  )

###############################################################################


source("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/adderrorbars.R")



# Hacemos un gráfico básico
plot(datos$Year, datos$MgC, type="p", pch=19, ylim=c(65,100),
     xlab="Year", ylab="Biomass (MgC)", main="Biomass with Error Bars")
# Agregamos barras de error verticales (arriba y abajo)
adderrorbars(datos$Year, datos$MgC, datos$SD, direction="updown")









################################################################################

plot(data.frame$biomass10,
     col = "blue",
     pch = 16,
     xlab = "Índice",
     ylab = "Biomasa",
     main = "Dispersión de biomasa a distintas alturas")

points(data.frame$biomass30, col = "red", pch = 16)
points(data.frame$biomass50, col = "green", pch = 16)

legend("topright",
       legend = c("10 in", "30 in", "50 in"),
       col = c("blue", "red", "green"),
       pch = 16)

##############################################################################





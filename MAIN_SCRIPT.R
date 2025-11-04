#Hello!
#This is the main script :)! 


#--------------------------------------------------------------------
#  Packages 
#--------------------------------------------------------------------

# Allometric uncertainty functions 
source("2_Biomass_Project/R/Load_packages.R")
source("2_Biomass_Project/R/Read_data.R")
source("2_Biomass_Project/R/Process_iteration.R")
source("2_Biomass_Project/R/Run_iterations.R")

# Sampling variance 
source("3_Sampling_Va/Scripts/functions_biomass.R")

# Ejecutar flujo
load_packages()

#--------------------------------------------------------------------
#  Allometric uncertainty :) 
#--------------------------------------------------------------------


#--------------------------------------------------------------------
# ....1970
#--------------------------------------------------------------------

data1970 <- read_data(
  "4_General_Resources/MergeCov.csv",
  "4_General_Resources/data1970.csv"
)


results1970 <- run_iterations(data1970$frameCoeff2, data1970$Plot1HWF)

print(results1970)



#--------------------------------------------------------------------
# ....1981
#--------------------------------------------------------------------

data1981 <- read_data(
  "4_General_Resources/MergeCov.csv",
  "4_General_Resources/data1981.csv"
)

results1981 <- run_iterations(data1981$frameCoeff2, data1981$Plot1HWF)

print(results1981)
# Guardar resultados si quieres
#write.csv(results1981, "2_Biomass_project/outputBm/PlotsTotal1981.csv", row.names = FALSE)

#--------------------------------------------------------------------
# ....1991
#--------------------------------------------------------------------

data1991 <- read_data(
  "4_General_Resources/MergeCov.csv",
  "4_General_Resources/data1991.csv"
)

results1991 <- run_iterations(data1991$frameCoeff2, data1991$Plot1HWF)

print(results1991)


#--------------------------------------------------------------------
# ....2001
#--------------------------------------------------------------------

data2001 <- read_data(
  "4_General_Resources/MergeCov.csv",
  "4_General_Resources/data2001.csv"
)

results2001 <- run_iterations(data2001$frameCoeff2, data2001$Plot1HWF)

print(results2001)

#--------------------------------------------------------------------
# ....2011
#--------------------------------------------------------------------

data2011 <- read_data(
  "4_General_Resources/MergeCov.csv",
  "4_General_Resources/data2011.csv"
)

results2011 <- run_iterations(data2011$frameCoeff2, data2011$Plot1HWF)

print(results2011)

#--------------------------------------------------------------------
# ....2021
#--------------------------------------------------------------------

data2021 <- read_data(
  "4_General_Resources/MergeCov.csv",
  "4_General_Resources/data2021.csv"
)

results2021 <- run_iterations(data2021$frameCoeff2, data2021$Plot1HWF)

print(results2021)

write.csv(results2021, "2_Biomass_project/outputBm/PlotsTotal2021.csv", row.names = FALSE)


#--------------------------------------------------------------------

#--------------------------------------------------------------------
#  Allometric uncertainty :) 
#--------------------------------------------------------------------
# Some plots have been added over the years.
#--------------------------------------------------------------------
# ....1970
#--------------------------------------------------------------------

# Execute the function- Ejecutar función
resultados <- calc_biomass("4_General_Resources/OriginalNUcsv.csv", 
                           "4_General_Resources/data1970.csv", 
                           "4_General_Resources/ComPlot.xlsx", 
                           "4_General_Resources/Comp_1.xlsx")

# save results - Guardar resultados 
write.csv(resultados$Plots_Compa, "3_Sampling_Va/output/Plots_Compa1970.csv", row.names = FALSE)
write.csv(resultados$SampVa, "3_Sampling_Va/output/SampVa1970.csv", row.names = FALSE)

#The standard deviation of the sampling‐variance storage in "resultados"
head(resultados)

#--------------------------------------------------------------------
# ....1981
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("4_General_Resources/OriginalNUcsv.csv", 
                           "4_General_Resources/data1981.csv", 
                           "4_General_Resources/ComPlot.xlsx", 
                           "4_General_Resources/Comp_1.xlsx")

# Guardar resultados
write.csv(resultados$Plots_Compa, "3_Sampling_Va/output/Plots_Compa1981.csv", row.names = FALSE)
write.csv(resultados$SampVa, "3_Sampling_Va/output/SampVa1981.csv", row.names = FALSE)


#--------------------------------------------------------------------
# ....1991
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("4_General_Resources/OriginalNUcsv.csv", 
                           "4_General_Resources/data1991.csv", 
                           "4_General_Resources/ComPlot.xlsx", 
                           "4_General_Resources/Comp_1.xlsx")

# Guardar resultados
write.csv(resultados$Plots_Compa, "3_Sampling_Va/output/Plots_Compa1991.csv", row.names = FALSE)
write.csv(resultados$SampVa, "3_Sampling_Va/output/SampVa1991.csv", row.names = FALSE)


#--------------------------------------------------------------------
# ....2001
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("4_General_Resources/OriginalNUcsv.csv", 
                           "4_General_Resources/data2001.csv", 
                           "4_General_Resources/ComPlot.xlsx", 
                           "4_General_Resources/Comp_1.xlsx")

# Guardar resultados
write.csv(resultados$Plots_Compa, "3_Sampling_Va/output/Plots_Compa2001.csv", row.names = FALSE)
write.csv(resultados$SampVa, "3_Sampling_Va/output/SampVa2001.csv", row.names = FALSE)


#--------------------------------------------------------------------
# ....2011
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("4_General_Resources/OriginalNUcsv.csv", 
                           "4_General_Resources/data2011.csv", 
                           "4_General_Resources/ComPlot.xlsx", 
                           "4_General_Resources/Comp_1.xlsx")

# Guardar resultados 
write.csv(resultados$Plots_Compa, "3_Sampling_Va/output/Plots_Compa2011.csv", row.names = FALSE)
write.csv(resultados$SampVa, "3_Sampling_Va/output/SampVa2011.csv", row.names = FALSE)


#--------------------------------------------------------------------
# ....2021
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("4_General_Resources/OriginalNUcsv.csv", 
                           "4_General_Resources/data2021.csv", 
                           "4_General_Resources/ComPlot.xlsx", 
                           "4_General_Resources/Comp_1.xlsx")

# Guardar resultados
write.csv(resultados$Plots_Compa, "3_Sampling_Va/output/Plots_Compa2021.csv", row.names = FALSE)
write.csv(resultados$SampVa, "3_Sampling_Va/output/SampVa2021.csv", row.names = FALSE)



#--------------------------------------------------------------------
# .... Rate...
#--------------------------------------------------------------------

#This code helps us graph the difference between 1981 and 2021 to understand carbon capture during this time period.

library(readr)

# 1. Load data for the years 1981 and 2021- Cargamos los archivos con los datos a usar para 1981 y 2021

PlotsTotal1981 <- read_csv("2_Biomass_Project/outputBm/PlotsTotal1981.csv")
PlotsTotal2021 <- read_csv("2_Biomass_Project/outputBm/PlotsTotal2021.csv")


# 2. ID per simulation (useful for the substraction) - Le agregamos un ID para poder realizar la diferencia entre los resultados

PlotsTotal1981$PlotID <- 1:nrow(PlotsTotal1981)
PlotsTotal2021$PlotID <- 1:nrow(PlotsTotal2021)


# 3. Merge per simulation - Juntamos los valores usando el ID 

merged_data <- merge(PlotsTotal1981, PlotsTotal2021, by = "PlotID", suffixes = c("_1981", "_2021"))


# 4. Convert columns to numeric - Convertimos las columnas a un valor númerico 
merged_data$resultados_df.mg_ha_1981 <- as.numeric(merged_data$resultados_df.mg_ha_1981)
merged_data$resultados_df.mg_ha_2021 <- as.numeric(merged_data$resultados_df.mg_ha_2021)

# 5. anual rate - Realizamos el calculo de la diferencia 
years <- 2021 - 1981
merged_data$annual_rate <- (merged_data$resultados_df.mg_ha_2021 - merged_data$resultados_df.mg_ha_1981) / years
merged_data$Diff <- (merged_data$resultados_df.mg_ha_2021 - merged_data$resultados_df.mg_ha_1981)

# Stats (mean and sd) - calculo de estadisticos (media y desviación estandar)
annual_rate_mean <- mean(merged_data$annual_rate, na.rm = TRUE)
annual_rate_sd <- sd(merged_data$annual_rate, na.rm = TRUE)
Diff_mean <- mean(merged_data$Diff, na.rm = TRUE)
Diff_sd <- sd(merged_data$Diff, na.rm = TRUE)
annual_rate_ci <- quantile(merged_data$annual_rate, probs = c(0.025, 0.975), na.rm = TRUE)

summary_stats <- data.frame(
  mean = annual_rate_mean,
  sd_annual = annual_rate_sd,
  sd_Diff = Diff_sd,
  ci_lower = annual_rate_ci[1],
  ci_upper = annual_rate_ci[2]
)
print(summary_stats)

#Graph with the raw difference - Graficamos los valores de la diferencia cruda

hist(merged_data$Diff,
     breaks = 30,                 
     main = "",
     xlab = "Aboveground Live Biomass Mg C/ha ",
     col = "#56B4E9",
     border = "white",
     ylim = c(0, 1000))   

# Línea de la media
abline(v = Diff_mean, col = "#CC79A7", lwd = 2)  

# Líneas de ±1 SD
abline(v = Diff_mean - Diff_sd, col = "#009E73", lwd = 2, lty = 2)
abline(v = Diff_mean + Diff_sd, col = "#009E73", lwd = 2, lty = 2)

# Leyenda
legend("topleft",
       legend = c(paste0("Mean = ", round(Diff_mean, 1)),
                  paste0("±1 SD = ", round(Diff_sd, 1))),
       col = c("#CC79A7", "#009E73"),
       lty = c(1, 2),
       lwd = 1,
       bty = "n",
       cex = 0.8)


#Graph with the rate (optional) - Aqui graficamos con el rate de forma opcional 

hist(merged_data$annual_rate,
     breaks = 30,                 # número de barras
     main = "",
     xlab = "Mg C/ha",
     col = "skyblue",
     border = "white")
abline(v = annual_rate_mean, col = "red", lwd = 2)  # línea de la media




#--------------------------------------------------------------------
# .... Graphic... Allometric uncertainty vs sampling variance
#--------------------------------------------------------------------
#Grafico de resultados primarios, incertidumbre alometrica vs error de muestreo
#results graph, allometric uncertainty vs. sampling error

# My data 
datos <- data.frame(
  Year = c(1970, 1981, 1991, 2001, 2010, 2020),
  MgC = c(79.31, 71.24, 76.40, 83.99, 90.89, 94),
  SD = c(1.5, 1.32, 1.64, 1.87, 2.17, 2.09), # allometric uncertainty
  SD_SV <- c(1.93, 2.03, 2.20, 2.06, 1.99, 2.03) #Sampling variance New
)

# Hacemos un gráfico básico - Creation of the graphic 

plot(datos$Year, datos$MgC, type="p", pch=19, cex= 1.2, col="black", ylim=c(65,100),
     xlab="Year", ylab="Biomass Mg C ha -1", main="Biomass with Error Bars")

lines(datos$Year, datos$MgC, lwd=1, col="black",lty=2)

# Barras de error para la incertidumbre alometrica - Error bars for allometric uncertainty
adderrorbars(datos$Year-0.8, datos$MgC, datos$SD, direction="updown", col="#D55E00")

# Barras de error para el error de muestreo - Error bars from the sampling variance
adderrorbars(datos$Year+0.8, datos$MgC, datos$SD_SV, direction="updown", col="#0000FF")

#Leyenda - Legend

legend("topleft", legend = c("Allometric Uncertainty Sd", "Sampling Variance Sd"),
       col = c("#D55E00" , "#0000FF"), lty = 1, lwd = 1, pch = 19, cex = 0.7, xpd = TRUE, bty = "n")






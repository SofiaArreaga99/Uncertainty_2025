#Este codigo nos ayuda a graficar el resultado de la diferencia entre el año de 1981 y 2021 para saber sobre la captura de carbono durante este periodo de tiempo. 
#This code helps us graph the difference between 1981 and 2021 to understand carbon capture during this time period.

library(readr)

# 1. Load data for the years 1981 and 2021- Cargamos los archivos con los datos a usar para 1981 y 2021

PlotsTotal1981 <- read_csv("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/2_Biomass_Project/outputBm/PlotsTotal1981.csv")
PlotsTotal2021 <- read_csv("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/2_Biomass_Project/outputBm/PlotsTotal2021.csv")


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
     xlab = "Mg C/ha ",
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




# Guardar resultados completos y resumen en CSV
write.csv(merged_data, "C:/Users/vanco/Desktop/ResearchR/Final_Bio/Biomass_Project/outputBm/PlotsTotal_merged_rates.csv", row.names = FALSE)
write.csv(summary_stats, "C:/Users/vanco/Desktop/ResearchR/Final_Bio/Biomass_Project/outputBm/AnnualRate_summary.csv", row.names = FALSE)






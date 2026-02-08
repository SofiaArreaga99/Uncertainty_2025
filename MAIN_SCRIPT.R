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
  "4_General_Resources/MergeCov_2.csv",
  "4_General_Resources/data1970.csv"
)


results1970 <- run_iterations(data1970$frameCoeff2, data1970$Plot1HWF)

print(results1970)



#--------------------------------------------------------------------
# ....1981
#--------------------------------------------------------------------

data1981 <- read_data(
  "4_General_Resources/MergeCov_2.csv",
  "4_General_Resources/data1981.csv"
)

results1981 <- run_iterations(data1981$frameCoeff2, data1981$Plot1HWF)

print(results1981)
# Guardar resultados si quieres
write.csv(results1981, "2_Biomass_project/outputBm/PlotsTotal1981.csv", row.names = FALSE)

#--------------------------------------------------------------------
# ....1991
#--------------------------------------------------------------------

data1991 <- read_data(
  "4_General_Resources/MergeCov_2.csv",
  "4_General_Resources/data1991.csv"
)

results1991 <- run_iterations(data1991$frameCoeff2, data1991$Plot1HWF)

print(results1991)


#--------------------------------------------------------------------
# ....2001
#--------------------------------------------------------------------

data2001 <- read_data(
  "4_General_Resources/MergeCov_2.csv",
  "4_General_Resources/data2001.csv"
)

results2001 <- run_iterations(data2001$frameCoeff2, data2001$Plot1HWF)

print(results2001)

#--------------------------------------------------------------------
# ....2011
#--------------------------------------------------------------------

data2011 <- read_data(
  "4_General_Resources/MergeCov_2.csv",
  "4_General_Resources/data2011.csv"
)

results2011 <- run_iterations(data2011$frameCoeff2, data2011$Plot1HWF)

print(results2011)

#--------------------------------------------------------------------
# ....2021
#--------------------------------------------------------------------

data2021 <- read_data(
  "4_General_Resources/MergeCov_2.csv",
  "4_General_Resources/data2021.csv"
)

results2021 <- run_iterations(data2021$frameCoeff2, data2021$Plot1HWF)

print(results2021)

write.csv(results2021, "2_Biomass_project/outputBm/PlotsTotal2021.csv", row.names = FALSE)


#--------------------------------------------------------------------

#--------------------------------------------------------------------
#  Sampling variance  :) 
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
resultados
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

# Function for the graph 
source("adderrorbars.R")

# My data 
datos <- data.frame(
  Year = c(1970, 1981, 1991, 2001, 2010, 2020),
  MgC = c(79.38, 71.32, 76.50, 84.12, 91.06, 94.17),
  SD = c(1.49, 1.31, 1.62, 1.85, 2.14, 2.06), # allometric uncertainty
  SD_SV <- c(1.93, 2.03, 2.20, 2.06, 1.99, 2.03) #Sampling variance New
)

# Hacemos un gráfico básico - Creation of the graphic 

plot(datos$Year, datos$MgC, type="p", pch=19, cex= 1.2, col="black", ylim=c(65,100),
     xlab="Year", ylab="Aboveground Live Biomass Mg C ha -1")

lines(datos$Year, datos$MgC, lwd=1, col="black",lty=2)

# Barras de error para la incertidumbre alometrica - Error bars for allometric uncertainty
adderrorbars(datos$Year-0.8, datos$MgC, datos$SD, direction="updown", col="#D55E00")

# Barras de error para el error de muestreo - Error bars from the sampling variance
adderrorbars(datos$Year+0.8, datos$MgC, datos$SD_SV, direction="updown", col="#0000FF")

#Leyenda - Legend

legend("topleft", legend = c("Allometric Uncertainty Sd", "Sampling Variance Sd"),
       col = c("#D55E00" , "#0000FF"), lty = 1, lwd = 1, pch = 19, cex = 0.7, xpd = TRUE, bty = "n")



########################################################################
# NEW
########################################################################

# Function for the graph 
source("adderrorbars.R")

# My data 
datos <- data.frame(
  Year = c(1970, 1981, 1991, 2001, 2010, 2020),
  MgC = c(79.38, 71.32, 76.50, 84.12, 91.06, 94.17),
  SD = c(1.49, 1.31, 1.62, 1.85, 2.14, 2.06), # allometric uncertainty
  SD_SV <- c(1.93, 2.03, 2.19, 2.05, 1.98, 2.03) #Sampling variance New
)

# Hacemos un gráfico básico - Creation of the graphic 

plot(datos$Year, datos$MgC, type="p", pch=19, cex= 1.2, col="black", ylim=c(65,100),
     xlab="Year", ylab="Aboveground Live Biomass Mg C ha -1")

lines(datos$Year, datos$MgC, lwd=1, col="black",lty=2)

# Barras de error para la incertidumbre alometrica - Error bars for allometric uncertainty
adderrorbars(datos$Year-0.8, datos$MgC, datos$SD, direction="updown", col="#D55E00")

# Barras de error para el error de muestreo - Error bars from the sampling variance
adderrorbars(datos$Year+0.8, datos$MgC, datos$SD_SV, direction="updown", col="#0000FF")

#Leyenda - Legend

legend("topleft", legend = c("Allometric Uncertainty Sd", "Sampling Variance Sd"),
       col = c("#D55E00" , "#0000FF"), lty = 1, lwd = 1, pch = 19, cex = 0.7, xpd = TRUE, bty = "n")

########################################################################
# Density by category of DBH 
########################################################################


# Análisis de Distribución Diamétrica por Especie
# Código optimizado y documentado

# Librerías necesarias
# Required libraries

library(readr)
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. CARGA DE DATOS - DATA LOADING #Change the source to get the two different years
# ==============================================================================

data2021_Trees <- read_csv("4_General_Resources/data2021.csv")
Code_SpEq <- read_csv("4_General_Resources/Code_SpEq.csv")

View(data2021_Trees)

# Hacer el join usando la columna Equation
data2021_Trees <- data2021_Trees %>%
  left_join(Code_SpEq %>% select(Equation, common_name), 
            by = "Equation")

View(data2021_Trees)
# ==============================================================================
# 2. PROCESAMIENTO DE DATOS - DATA PROCESSING - This should repeated be for each year
# ==============================================================================

# 2.1 Clasificación en intervalos diamétricos de 5 cm
# Classification in 5 cm diameter intervals
plots_dbh <- data2021_Trees %>%
  filter(!is.na(DBHcm)) %>%  # removing NA
  mutate(
    DBH_interval = cut(
      DBHcm,
      breaks = seq(0, ceiling(max(DBHcm) / 5) * 5, by = 5),
      right = FALSE,
      include.lowest = TRUE,
      labels = paste0(seq(0, ceiling(max(DBHcm) / 5) * 5 - 5, by = 5), "-", 
                      seq(5, ceiling(max(DBHcm) / 5) * 5, by = 5))
    )
  )

# 2.2 Agrupación por parcela, tamaño, ecuación e intervalo diamétrico
# Grouping by plot, size, equation, and diameter interval
trees_grouped <- plots_dbh %>%
  group_by(Plot, PlotSize, Equation, DBH_interval) %>%
  summarise(n_trees = n(), .groups = "drop")

# 2.3 Estandarización a árboles por hectárea
# Standardization to trees per hectare
# Factores de conversión basados en área de parcelas
CONVERSION_FACTORS <- c(
  "POLE" = 10000 / 202.343,  # Parcelas pequeñas
  "SAW" = 10000 / 809.372     # Parcelas grandes
)

trees_per_ha <- trees_grouped %>%
  mutate(
    trees_ha = n_trees * CONVERSION_FACTORS[PlotSize]
  ) %>%
  filter(!is.na(trees_ha))  # Remover valores no válidos

# 2.4 Sumarización por parcela, ecuación e intervalo diamétrico
# Summarization by plot, equation, and diameter interval
sum_by_plot <- trees_per_ha %>%
  group_by(Plot, DBH_interval, Equation) %>%
  summarise(trees_ha_total = sum(trees_ha, na.rm = TRUE), .groups = "drop")

# 2.5 Promedio entre parcelas por especie e intervalo diamétrico
# Average across plots by species and diameter class.
average_distribution <- sum_by_plot %>%
  group_by(DBH_interval, Equation) %>%
  summarise(
    mean_trees_ha = mean(trees_ha_total, na.rm = TRUE),
    se_trees_ha = sd(trees_ha_total, na.rm = TRUE) / sqrt(n()),
    n_plots = n(),
    .groups = "drop"
  )

# 2.6 Agregar nombres comunes de especies
#Add common names of species
distribution_final <- average_distribution %>%
  left_join(Code_SpEq, by = "Equation") %>%
  filter(!is.na(DBH_interval), !is.na(mean_trees_ha)) %>%
  arrange(Equation, DBH_interval)

View(distribution_final)

# ==============================================================================
# 3. EXPORTAR RESULTADOS - EXPORT RESULTS
# ==============================================================================

# Guardar tabla de resultados
#write_csv(distribution_final, "figures/diameter_distribution_by_species2021.csv")

# Guardar gráficos
#ggsave("figures/fig_diameter_distribution_lines2021.png", p1, 
#       width = 10, height = 6, dpi = 300)
#ggsave("figures/fig_diameter_distribution_stacked2021.png", p2, 
#       width = 10, height = 6, dpi = 300)

# ==============================================================================
# 4. ESTADÍSTICAS DESCRIPTIVAS
# ==============================================================================

# Resumen por especie
species_summary <- distribution_final %>%
  group_by(common_name) %>%
  summarise(
    total_trees_ha = sum(mean_trees_ha),
    n_diameter_classes = n(),
    .groups = "drop"
  )

print(species_summary)


# ==============================================================================
# 5. TWO GRAPHS TOGETHER - DOS GRAFICOS JUNTOS 
# ==============================================================================

# Uploading the files
dbh_dis2021 <- read_csv("Figures/diameter_distribution_by_species2021.csv")
dbh_dis1981 <- read_csv("Figures/diameter_distribution_by_species1981.csv")

# Adding years
dbh_dis1981 <- dbh_dis1981 %>%
  mutate(year = 1981)
dbh_dis2021 <- dbh_dis2021 %>%
  mutate(year = 2021)

# Combining the information 
dbh_dis81_21 <- rbind(dbh_dis1981, dbh_dis2021)

library(ggplot2)
library(patchwork)
library(dplyr)

# Paleta Okabe–Ito (color-blind friendly)
okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7"  # reddish purple
)

# Reordenar los niveles de DBH_interval
dbh_dis81_21$DBH_interval <- factor(dbh_dis81_21$DBH_interval,
                                    levels = c("10-15", "15-20", "20-25", "25-30",
                                               "30-35", "35-40", "40-45", "45-50",
                                               "50-55", "55-60", "60-65", "65-70",
                                               "70-75", "75-80", "80-85", "85-90",
                                               "90-95", "95-100", "100-105"))

# Definir especies con aumento y disminución en QMD
species_increase <- c("Eastern Hemlock", "Red Maple", "Sugar Maple")
species_decrease <- c("American Beech", "Red Spruce", "White Ash", "Yellow Birch")

# Asignar colores únicos a cada especie
species_colors <- c(
  "Eastern Hemlock" = okabe_ito[1],
  "Red Maple" = okabe_ito[2],
  "Sugar Maple" = okabe_ito[3],
  "American Beech" = okabe_ito[4],
  "Red Spruce" = okabe_ito[5],
  "White Ash" = okabe_ito[6],
  "Yellow Birch" = okabe_ito[7]
)

# Calcular frecuencia acumulada para cada especie y año
dbh_cumulative <- dbh_dis81_21 %>%
  group_by(common_name, year) %>%
  arrange(DBH_interval) %>%
  mutate(cumulative_trees_ha = cumsum(mean_trees_ha)) %>%
  ungroup()

# Filtrar datos para cada grupo
dbh_increase <- dbh_cumulative %>%
  filter(common_name %in% species_increase)

dbh_decrease <- dbh_cumulative %>%
  filter(common_name %in% species_decrease)

# Seleccionar intervalos para mostrar en el eje X (cada 10 cm o cada 2 clases)
x_breaks <- c("10-15", "20-25", "30-35", "40-45", "50-55", 
              "60-65", "70-75", "80-85", "90-95", "100-105")

# Gráfico para especies con AUMENTO en QMD (izquierda)
p_increase <- ggplot(dbh_increase,
                     aes(x = DBH_interval,
                         y = cumulative_trees_ha,
                         color = common_name,
                         group = interaction(common_name, year),
                         linetype = factor(year))) +
  geom_line(linewidth = 1) + 
  geom_point(aes(shape = factor(year)), size = 2) +
  labs(
    x = "Diameter class (cm)",
    y = expression(paste("Cumulative trees ha"^"-1")),
    color = "Species",
    linetype = "Year",
    shape = "Year",
    title = "Species with QMD Increase"
  ) + 
  scale_x_discrete(breaks = x_breaks) +  # Mostrar solo algunos intervalos
  scale_y_continuous(breaks = seq(0, max(dbh_cumulative$cumulative_trees_ha, na.rm = TRUE), 
                                  by = 50),
                     limits = c(0, NA)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "italic"),
    panel.grid.major = element_line(color = "gray90")
  ) + 
  scale_color_manual(values = species_colors, drop = FALSE) +
  scale_linetype_manual(values = c("1981" = "dashed", "2021" = "solid"),
                        labels = c("1981", "2021"))

# Gráfico para especies con DISMINUCIÓN en QMD (derecha)
p_decrease <- ggplot(dbh_decrease,
                     aes(x = DBH_interval,
                         y = cumulative_trees_ha,
                         color = common_name,
                         group = interaction(common_name, year),
                         linetype = factor(year))) +
  geom_line(linewidth = 1) + 
  geom_point(aes(shape = factor(year)), size = 2) +
  labs(
    x = "Diameter class (cm)",
    y = expression(paste("Cumulative trees ha"^"-1")),
    color = "Species",
    linetype = "Year",
    shape = "Year",
    title = "Species with QMD Decrease"
  ) + 
  scale_x_discrete(breaks = x_breaks) +  # Mostrar solo algunos intervalos
  scale_y_continuous(breaks = seq(0, max(dbh_cumulative$cumulative_trees_ha, na.rm = TRUE), 
                                  by = 50),
                     limits = c(0, NA)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "italic"),
    panel.grid.major = element_line(color = "gray90")
  ) + 
  scale_color_manual(values = species_colors, drop = FALSE) +
  scale_linetype_manual(values = c("1981" = "dashed", "2021" = "solid"),
                        labels = c("1981", "2021"))

# Combinar ambos gráficos lado a lado con leyenda compartida
p_combined <- p_increase + p_decrease + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

print(p_combined)

########################################################################
# Stats category of DBH 
########################################################################

# ==============================================================================
# 1. CARGA DE DATOS - DATA LOADING 
# ==============================================================================
library(dplyr)
library(readr)

#Density 
#uploading the files
dbh_dis2021 <- read_csv("Figures/diameter_distribution_by_species2021.csv")
dbh_dis1981 <- read_csv("Figures/diameter_distribution_by_species1981.csv")

#Summary
species_summary2021 <- dbh_dis2021 %>%
  group_by(common_name) %>%
  summarise(
    total_trees_ha = sum(mean_trees_ha),
    n_diameter_classes = n(),
    .groups = "drop"
  )

#Summary
species_summary1981 <- dbh_dis1981 %>%
  group_by(common_name) %>%
  summarise(
    total_trees_ha = sum(mean_trees_ha),
    n_diameter_classes = n(),
    .groups = "drop"
  )
#View(species_summary1981)

# ==============================================================================
# 2. Función para calcular QMD - Function to calculate QMD
# ==============================================================================
dataTrees2021 <- read_csv("4_General_Resources/data2021.csv")
dataTrees1981 <- read_csv("4_General_Resources/data1981.csv")
Code_SpEq <- read_csv("4_General_Resources/Code_SpEq.csv")

#View(data2021_Trees)


# Hacer el join y añadir la columna common_name
dataTrees2021 <- dataTrees2021 %>%
  left_join(Code_SpEq %>% select(Equation, common_name), 
            by = "Equation")
dataTrees1981 <- dataTrees1981 %>%
  left_join(Code_SpEq %>% select(Equation, common_name), 
            by = "Equation")

# Función para calcular QMD
calculate_qmd <- function(dbh_values) {
  sqrt(sum(dbh_values^2) / length(dbh_values))
}

# Calcular QMD para 1981
qmd_1981 <- dataTrees1981 %>%
  group_by(common_name) %>%
  summarise(
    QMD_1981 = calculate_qmd(DBHcm),
    N_1981 = n()
  ) %>%
  ungroup()

# Calcular QMD para 2021
qmd_2021 <- dataTrees2021 %>%
  group_by(common_name) %>%
  summarise(
    QMD_2021 = calculate_qmd(DBHcm),
    N_2021 = n()
  ) %>%
  ungroup()

# Combinar con merge/join
qmd_change <- qmd_1981 %>%
  full_join(qmd_2021, by = "common_name") %>%
  mutate(
    QMD_change = QMD_2021 - QMD_1981,  # Usa los nombres de las COLUMNAS
    QMD_change_percent = ((QMD_2021 - QMD_1981) / QMD_1981) * 100,
    Individuals_change = N_2021 - N_1981,
    Density2021 = species_summary2021,
    Density2018 = species_summary1981
  )

#View(qmd_change)

# ==============================================================================
# Kolmogorov-Smirnov test 
# ==============================================================================

library(dplyr)

# Obtener lista única de especies
species_list <- unique(dataTrees1981$common_name)

# Crear dataframe para almacenar resultados
ks_results <- data.frame()

# Loop a través de todas las especies
for(sp in species_list) {  
  # Filtrar DBH por especie y año
  dbh_1981 <- dataTrees1981 %>%
    filter(common_name == sp) %>%
    pull(DBHcm)
  
  dbh_2021 <- dataTrees2021 %>%
    filter(common_name == sp) %>%
    pull(DBHcm)
  
  # Realizar test de Kolmogorov-Smirnov
  ks_result <- ks.test(dbh_1981, dbh_2021)
  
  # Guardar resultados
  ks_results <- rbind(ks_results, data.frame(  
    species = sp,  
    D_statistic = ks_result$statistic,  
    p_value = ks_result$p.value,  
    n_1981 = length(dbh_1981),  
    n_2021 = length(dbh_2021),
    significant = ifelse(ks_result$p.value < 0.05, "Yes", "No")
  )) 
}

# Ver resultados
print(ks_results)

# Ordenar por p-value
ks_results_sorted <- ks_results %>%
  arrange(p_value)

print(ks_results_sorted)

# Combinar con resultados de QMD
combined_results <- qmd_change %>%
  left_join(ks_results, by = c("common_name" = "species"))

print(combined_results)


View(combined_results)


###############################################################################
# Graphic 

library(ggplot2)

# Graphs 

# Calcular el factor de escala para el segundo eje Y
scale_factor <- max(qmd_change$QMD_change, na.rm = TRUE) / max(abs(qmd_change$Individuals_change), na.rm = TRUE)

# Gráfico con dos ejes Y
p_qmd_dual <- ggplot(qmd_change, aes(x = common_name)) +
  geom_col(aes(y = QMD_change, fill = common_name), alpha = 0.7, width = 0.7) +
  geom_line(aes(y = Individuals_change * scale_factor, group = 1), 
            linewidth = 0.8, color = "gray30", linetype = "solid") +
  geom_point(aes(y = Individuals_change * scale_factor), 
             size = 2.5, shape = 21, fill = "white", color = "gray30", stroke = 1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
  scale_y_continuous(
    name = "Quadratic mean change DBH (cm)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Change in Number of Individuals")
  ) +
  labs(
    x = "Species",
    title = "Changes in QMD and Population Size (1981-2021)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "italic"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    axis.title.y.right = element_text(color = "gray30", face = "bold"),
    axis.text.y.right = element_text(color = "gray30"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3)
  ) +
  scale_fill_manual(values = okabe_ito)

print(p_qmd_dual)


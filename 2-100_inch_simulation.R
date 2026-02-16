# Start #
# Inicio #

library(readr)
library(dplyr)

# Simple function to calculate biomass by species
# Función simple para calcular biomasa por especie
calcular_biomasa_especie <- function(archivo, nombre_especie, a_monteith, b_monteith, c_monteith) {
  
  # Read file
  # Leer archivo
  datos <- read_csv(archivo, show_col_types = FALSE)
  
  # Find columns that begin with a, b, c
  # Encontrar columnas que empiezan con a, b, c
  columna_a <- colnames(datos)[startsWith(tolower(colnames(datos)), "a")][1]
  columna_b <- colnames(datos)[startsWith(tolower(colnames(datos)), "b")][1]
  columna_c <- colnames(datos)[startsWith(tolower(colnames(datos)), "c")][1]
  
  # Diámetros a evaluar (pulgadas)
  #Usually de dimeter were collected for 2.5 - 55 cm
  
  diametros <- c(1, 5, 10, 30, 50, 70, 100) #inches
  
  # Convertir a mm
  diametros_mm <- diametros * 25.4
  
  # Calculate biomass for each diameter
  # Calcular biomasa para cada diámetro
  resultados <- data.frame(
    species = nombre_especie,
    diameter = diametros,
    diametercm =diametros_mm/10,
    mean = NA,
    sd = NA,
    Monteith = NA
      )
  
  for(i in 1:length(diametros)) {
    d <- diametros_mm[i]
    
    # Calculate biomass with each set of coefficients
    # Calcular biomasa con cada set de coeficientes
    biomasa <- datos[[columna_a]] + datos[[columna_b]] * d + datos[[columna_c]] * (d^2)
    
    # Statistics # Estadísticas
    resultados$mean[i] <- mean(biomasa, na.rm = TRUE)
    resultados$sd[i] <- sd(biomasa, na.rm = TRUE)
    
    # Biomass from Monteith
    resultados$Monteith[i] <- a_monteith + b_monteith * d + c_monteith * (d^2)
  }
  
  #  CV
  resultados$CVsd_mean <- resultados$sd / resultados$mean
  
  return(resultados)
}

# Process each species
#Procesar cada especie

sugar_maple <- calcular_biomasa_especie(
  "1_Coefficients_Species_Uncertainty/coefficients_1.csv",
  "Sugar Maple", 5.248, -0.366, 0.008
)

yellow_birch <- calcular_biomasa_especie(
  "1_Coefficients_Species_Uncertainty/coefficients_2.csv",
  "Yellow Birch", 9.370, -0.449, 0.007
)

american_beech <- calcular_biomasa_especie(
  "1_Coefficients_Species_Uncertainty/coefficients_3.csv",
  "American Beech", 5.337, -0.326, 0.007
)

eastern_hemlock <- calcular_biomasa_especie(
  "1_Coefficients_Species_Uncertainty/coefficients_4.csv",
  "Eastern Hemlock", 6.137, -0.278, 0.004
)

red_maple <- calcular_biomasa_especie(
  "1_Coefficients_Species_Uncertainty/coefficients_5.csv",
  "Red Maple", 6.115, -0.360, 0.006
)

red_spruce <- calcular_biomasa_especie(
  "1_Coefficients_Species_Uncertainty/coefficients_6.csv",
  "Red Spruce", 6.018, -0.282, 0.005
)

white_ash <- calcular_biomasa_especie(
  "1_Coefficients_Species_Uncertainty/coefficients_7.csv",
  "White Ash", 3.203, -0.234, 0.006
)

# Combine all species
# Combinar todas las especies
todas_especies <- rbind(
  sugar_maple,
  yellow_birch,
  american_beech,
  eastern_hemlock,
  red_maple,
  red_spruce,
  white_ash
)

# View result
# Ver resultado
View(todas_especies)

# Save Guardar
write.csv(todas_especies, "all_species_uncertainty.csv", row.names = FALSE)




# Convertir diámetros a cm y filtrar solo los diámetros deseados
# Graph for 5, 10, 30, 50, 70, 100 inches
especies_filtradas <- todas_especies %>%
  filter(diameter %in% c(5, 10, 30, 50, 70, 100))

library(ggplot2)

ggplot(especies_filtradas, aes(x = diametercm, y = CVsd_mean, color = species)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = c(12.7, 25.4, 76.2, 127, 177.8, 254),
    labels = c("12.7", "25.4", "76.2", "127", "177.8", "254")
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Diameter (cm)",
    y = "Coefficient of Variation (%)",
    color = "Species",
    title = "Coefficient of variation (sd/mean) by species and different diameter"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar las etiquetas
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  )

# Simple chart # Gráfico simple
library(ggplot2)

ggplot(todas_especies, aes(x = diameter, y = mean, color = species)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 2) +
  labs(x = "Diameter (inches)", y = "Biomass", color = "Species") +
  theme_classic()


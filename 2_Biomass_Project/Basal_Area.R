#Este código grafica por especie la cantidad de area basal y el número de individuos

#This code graphs by species the amount of basal area and the number of individuals

library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Función para graficar área basal e individuos por especie
plot_basal_area <- function(tree_file, species_file) {
  
  # Leer datos
  NewCFI <- read.csv(tree_file)
  SpeciesCode <- read_excel(species_file)
  
  # Calcular área basal
  NewCFI$BasalAreacm2 <- pi * (NewCFI$DBHcm / 2)^2
  NewCFI$BasalAream2 <- NewCFI$BasalAreacm2 / 10000
  
  # Merge con código de especies
  BS_A_Sp <- merge(NewCFI, SpeciesCode, by.x = "Species", by.y = "Code")
  
  # Resumir datos
  df_summary <- BS_A_Sp %>%
    filter(Species.y != "Noncommercial") %>%   # 🔹 aquí el filtro
    group_by(Species.y) %>%
    summarise(
      BasalA = sum(BasalAream2, na.rm = TRUE),
      Individuals = n()
    )
  
  # Crear gráfico
  p <- ggplot(df_summary, aes(x = reorder(Species.y, -BasalA))) +
    geom_col(aes(y = BasalA), fill = "forestgreen", alpha = 0.7) +
    geom_line(aes(y = Individuals * (max(BasalA) / max(Individuals)), group = 1),
              color = "steelblue", size = 1.2) +
    geom_point(aes(y = Individuals * (max(BasalA) / max(Individuals))),
               color = "steelblue", size = 2) +
    scale_y_continuous(
      name = expression("Basal area (m"^2*")"),
      sec.axis = sec_axis(~ . / (max(df_summary$BasalA) / max(df_summary$Individuals)),
                          name = "Number of individuals")
    ) +
    labs(
      x = "Species",
          ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
      axis.title.y = element_text(color = "forestgreen", face = "bold"),
      axis.title.y.right = element_text(color = "steelblue", face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  return(p)
}

plot_basal_area("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data2021.csv", "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/SpeciesCode.xlsx")










library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

Original<-read_excel("4_General_Resources/OriginalNU.xlsx")
#View(Original)

library(readxl)
Original <- read_excel("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/OriginalNU.xlsx")
View(OriginalNU)

#common name 
Original <- dplyr::rename(Original, Species = `common name`)

# Supongamos que CombinationSI tiene columnas:
# Species, MC_a, MC_b, MC_c
# y DBHmm (diámetro en mm)

# Definir un rango de DBH en mm (ejemplo 1 a 50 cm = 10 a 500 mm)
dbh_range <- data.frame(DBHmm = seq(10, 500, by = 10))

# Expandir por especie
curves <- Original %>%
  select(Species, a, b, c) %>%
  distinct() %>%
  crossing(dbh_range) %>%
  mutate(
    DBHcm = DBHmm / 10,                  # 🔹 convertir mm a cm
    Y_kg = a + b * DBHmm + c * (DBHmm^2)
  )

ggplot(curves, aes(x = DBHcm, y = Y_kg, color = Species)) +
  geom_line(size = 1.2) +
  labs(
    x = "DBH (cm)",                      # eje X en cm
    y = "Aboveground Live Biomass (kg)",
    title = "Biomass as a function of DBH"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),  # 🔹 centra el título
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )

##########################################################################

####### SECOND OPTION #####

##########################################################################

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

# Definir linetypes alternados
line_types <- c("solid", "dashed", "solid", "dashed", "solid", "dashed", "solid")

# Plot
ggplot(curves, aes(x = DBHcm, y = Y_kg, color = Species, linetype = Species)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = okabe_ito) +
  scale_linetype_manual(values = line_types) +
  labs(
    x = "DBH (cm)",
    y = "Aboveground Live Biomass (kg)",
    title = "Biomass as a Function of DBH"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

################################################################

#Ordenado 



library(dplyr)
library(ggplot2)

# Calcular el valor máximo de Y_kg por especie
species_order <- curves %>%
  group_by(Species) %>%
  summarise(maxY = max(Y_kg, na.rm = TRUE)) %>%
  arrange(desc(maxY)) %>%      # de mayor a menor
  pull(Species)                # extrae el vector de especies ordenadas

# Convertir Species en factor con niveles ordenados
curves$Species <- factor(curves$Species, levels = species_order)

# Paleta Okabe–Ito
okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00",
  "#CC79A7"
)

# Linetypes alternados
line_types <- c("solid", "dashed", "solid", "dashed", "solid", "dashed", "solid")

# Plot con leyenda ordenada
ggplot(curves, aes(x = DBHcm, y = Y_kg, color = Species, linetype = Species)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = okabe_ito) +
  scale_linetype_manual(values = line_types) +
  labs(
    x = "DBH (cm)",
    y = "Aboveground Live Biomass (kg)",
    title = "Biomass as a Function of DBH"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )





library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

Original<-read_excel("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/SD/Final_Bio/OriginalNU.xlsx")
#View(Original)

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
    y = "Biomass (kg)",
    title = "Biomass as a function of DBH"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),  # 🔹 centra el título
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )


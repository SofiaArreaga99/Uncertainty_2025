# Análisis de Distribución Diamétrica por Especie
# Código optimizado y documentado

# Librerías necesarias
# Required libraries

library(readr)
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. CARGA DE DATOS - DATA LOADING
# ==============================================================================

data2021_Trees <- read_csv("4_General_Resources/data2021.csv")
Code_SpEq <- read_csv("4_General_Resources/Code_SpEq.csv")

# ==============================================================================
# 2. PROCESAMIENTO DE DATOS - DATA PROCESSING
# ==============================================================================

# 2.1 Clasificación en intervalos diamétricos de 5 cm
# Classification in 5 cm diameter intervals
plots_dbh <- data2021_Trees %>%
  filter(!is.na(DBHcm)) %>%  # Remover NAs desde el inicio
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

# ==============================================================================
# 3. VISUALIZACIÓN
# ==============================================================================

# Paleta Okabe–Ito (color-blind friendly)
okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7" ) # reddish purple

# 3.1 Gráfico principal de distribución diamétrica
# Main graph of diameter distribution
p1 <- ggplot(distribution_final,
             aes(x = DBH_interval,
                 y = mean_trees_ha,
                 color = common_name,
                 group = common_name)) +
  geom_line() + 
  geom_point() +
  labs(
    x = "Diameter class (cm)",
    y = expression(paste("Trees ha"^"-1")),
    color = "Species"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "italic"),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90")
  ) + scale_color_manual(values = okabe_ito) 

print(p1)

# 3.2 Gráfico alternativo con barras apiladas
# Alternative chart with stacked bars
p2 <- ggplot(distribution_final,
             aes(x = DBH_interval,
                 y = mean_trees_ha,
                 fill = common_name)) +
  geom_col(position = "stack", alpha = 0.8) +
  labs(
    x = "Diameter class (cm)",
    y = expression(paste("Trees ha"^"-1")),
    fill = "Species"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(face = "italic")
  ) 

print(p2)

# ==============================================================================
# 4. EXPORTAR RESULTADOS - EXPORT RESULTS
# ==============================================================================

# Guardar tabla de resultados
write_csv(distribution_final, "figures/diameter_distribution_by_species2021.csv")

# Guardar gráficos
ggsave("figures/fig_diameter_distribution_lines.png", p1, 
       width = 10, height = 6, dpi = 300)
ggsave("figures/fig_diameter_distribution_stacked.png", p2, 
       width = 10, height = 6, dpi = 300)

# ==============================================================================
# 5. ESTADÍSTICAS DESCRIPTIVAS (opcional para el artículo)
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

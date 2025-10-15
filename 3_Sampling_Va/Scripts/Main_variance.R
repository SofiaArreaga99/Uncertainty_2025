#Main_variance

# Loading function :) - carga la función
source("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/Scripts/functions_biomass.R")

#--------------------------------------------------------------------
# ....1970
#--------------------------------------------------------------------

# Execute the function- Ejecutar función
resultados <- calc_biomass("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/OriginalNUcsv.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data1970.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/ComPlot.xlsx", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/Comp.xlsx")

# save results - Guardar resultados 
write.csv(resultados$Plots_Compa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/Plots_Compa1970.csv", row.names = FALSE)
write.csv(resultados$SampVa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/SampVa1970.csv", row.names = FALSE)

#--------------------------------------------------------------------
# ....1981
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/OriginalNUcsv.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data1981.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/ComPlot.xlsx", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/Comp.xlsx")

# Guardar resultados
write.csv(resultados$Plots_Compa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/Plots_Compa1981.csv", row.names = FALSE)
write.csv(resultados$SampVa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/SampVa1981.csv", row.names = FALSE)


#--------------------------------------------------------------------
# ....1991
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/OriginalNUcsv.csv", 
                            "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data1991.csv", 
                            "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/ComPlot.xlsx", 
                            "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/Comp.xlsx")

# Guardar resultados
write.csv(resultados$Plots_Compa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/Plots_Compa1991.csv", row.names = FALSE)
write.csv(resultados$SampVa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/SampVa1991.csv", row.names = FALSE)


#--------------------------------------------------------------------
# ....2001
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/OriginalNUcsv.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data2001.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/ComPlot.xlsx", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/Comp.xlsx")

# Guardar resultados
write.csv(resultados$Plots_Compa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/Plots_Compa2001.csv", row.names = FALSE)
write.csv(resultados$SampVa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/SampVa2001.csv", row.names = FALSE)


#--------------------------------------------------------------------
# ....2011
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/OriginalNUcsv.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data2011.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/ComPlot.xlsx", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/Comp.xlsx")

# Guardar resultados 
write.csv(resultados$Plots_Compa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/Plots_Compa2011.csv", row.names = FALSE)
write.csv(resultados$SampVa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/SampVa2011.csv", row.names = FALSE)


#--------------------------------------------------------------------
# ....2021
#--------------------------------------------------------------------

# Ejecutar función
resultados <- calc_biomass("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/OriginalNUcsv.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data2021.csv", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/ComPlot.xlsx", 
                           "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/Comp.xlsx")

# Guardar resultados
write.csv(resultados$Plots_Compa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/Plots_Compa2021.csv", row.names = FALSE)
write.csv(resultados$SampVa, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/3_Sampling_Va/output/SampVa2021.csv", row.names = FALSE)







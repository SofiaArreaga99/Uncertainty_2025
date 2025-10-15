# Cargar funciones
source("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/2_Biomass_Project/R/Load_packages.R")
source("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/2_Biomass_Project/R/Read_data.R")
source("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/2_Biomass_Project/R/Process_iteration.R")
source("C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/2_Biomass_Project/R/Run_iterations.R")

# Ejecutar flujo
load_packages()

#--------------------------------------------------------------------
# ....1970
#--------------------------------------------------------------------

data1970 <- read_data(
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/MergeCov.csv",
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data1970.csv"
)


results1970 <- run_iterations(data1970$frameCoeff2, data1970$Plot1HWF)

print(results1970)


#--------------------------------------------------------------------
# ....1981
#--------------------------------------------------------------------

data1981 <- read_data(
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/MergeCov.csv",
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data1981.csv"
)

results1981 <- run_iterations(data1981$frameCoeff2, data1981$Plot1HWF)

print(results1981)
# Guardar resultados si quieres
#write.csv(results1981, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/2_Biomass_project/outputBm/PlotsTotal1981.csv", row.names = FALSE)

#--------------------------------------------------------------------
# ....1991
#--------------------------------------------------------------------

data1991 <- read_data(
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/MergeCov.csv",
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data1991.csv"
)

results1991 <- run_iterations(data1991$frameCoeff2, data1991$Plot1HWF)

print(results1991)


#--------------------------------------------------------------------
# ....2001
#--------------------------------------------------------------------

data2001 <- read_data(
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/MergeCov.csv",
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data2001.csv"
)

results2001 <- run_iterations(data2001$frameCoeff2, data2001$Plot1HWF)

print(results2001)

#--------------------------------------------------------------------
# ....2011
#--------------------------------------------------------------------

data2011 <- read_data(
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/MergeCov.csv",
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data2011.csv"
)

results2011 <- run_iterations(data2011$frameCoeff2, data2011$Plot1HWF)

print(results2011)

#--------------------------------------------------------------------
# ....2021
#--------------------------------------------------------------------

data2021 <- read_data(
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/MergeCov.csv",
  "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/4_General_Resources/data2021.csv"
)

results2021 <- run_iterations(data2021$frameCoeff2, data2021$Plot1HWF)

print(results2021)

write.csv(results2021, "C:/Users/vanco/Desktop/ResearchR/Uncertainty_2025/2_Biomass_project/outputBm/PlotsTotal2021.csv", row.names = FALSE)


#--------------------------------------------------------------------


                          
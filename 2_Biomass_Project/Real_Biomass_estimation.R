# Estimation simple biomass " Should be easy! easier??


library(readxl)
library(dplyr)
library(readr)
library(doBy)


# 1. Import data
#frameCoeff2 <- read_delim(coeff_file, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
#Plot1HWF <- read.csv(inv_file)
library(readr)
TreeBiomass <- read_csv
View(TreeBiomass)

frameCoeff2 <- read_delim("4_General_Resources/TreeBiomass_2csv.csv",
                          delim = ";")

Plot1HWF <- read_csv("4_General_Resources/data2021.csv")

View(frameCoeff2)
# 2. Prepare coefficients
frameCoeffCV <- data.frame(
  especie = c("HacerSac", "BetulaAllegh","FagusGrandi","TsugaCana", "HacerRu", "PiceaRube", "FraxinusAmeri"), 
  Equation = c("1","2","3","4","5","6","7"),
  MC_a = c(frameCoeff2$aHacerSa, frameCoeff2$aBetulaA, frameCoeff2$aFagusG, 
           frameCoeff2$aTsuga, frameCoeff2$aHacerRu, frameCoeff2$aPiceaRu, frameCoeff2$aFraxinusA),
  MC_b = c(frameCoeff2$bHacerSa, frameCoeff2$bBetulaA, frameCoeff2$bFagusG, 
           frameCoeff2$bTsuga, frameCoeff2$bHacerRu, frameCoeff2$bPiceaRu, frameCoeff2$bFraxinusA),
  MC_c = c(frameCoeff2$cHacerSa, frameCoeff2$cBetulaA, frameCoeff2$cFagusG, 
           frameCoeff2$cTsuga, frameCoeff2$cHacerRu, frameCoeff2$cPiceaRu, frameCoeff2$cFraxinusA)
)


# 3. Biomass per tree
CombinationSI <- merge(frameCoeffCV, Plot1HWF, by="Equation")
CombinationSI$DBHmm <- CombinationSI$DBHcm * 10
CombinationSI$Y_kg <- CombinationSI$MC_a + CombinationSI$MC_b*CombinationSI$DBHmm +
  CombinationSI$MC_c*(CombinationSI$DBHmm^2)

# 4. Biomass per plot and scale in kg/ha
SumBioPlots <- summaryBy(Y_kg ~ Plot + PlotSize, data=CombinationSI, FUN=sum)
SumBioPlots <- SumBioPlots %>%
  mutate(PloKg_Ha = ifelse(PlotSize=="POLE", (Y_kg.sum*10000)/202.343,
                           ifelse(PlotSize=="SAW", (Y_kg.sum*10000)/809.372, NA)))

Sum_A_Plots <- summaryBy(PloKg_Ha~Plot, data=SumBioPlots, FUN=sum)
Average_A_Plots <- mean(Sum_A_Plots$PloKg_Ha, na.rm = TRUE)

resultados_df <- data.frame(PloKg_Ha = numeric(), stringsAsFactors = FALSE)


resultados_df <- rbind(resultados_df, data.frame(PloKg_Ha = Average_A_Plots))

resultados_df$mg_ha <- ((resultados_df$PloKg_Ha / 1000) / 2)

total_mean_mg_ha <- mean(resultados_df$mg_ha, na.rm = TRUE)
total_sd_mgha    <- sd(resultados_df$mg_ha, na.rm = TRUE)
CV               <- ((total_sd_mgha / total_mean_mg_ha)) * 100

print(list(
  resultados_df    = resultados_df,
  total_mean_mg_ha = round(total_mean_mg_ha, 2),
  total_sd_mgha    = round(total_sd_mgha, 2),
  CV               = round(CV, 2)
))





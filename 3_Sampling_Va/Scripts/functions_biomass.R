calc_biomass <- function(coeff_file, inv_file, complot_file, comp_file) {
  library(readxl)
  library(dplyr)
  library(readr)
  library(doBy)
  
  # 1. Import data
  frameCoeff2 <- read_delim(coeff_file, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  Plot1HWF <- read.csv(inv_file)
  ComPlot <- read_excel(complot_file)
  SampVa <- read_excel(comp_file)
  
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
  Plots_Compa <- merge(Sum_A_Plots, ComPlot, by="Plot")
  
  # 5. Statistics by compartment (dynamic)
  mean_by_comp <- aggregate(PloKg_Ha.sum ~ Comp, data=Plots_Compa, FUN=mean)
  var_by_comp  <- aggregate(PloKg_Ha.sum ~ Comp, data=Plots_Compa, FUN=var)
  n_by_comp    <- aggregate(PloKg_Ha.sum ~ Comp, data=Plots_Compa, FUN=length)
  
  # Dynamic Merge with SampVa (This was neccesary because we are using different years)
  SampVa <- merge(SampVa, mean_by_comp, by="Comp", all.x=TRUE)
  SampVa <- merge(SampVa, var_by_comp, by="Comp", all.x=TRUE, suffixes=c("_mean","_var"))
  SampVa <- merge(SampVa, n_by_comp, by="Comp", all.x=TRUE, suffixes=c("","_n"))
  
  # Rename clear columns (This was neccesary because we are using different years)
  names(SampVa)[names(SampVa) == "PloKg_Ha.sum"] <- "n"
  names(SampVa)[names(SampVa) == "PloKg_Ha.sum_mean"] <- "Mean"
  names(SampVa)[names(SampVa) == "PloKg_Ha.sum_var"] <- "Var"
  
  # 6. Weighted calculations
  SampVa$Wh <- SampVa$Area_Ha / sum(SampVa$Area_Ha)
  SampVa$Wh_2xSh_2_nh <- SampVa$Wh^2 * SampVa$Var / SampVa$n
  SampVa$WhxYbarh <- SampVa$Wh * SampVa$Mean
  
  # 7. Final results
  MgCHa <- ((sum(SampVa$Wh * SampVa$Mean, na.rm=TRUE))/1000)/2
  Var_mean <- sum(SampVa$Wh_2xSh_2_nh, na.rm=TRUE)
  Sd_mean <- sqrt(Var_mean)
  VarMgC <- (Var_mean/1000)/2
  SdMgC <- (Sd_mean/1000)/2
  
  # Return results
  return(list(
    SampVa = SampVa,
    Plots_Compa = Plots_Compa,
    MgCHa = MgCHa,
    VarMgC = VarMgC,
    SdMgC = SdMgC
  ))
}


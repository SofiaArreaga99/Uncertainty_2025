#Aquí se generan las iteraciones, empleando la matriz generada para los distintos coeficientes 
#y se escalan las parcelas a Ha los valores de las parcelas 

#Here the iterations are generated, using the matrix generated for the different coefficients
#and the plots are scaled to Ha the plot values

process_iteration <- function(Extracction, Plot1HWF) {
  
  frameCoeffCV <- data.frame(
    especie = c("HacerSac", "BetulaAllegh","FagusGrandi","TsugaCana", "HacerRu", "PiceaRube", "FraxinusAmeri"), 
    Equation = c("1","2","3","4","5","6","7"),
    MC_a = c(Extracction$aHacerSa, Extracction$aBetulaA,Extracction$aFagusG, Extracction$aTsuga,Extracction$aHacerRu, Extracction$aPiceaRu, Extracction$aFraxinusA),
    MC_b = c(Extracction$bHacerSa, Extracction$bBetulaA,Extracction$bFagusG, Extracction$bTsuga,Extracction$bHacerRu, Extracction$bPiceaRu, Extracction$bFraxinusA),
    MC_c = c(Extracction$cHacerSa, Extracction$cBetulaA,Extracction$cFagusG, Extracction$cTsuga,Extracction$cHacerRu, Extracction$cPiceaRu, Extracction$cFraxinusA)
  )
  
  CombinationSI <- merge(frameCoeffCV, Plot1HWF, by.x = "Equation")
  CombinationSI$DBHmm <- CombinationSI$DBHcm * 10
  
  CombinationSI$Y_kg <- CombinationSI$MC_a + CombinationSI$MC_b * CombinationSI$DBHmm + 
    CombinationSI$MC_c * (CombinationSI$DBHmm^2)
  
  SumBioPlots <- summaryBy(Y_kg ~ Plot + PlotSize, data = CombinationSI, FUN = sum)
  
  SumBioPlots <- SumBioPlots %>%
    mutate(PloKg_Ha = ifelse(PlotSize == "POLE", ((Y_kg.sum*10000)/202.343), 
                             ifelse(PlotSize == "SAW", ((Y_kg.sum*10000)/809.372), NA)))
  
  Sum_A_Plots <- summaryBy(PloKg_Ha ~ Plot, data = SumBioPlots, FUN = sum)
  Average_A_Plots <- mean(Sum_A_Plots$PloKg_Ha, na.rm = TRUE)
  
  return(Average_A_Plots)
}

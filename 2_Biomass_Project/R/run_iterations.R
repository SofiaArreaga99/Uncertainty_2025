run_iterations <- function(frameCoeff2, Plot1HWF) {
  resultados_df <- data.frame(PloKg_Ha = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:nrow(frameCoeff2)) {
    Extracction <- frameCoeff2[i, ]
    Average_A_Plots <- process_iteration(Extracction, Plot1HWF)
    
    
    resultados_df <- rbind(resultados_df, data.frame(PloKg_Ha = Average_A_Plots))
  }
  
  
  resultados_df$mg_ha <- ((resultados_df$PloKg_Ha / 1000) / 2)
  
  total_mean_mg_ha <- mean(resultados_df$mg_ha, na.rm = TRUE)
  total_sd_mgha    <- sd(resultados_df$mg_ha, na.rm = TRUE)
  CV               <- ((total_sd_mgha / total_mean_mg_ha)) * 100
  
  return(list(
    resultados_df    = resultados_df,
    total_mean_mg_ha = round(total_mean_mg_ha, 2),
    total_sd_mgha    = round(total_sd_mgha, 2),
    CV               = round(CV, 2)
  ))
}



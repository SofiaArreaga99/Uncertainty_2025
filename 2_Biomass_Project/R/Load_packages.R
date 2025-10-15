#Cargamos los paquetes que estaremos utilizando 
#Here We load the packages that we will be using

load_packages <- function() {
  packages <- c(
    "readxl", "plyr", "doBy", "dplyr", 
    "knitr", "ggplot2", "RColorBrewer", "readr"
  )
  
  #Install packages just in case
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  
  # loading 
  invisible(lapply(packages, library, character.only = TRUE))
  
  message("✅ Packages loaded successfully")
}





#Este script ayuda solamente a cargar los archivos que tenemos de los inventarios y la lista de los coeficientes

#This script only helps to load the files we have of the inventories and the list of coefficients

read_data <- function(coeff_file, inventory_file) {
  frameCoeff2 <- read_delim(coeff_file, delim = ",", 
                            escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  Plot1HWF <- read.csv(inventory_file, 
                       header = TRUE,
                       stringsAsFactors = FALSE)
  
  return(list(frameCoeff2 = frameCoeff2, Plot1HWF = Plot1HWF))
}





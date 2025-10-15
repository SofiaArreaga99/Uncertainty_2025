# Sugar maple hacer saccharum

#Monteith 1979 (New York), Range: 2.5 - 55 cm, Whole tree (above stump)

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)
library(readxl)
#First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH
# Specie: Sugar Maple


# Equation 1 Monteith 1979 (New York)
# Units DBH mm and biomass kg 
set.seed(124)

rsqp_1<-0.981 ##Published R^2 value 
minDBH_1<-2.5*10 #From Jenkin´s *10 conversion to mm
maxDBH_1<-55*10 #From Jenkin´s *10 conversion to mm
a_1<- 5.248 
b_1<- -0.366
c_1<- 0.008 
d_1<- 2

##CREATE 10,000 matrix

test_1 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) #Old form 
#test9_1 <- matrix(rnorm(10000 * 1000, mean = 0, sd = 0.1), nrow = 10000, ncol = 1000) # one form 
#test9_1 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) / 10 # Other form 
test_1 <- pmin(pmax(test_1, -2), 2)  # truncar a ±3σ ->This works

##Create Random DBHs over the range sampled, dbh= mm unit

dbh_1 <- minDBH_1 + (maxDBH_1 - minDBH_1) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = a+b*dia+c*(dia^d)

meany_1 <-a_1 + b_1*(dbh_1) + c_1*((dbh_1)^d_1)


#View(meany9_1)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys_1 <- matrix(rep(meany_1, times = 1000), nrow = length(meany_1), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomases. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
#stdevs9_1 <- seq(0.1, 100, length.out=1000)  #works better
stdevs_1 <- seq(0.05, 1, length.out=1000)
stdevs2_1 <- matrix(rep(stdevs_1, each = 10000), nrow = 10000, ncol = length(stdevs_1))  
dbh2_1 <- matrix(rep(dbh_1, times = 1000), nrow = length(dbh_1), ncol = 1000)

#psuedys9_1 <- ys9_1 + stdevs2_9_1 * test9_1  # this makes the new biomases if no heteroscedasticity #

psuedys_1 <- ys_1 + stdevs2_1 * test_1 * dbh2_1 # This makes the new biomasses if heteroscedasticity

#this makes the new biomasses with heteroscedasticity #

rsq2_1 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst_1 <- sum((psuedys_1[, i] - mean(psuedys_1[, i]))^2)  # Total sum of squares
  sse_1 <- sum((psuedys_1[, i] - meany_1)^2)  # Sum of squares of the error 
  rsq2_1[i] <- 1 - (sse_1 / sst_1)  # Coefficient of determination R²
}

## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop

diffs_1 <- abs(rsq2_1 - rsqp_1)
I_1 <- which.min(diffs_1)  # Find the index of the minimum value
BM_1<- psuedys_1[, I_1]  # Select corresponding column

I_1
diffs_1[260]
rsq2_1[260]

## Create figure for checking if result is reasonable ##

plot((dbh_1/10), BM_1, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Hacer saccharum")

# Write the data in an Excel file

PseudoData1 <- data.frame(dbh_1, BM_1)
#PseudoDataTsugaCa1 <- subset(PseudoDataTsugaCa1, BMTsugaCa_1>1)
#head(PseudoDataTsugaCa1)
#head(PseudoDataTsugaCa1)
## Logaritmic differences #meany9_1 <-a_9_1 + b_9_1*(dbhTsugaCa_1) + c_9_1*((dbhTsugaCa_1)^d_9_1)


noiter <- 10000
coefficients_1 <- data.frame(a = rep(NA, noiter), b = rep(NA, noiter), c = rep(NA, noiter))


for (i in 1:noiter) {
  
  datatofit_1 <- sample_n(PseudoData1, 33, replace = FALSE)
  modelfit_1 <- lm(BM_1 ~ dbh_1 + I((dbh_1)^2), data = na.omit(datatofit_1))
  coefficients_1[i, ] <- unname(coef(modelfit_1))
}

#Preparing data to export
PseudoData1$eq <-"eq_1"
PseudoData1$dbh_1 <- PseudoData1$dbh_1 / 10  #convertion to cm
head(PseudoData1)
# Muestra las primeras filas
#head(coefficients9_1)
#head(datatofit_1)

#Mean
aHacerSac<-mean(coefficients_1$a)
bHacerSac<-mean(coefficients_1$b)
cHacerSac<-mean(coefficients_1$c)


#any(is.na(datatofit)) #NA revision in the data

#SD
SDaHacerSac<-sd(coefficients_1$a) #standar deviation intercept
SDbHacerSac<-sd(coefficients_1$b)
SDcHacerSac<-sd(coefficients_1$c)

#Percentile
#50th percentile
QaHacerSac<-quantile(coefficients_1$a, probs = 0.5)
QbHacerSac<-quantile(coefficients_1$b, probs = 0.5)
QcHacerSac<-quantile(coefficients_1$c, probs = 0.5)


#hist(coefficients9_1$a)
#coefficients_1<-read.csv("C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/New_pseudodata/Coefficients/HacerSac_C.csv")
#head(coefficients_1)
#
hist(coefficients_1$aHacerSa,
     main = "Coefficient `a` Acer saccharum",
     xlab = "Coefficient a",
     ylab = "Frequency",
     col = "#FFD700",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)          # tamaño de números en ejes
# Add vertical line at 5.248
abline(v = 5.248, col = "#999999", lwd = 2, lty = 2)

#

hist(coefficients_1$bHacerSa,
     main = "Coefficient `b` Acer saccharum",
     xlab = "Coefficient b",
     ylab = "Frequency",
     col = "#999999",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)  
# Add vertical line 
abline(v = -0.366, col = "#800080", lwd = 2, lty = 2)

#
hist(coefficients_1$cHacerSa,
     main = "Coefficient `c` Acer saccharum",
     xlab = "Coefficient c",
     ylab = "Frequency",
     col = "#CC79A7",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1) 
# Add vertical line
abline(v = 0.008, col = "#0000FF", lwd = 2, lty = 2)
#

#plot(a~c,data=coefficients_1)

plot(aHacerSa ~ cHacerSa, data = coefficients_1,
     main = "Relationship between a and c",
     xlab = "c ",
     ylab = "a ",
     pch = 1,            # solid circles
     col = "#0072B2",   # point color
     cex = 1,           # point size
     cex.lab = 1.2,       # axis label size
     cex.main = 1.3,      # title size
     bty = "l")           # box only on two sides

#
plot(aHacerSa ~ bHacerSa, data = coefficients_1,
     main = "Relationship between a and b",
     xlab = "b ",
     ylab = "a ",
     pch = 1,            # solid circles
     col = "#0072B2",   # point color
     cex = 1,           # point size
     cex.lab = 1.2,       # axis label size
     cex.main = 1.3,      # title size
     bty = "l")           # box only on two sides
#
plot(bHacerSa ~ cHacerSa, data = coefficients_1,
     main = "Relationship between b and c",
     xlab = "c ",
     ylab = "b ",
     pch = 1,            # solid circles
     col = "#0072B2",   # point color
     cex = 1,           # point size
     cex.lab = 1.2,       # axis label size
     cex.main = 1.3,      # title size
     bty = "l")           # box only on two sides


range(coefficients_1$a)

#View(coefficients9_1)


# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients_1) <- c("aHacerSa", "bHacerSa","cHacerSa")
# Adding the correlative 
coefficients_1$correlative <- seq_len(nrow(coefficients_1))

head(coefficients_1)

write.csv(coefficients_1, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/New_pseudodata/Coefficients/HacerSac_C.csv", 
          row.names = FALSE)


#View(PseudoDataTsugaCa1)
#plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa, pch = 16, xlab = "DBH (mm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")
#plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa,pch = 16, col = rgb(0.2, 0.4, 0.6, 0.5), azul semi-transparente, xlab = "DBH (mm)", ylab = "Biomass (kg)", main = "Tsuga canadensis",cex = 1.2)  # tamaño de los puntos
#grid()  # agrega rejilla

# Specifies the full path to save the file
#write.csv(PseudoDataTsugaCa1, file = "TsugaCa.csv1", row.names = FALSE)
write.csv(PseudoData1, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/Biomass_Year/HacerSac.csv", 
          row.names = FALSE)



####

PseudoData1<-read.csv("C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/Biomass_Year/HacerSac.csv")

# Real Data #
Sugar_Monteith <- data.frame(
  location = "New York",
  source   = "Monteith 1979",
  dbh      = seq(from = 2.5, to = 55, by = 2.5),
  kg       = c(1,6,21,45,78,121,174,236,308,389,480,580,690,809,937,1076,1223,1380,1547,1723,1909,2104)
)


head(PseudoData1)

# pseudo data
plot(PseudoData1$dbh_1, PseudoData1$BM_1,
     pch = 1,
     col = rgb(0.2, 0.4, 0.6, 0.5),   # azul semi-transparente
     xlab = "DBH (cm)", 
     ylab = "Biomass (kg)", 
     main = "Hacer saccharum",
     cex = 1.2)
grid()

# Monteith 1979
points(Sugar_Monteith$dbh, Sugar_Monteith$kg,
       pch = 17,   # triángulos
       col = rgb(0.8, 0.2, 0.2, 0.7), # rojo semi-transparente
       cex = 1.3)

legend("topleft",
       legend = c("Pseudo data", "Monteith 1979"),
       col = c(rgb(0.2,0.4,0.6,0.5), rgb(0.8,0.2,0.2,0.7)),
       pch = c(16, 17),
       bty = "n")

library(MASS)

##############

#Only 1000

# Seleccionar 1000 filas aleatorias
set.seed(555)  # para reproducibilidad
subset_idx <- sample(1:nrow(PseudoData1), 800)

# Graficar solo esas 1000
plot(PseudoData1$dbh_1[subset_idx], PseudoData1$BM_1[subset_idx],
     pch = 1,
     col = "#FFD700",   # azul semi-transparente
     xlab = "DBH (cm)", 
     ylab = "Biomass (kg)", 
     main = "Acer saccharum",
     cex = 1.2)
grid()

# Monteith 1979
points(Sugar_Monteith$dbh, Sugar_Monteith$kg,
       pch = 17,   # triángulos
       col = "#0072B2", # rojo semi-transparente
       cex = 1)

legend("topleft",
       legend = c("Pseudo data", "Predicted values, Monteith 1979"),
       col = c("#FFD700","#0072B2"),
       pch = c(16, 17),
       bty = "n")


##############
#Log in Y (Biomass)

# Pseudo data con escala log en Y
plot(PseudoData1$dbh_1, PseudoData1$BM_1,
     pch = 16,
     col = "#006400",   # azul semi-transparente
     xlab = "DBH (cm)", 
     ylab = "Log Biomass (kg)", 
     main = "Acer saccharum",
     cex = 1.2,
     log = "y")   # escala log en eje Y
grid()

# Monteith 1979
points(Sugar_Monteith$dbh, Sugar_Monteith$kg,
       pch = 17,   # triángulos
       col = "red",
       cex = 1.3)

legend("bottomright",
       legend = c("Pseudo data", "Monteith 1979"),
       col = c("#006400", "red"),
       pch = c(16, 17),
       bty = "n")
#
#
#






coefficients_1Ha <- read.csv("C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/New_pseudodata/Coefficients/HacerSac_C.csv")
#View(coefficients_1Ha)
library(dplyr)

coefficients_1Ha <- coefficients_1Ha %>%
  dplyr::select(-correlative)


View(coefficients_1Ha)
# Calcular media y matriz de covarianza
cov_matrix_HacerSac <- cov(coefficients_1Ha)        # ahora es 3x3 si coefficients1 tiene 3 columnas
mean_vector_HacerSac <- colMeans(coefficients_1Ha)

View(cov_matrix_HacerSac)

# Simular nuevos tríos de a, b y c
sim_abc_HacerSac <- as.data.frame(
  mvrnorm(n = 10000, mu = mean_vector_HacerSac, Sigma = cov_matrix_HacerSac)
)

# Renombrar columnas
colnames(sim_abc_HacerSac) <- c("aHacerSac", "bHacerSac", "cHacerSac")

# Agregar columna correlative
sim_abc_HacerSac$correlative <- seq_len(nrow(sim_abc_HacerSac))

View(sim_abc_HacerSac)

hist(sim_abc_HacerSac$aHacerSac)
range(sim_abc_HacerSac$aHacerSac)

#PiceaRube

# Hacer rubrum
#Monteith 1979 (New York), Range: 2.5 - 55 cm, Whole tree (above stump)

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

#First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH
set.seed(123)

# Equation 1 Monteith 1979 (New York)
# Units DBH mm and biomass kg 

rsqp_6<-0.962 ##Published R^2 value 
minDBH_6<-2.5*10 #From Jenkin´s *10 conversion to mm
maxDBH_6<-55*10 #From Jenkin´s *10 conversion to mm
a_6<- 6.018 
b_6<- -0.282
c_6<- 0.005 
d_6<- 2

##CREATE 10,000 matrix

test_6 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) #Old form 
test_6 <- pmin(pmax(test_6, -3), 3)  # truncar a ±3σ ->This works

##Create Random DBHs over the range sampled, dbh= mm unit

dbh_6 <- minDBH_6 + (maxDBH_6 - minDBH_6) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = a+b*dia+c*(dia^d)

meany_6 <-a_6 + b_6*(dbh_6) + c_6*((dbh_6)^2)


#View(meany9_1)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys_6 <- matrix(rep(meany_6, times = 1000), nrow = length(meany_6), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomases. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
#stdevs9_1 <- seq(0.1, 100, length.out=1000)  #works better
stdevs_6 <- seq(0.05, 1, length.out=1000)
stdevs2_6 <- matrix(rep(stdevs_6, each = 10000), nrow = 10000, ncol = length(stdevs_6))  
dbh2_6 <- matrix(rep(dbh_6, times = 1000), nrow = length(dbh_6), ncol = 1000)

#psuedys9_1 <- ys9_1 + stdevs2_9_1 * test9_1  # this makes the new biomases if no heteroscedasticity #

psuedys_6 <- ys_6 + stdevs2_6 * test_6 * dbh2_6 # This makes the new biomasses if heteroscedasticity

#this makes the new biomasses with heteroscedasticity #

rsq2_6 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst_6 <- sum((psuedys_6[, i] - mean(psuedys_6[, i]))^2)  # Total sum of squares
  sse_6 <- sum((psuedys_6[, i] - meany_6)^2)  # Sum of squares of the error 
  rsq2_6[i] <- 1 - (sse_6 / sst_6)  # Coefficient of determination R²
}

## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop

diffs_6 <- abs(rsq2_6 - rsqp_6)
I_6 <- which.min(diffs_6)  # Find the index of the minimum value
BM_6<- psuedys_6[, I_6]  # Select corresponding column

I_6
diffs_6[214]
rsq2_6[214]

## Create figure for checking if result is reasonable ##

plot((dbh_6/10), BM_6, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Picea rubens")

# Write the data in an Excel file

PseudoData_6 <- data.frame(dbh_6, BM_6)

# iteration 

noiter <- 10000
coefficients_6 <- data.frame(a = rep(NA, noiter), b = rep(NA, noiter), c = rep(NA, noiter))


for (i in 1:noiter) {
  
  datatofit_6 <- sample_n(PseudoData_6, 33, replace = FALSE)
  modelfit_6 <- lm(BM_6 ~ dbh_6 + I((dbh_6)^2), data = na.omit(datatofit_6))
  coefficients_6[i, ] <- unname(coef(modelfit_6))
}

#Preparing data to export
PseudoData_6$eq <-"eq_6"
PseudoData_6$dbh_6 <- PseudoData_6$dbh_6 / 10  #convertion to cm
head(PseudoData_6)

#Mean
aPiceaRu<-mean(coefficients_6$a)
bPiceaRu<-mean(coefficients_6$b)
cPiceaRu<-mean(coefficients_6$c)


#any(is.na(datatofit)) #NA revision in the data

#SD
SDaPiceaRu<-sd(coefficients_6$a) #standar deviation intercept
SDbPiceaRu<-sd(coefficients_6$b)
SDcPiceaRu<-sd(coefficients_6$c)

#Percentile
#50th percentile
QaPiceaRu<-quantile(coefficients_6$a, probs = 0.5)
QbPiceaRu<-quantile(coefficients_6$b, probs = 0.5)
QcPiceaRu<-quantile(coefficients_6$c, probs = 0.5)


#hist(coefficients9_1$a)
#
hist(coefficients_6$a,
     main = "coefficient `a` Picea Rubens",
     xlab = "Coefficient a",
     ylab = "Frequency",
     col = "skyblue",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)          
abline(v = 6.018, col = "blue", lwd = 2, lty = 2)
#

hist(coefficients_6$b,
     main = "Coefficient `b` Picea Rubens",
     xlab = "Coefficient b",
     ylab = "Frequency",
     col = "green",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1) 
abline(v = -0.282, col = "blue", lwd = 2, lty = 2)
#
hist(coefficients_6$c,
     main = "Coefficient `c` Picea Rubens",
     xlab = "Coefficient c",
     ylab = "Frequency",
     col = "red",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)     
abline(v = 0.005, col = "black", lwd = 2, lty = 2)
#

#
plot(a~c,data=coefficients_6)
#
range(coefficients_6$a)

#View(coefficients9_1)


# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients_6) <- c("aPiceaRu", "bPiceaRu","cPiceaRu")
# Adding the correlative 
coefficients_6$correlative <- seq_len(nrow(coefficients_6))

write.csv(coefficients_6, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/New_pseudodata/Coefficients/coefficients_6.csv", 
          row.names = FALSE)


# Specifies the full path to save the file

write.csv(PseudoData_6, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/Biomass_Year/PiceaRu.csv", 
          row.names = FALSE)


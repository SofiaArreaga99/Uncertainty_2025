#Fraxinus Americana

# White Ash
#Monteith 1979 (New York), Range: 2.5 - 55 cm, Whole tree (above stump)

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

#First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH
# Specie: White Ash
set.seed(223)


# Equation 1 Monteith 1979 (New York)
# Units DBH mm and biomass kg 

rsqp_7<-0.983 ##Published R^2 value 
minDBH_7<-2.5*10 #From Jenkin´s *10 conversion to mm
maxDBH_7<-55*10 #From Jenkin´s *10 conversion to mm
a_7<- 3.203 
b_7<- -0.234
c_7<- 0.006 
d_7<- 2

##CREATE 10,000 matrix

test_7 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) #Old form 
test_7 <- pmin(pmax(test_7, -3), 3)  # truncar a ±3σ ->This works

##Create Random DBHs over the range sampled, dbh= mm unit

dbh_7 <- minDBH_7 + (maxDBH_7 - minDBH_7) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = a+b*dia+c*(dia^d)

meany_7 <-a_7 + b_7*(dbh_7) + c_7*((dbh_7)^2)


#View(meany9_1)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys_7 <- matrix(rep(meany_7, times = 1000), nrow = length(meany_7), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomases. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
#stdevs9_1 <- seq(0.1, 100, length.out=1000)  #works better
stdevs_7 <- seq(0.05, 1, length.out=1000)
stdevs2_7 <- matrix(rep(stdevs_7, each = 10000), nrow = 10000, ncol = length(stdevs_7))  
dbh2_7 <- matrix(rep(dbh_7, times = 1000), nrow = length(dbh_7), ncol = 1000)

#psuedys9_1 <- ys9_1 + stdevs2_9_1 * test9_1  # this makes the new biomases if no heteroscedasticity #

psuedys_7 <- ys_7 + stdevs2_7 * test_7 * dbh2_7 # This makes the new biomasses if heteroscedasticity

#this makes the new biomasses with heteroscedasticity #

rsq2_7 <- numeric(1000)  # memory allocation is all, speeds up


for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst_7 <- sum((psuedys_7[, i] - mean(psuedys_7[, i]))^2)  # Total sum of squares
  sse_7 <- sum((psuedys_7[, i] - meany_7)^2)  # Sum of squares of the error 
  rsq2_7[i] <- 1 - (sse_7 / sst_7)  # Coefficient of determination R²
}

## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop

diffs_7 <- abs(rsq2_7 - rsqp_7)
I_7 <- which.min(diffs_7)  # Find the index of the minimum value
BM_7<- psuedys_7[, I_7]  # Select corresponding column

I_7
diffs_7[160]
rsq2_7[160]

## Create figure for checking if result is reasonable ##

plot((dbh_7/10), BM_7, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Fraxinus americana")

# Write the data in an Excel file

PseudoData_7 <- data.frame(dbh_7, BM_7)

# iteration 

noiter <- 10000
coefficients_7 <- data.frame(a = rep(NA, noiter), b = rep(NA, noiter), c = rep(NA, noiter))


for (i in 1:noiter) {
  
  datatofit_7 <- sample_n(PseudoData_7, 32, replace = FALSE)
  modelfit_7 <- lm(BM_7 ~ dbh_7 + I((dbh_7)^2), data = na.omit(datatofit_7))
  coefficients_7[i, ] <- unname(coef(modelfit_7))
}

#Preparing data to export
PseudoData_7$eq <-"eq_7"
PseudoData_7$dbh_7 <- PseudoData_7$dbh_7 / 10  #convertion to cm
head(PseudoData_7)

#Mean
aFraxinusA<-mean(coefficients_7$a)
bFraxinusA<-mean(coefficients_7$b)
cFraxinusA<-mean(coefficients_7$c)


#any(is.na(datatofit)) #NA revision in the data

#SD
SDaFraxinusA<-sd(coefficients_7$a) #standar deviation intercept
SDbFraxinusA<-sd(coefficients_7$b)
SDcFraxinusA<-sd(coefficients_7$c)

#Percentile
#50th percentile
QaFraxinusA<-quantile(coefficients_7$a, probs = 0.5)
QbFraxinusA<-quantile(coefficients_7$b, probs = 0.5)
QcFraxinusA<-quantile(coefficients_7$c, probs = 0.5)


#hist(coefficients9_1$a)
#
hist(coefficients_7$a,
     main = "coefficient `a` Fraxinus americana",
     xlab = "Coefficient a",
     ylab = "Frequency",
     col = "skyblue",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)   
abline(v = 3.203, col = "blue", lwd = 2, lty = 2)

#

hist(coefficients_7$b,
     main = "Coefficient `b` Fraxinus americana",
     xlab = "Coefficient b",
     ylab = "Frequency",
     col = "green",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)          
abline(v = -0.234, col = "red", lwd = 2, lty = 2)
#
hist(coefficients_7$c,
     main = "Coefficient `c` Fraxinus americana",
     xlab = "Coefficient c",
     ylab = "Frequency",
     col = "red",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)   
abline(v = 0.006, col = "black", lwd = 2, lty = 2)
#

#
plot(a~c,data=coefficients_7)
#
range(coefficients_7$a)

#View(coefficients9_1)


# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients_7) <- c("aFraxinusA", "bFraxinusA","cFraxinusA")
# Adding the correlative 
coefficients_7$correlative <- seq_len(nrow(coefficients_7))

write.csv(coefficients_7, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/New_pseudodata/Coefficients/coefficients_7.csv", 
          row.names = FALSE)
head(coefficients_7)

# Specifies the full path to save the file

write.csv(PseudoData_7, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/Biomass_Year/FraxinusA.csv", 
          row.names = FALSE)

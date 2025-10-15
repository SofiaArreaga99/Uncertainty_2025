# American Beech Fagus grandifolia 
#Monteith 1979 (New York), Range: 2.5 - 55 cm, Whole tree (above stump)

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

#First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH
# Specie: American Beech
set.seed(123)

# Equation 1 Monteith 1979 (New York)
# Units DBH mm and biomass kg 

rsqp_3<-0.95 ##Published R^2 value 
minDBH_3<-2.5*10 #From Jenkin´s *10 conversion to mm
maxDBH_3<-55*10 #From Jenkin´s *10 conversion to mm
a_3<- 5.337 
b_3<- -0.326
c_3<- 0.007 
d_3<- 2

##CREATE 10,000 matrix

test_3 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) #Old form 
test_3 <- pmin(pmax(test_3, -3), 3)  # truncar a ±3σ ->This works

##Create Random DBHs over the range sampled, dbh= mm unit

dbh_3 <- minDBH_3 + (maxDBH_3 - minDBH_3) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = a+b*dia+c*(dia^d)

meany_3 <-a_3 + b_3*(dbh_3) + c_3*((dbh_3)^2)


#View(meany9_1)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys_3 <- matrix(rep(meany_3, times = 1000), nrow = length(meany_3), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomases. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
#stdevs9_1 <- seq(0.1, 100, length.out=1000)  #works better
stdevs_3 <- seq(0.05, 1, length.out=1000)
stdevs2_3 <- matrix(rep(stdevs_3, each = 10000), nrow = 10000, ncol = length(stdevs_3))  
dbh2_3 <- matrix(rep(dbh_3, times = 1000), nrow = length(dbh_3), ncol = 1000)

#psuedys9_1 <- ys9_1 + stdevs2_9_1 * test9_1  # this makes the new biomases if no heteroscedasticity #

psuedys_3 <- ys_3 + stdevs2_3 * test_3 * dbh2_3 # This makes the new biomasses if heteroscedasticity

#this makes the new biomasses with heteroscedasticity #

rsq2_3 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst_3 <- sum((psuedys_3[, i] - mean(psuedys_3[, i]))^2)  # Total sum of squares
  sse_3 <- sum((psuedys_3[, i] - meany_3)^2)  # Sum of squares of the error 
  rsq2_3[i] <- 1 - (sse_3 / sst_3)  # Coefficient of determination R²
}

## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop

diffs_3 <- abs(rsq2_3 - rsqp_3)
I_3 <- which.min(diffs_3)  # Find the index of the minimum value
BM_3<- psuedys_3[, I_3]  # Select corresponding column

I_3
diffs_3[385]
rsq2_3[385]

## Create figure for checking if result is reasonable ##

plot((dbh_3/10), BM_3, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Fagus grandifolia")

# Write the data in an Excel file

PseudoData3 <- data.frame(dbh_3, BM_3)

# iteration 

noiter <- 10000
coefficients_3 <- data.frame(a = rep(NA, noiter), b = rep(NA, noiter), c = rep(NA, noiter))


for (i in 1:noiter) {
  
  datatofit_3 <- sample_n(PseudoData3, 32, replace = FALSE)
  modelfit_3 <- lm(BM_3 ~ dbh_3 + I((dbh_3)^2), data = na.omit(datatofit_3))
  coefficients_3[i, ] <- unname(coef(modelfit_3))
}

#Preparing data to export
PseudoData3$eq <-"eq_3"
PseudoData3$dbh_3 <- PseudoData3$dbh_3 / 10  #convertion to cm
head(PseudoData3)

#Mean
aFagusG<-mean(coefficients_3$a)
bFagusG<-mean(coefficients_3$b)
cFagusG<-mean(coefficients_3$c)


#any(is.na(datatofit)) #NA revision in the data

#SD
SDaFagusG<-sd(coefficients_3$a) #standar deviation intercept
SDbFagusG<-sd(coefficients_3$b)
SDcFagusG<-sd(coefficients_3$c)

#Percentile
#50th percentile
QaFagusG<-quantile(coefficients_3$a, probs = 0.5)
QbFagusG<-quantile(coefficients_3$b, probs = 0.5)
QcFagusG<-quantile(coefficients_3$c, probs = 0.5)


#hist(coefficients9_1$a)
#
hist(coefficients_3$a,
     main = "coefficient `a` Fagus grandifolia",
     xlab = "Coefficient a",
     ylab = "Frequency",
     col = "skyblue",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)    
# Add vertical line 
abline(v = 5.337, col = "red", lwd = 2, lty = 2)

#

hist(coefficients_3$b,
     main = "Coefficient `b` Fagus grandifolia",
     xlab = "Coefficient b",
     ylab = "Frequency",
     col = "green",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)    
# Add vertical line 
  abline(v = -0.326, col = "blue", lwd = 2, lty = 2)
#
hist(coefficients_3$c,
     main = "Coefficient `c` Fagus grandifolia",
     xlab = "Coefficient c",
     ylab = "Frequency",
     col = "red",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)     
# Add vertical line 
abline(v = 0.007, col = "blue", lwd = 2, lty = 2)
#

#
plot(a~c,data=coefficients_3)
#
range(coefficients_3$a)

#View(coefficients9_1)


# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients_3) <- c("aFagusG", "bFagusG","cFagusG")
# Adding the correlative 
coefficients_3$correlative <- seq_len(nrow(coefficients_3))


write.csv(coefficients_3, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/New_pseudodata/Coefficients/coefficients_3.csv", 
          row.names = FALSE)
head(coefficients_3)
# Specifies the full path to save the file

write.csv(PseudoData3, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/Biomass_Year/FagusG.csv", 
          row.names = FALSE)



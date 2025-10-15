# Yellow Birch Betula alleghaniensis

#Monteith 1979 (New York), Range: 2.5 - 55 cm, Whole tree (above stump)

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

#First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH
# Specie: Yellow Birch


# Equation 1 Monteith 1979 (New York)
# Units DBH mm and biomass kg 
set.seed(124)

rsqp_2<-0.979 ##Published R^2 value 
minDBH_2<-2.5*10 #From Jenkin´s *10 conversion to mm
maxDBH_2<-55*10 #From Jenkin´s *10 conversion to mm
a_2<- 9.370 
b_2<- -0.449
c_2<- 0.007 
d_2<- 2

##CREATE 10,000 matrix

test_2 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) #Old form 
#test9_1 <- matrix(rnorm(10000 * 1000, mean = 0, sd = 0.1), nrow = 10000, ncol = 1000) # one form 
#test9_1 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) / 10 # Other form 
test_2 <- pmin(pmax(test_2, -2), 2)  # truncar a ±3σ ->This works

##Create Random DBHs over the range sampled, dbh= mm unit

dbh_2 <- minDBH_2 + (maxDBH_2 - minDBH_2) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = a+b*dia+c*(dia^d)

meany_2 <-a_2 + b_2*(dbh_2) + c_2*((dbh_2)^d_2)


#View(meany9_1)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys_2 <- matrix(rep(meany_2, times = 1000), nrow = length(meany_2), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomases. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
#stdevs9_1 <- seq(0.1, 100, length.out=1000)  #works better
stdevs_2 <- seq(0.05, 1, length.out=1000)
stdevs2_2 <- matrix(rep(stdevs_2, each = 10000), nrow = 10000, ncol = length(stdevs_2))  
dbh2_2 <- matrix(rep(dbh_2, times = 1000), nrow = length(dbh_2), ncol = 1000)

#psuedys9_1 <- ys9_1 + stdevs2_9_1 * test9_1  # this makes the new biomases if no heteroscedasticity #

psuedys_2 <- ys_2 + stdevs2_2 * test_2 * dbh2_2 # This makes the new biomasses if heteroscedasticity

#this makes the new biomasses with heteroscedasticity #

rsq2_2 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst_2 <- sum((psuedys_2[, i] - mean(psuedys_2[, i]))^2)  # Total sum of squares
  sse_2 <- sum((psuedys_2[, i] - meany_2)^2)  # Sum of squares of the error 
  rsq2_2[i] <- 1 - (sse_2 / sst_2)  # Coefficient of determination R²
}

## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop

diffs_2 <- abs(rsq2_2 - rsqp_2)
I_2 <- which.min(diffs_2)  # Find the index of the minimum value
BM_2<- psuedys_2[, I_2]  # Select corresponding column

I_2
diffs_2[226]
rsq2_2[226]
## Create figure for checking if result is reasonable ##

plot((dbh_2/10), BM_2, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Betula alleghaniensis")

# Write the data in an Excel file

PseudoData2 <- data.frame(dbh_2, BM_2)

# iteration 

noiter <- 10000
coefficients_2 <- data.frame(a = rep(NA, noiter), b = rep(NA, noiter), c = rep(NA, noiter))


for (i in 1:noiter) {
  
  datatofit_2 <- sample_n(PseudoData2, 31, replace = FALSE)
  modelfit_2 <- lm(BM_2 ~ dbh_2 + I((dbh_2)^2), data = na.omit(datatofit_2))
  coefficients_2[i, ] <- unname(coef(modelfit_2))
}

#Preparing data to export
PseudoData2$eq <-"eq_2"
PseudoData2$dbh_2 <- PseudoData2$dbh_2 / 10  #convertion to cm
head(PseudoData2)
# Muestra las primeras filas
#head(coefficients9_1)
#head(datatofit_1)

#Mean
aBetulaA<-mean(coefficients_2$a)
bBetulaA<-mean(coefficients_2$b)
cBetulaA<-mean(coefficients_2$c)


#any(is.na(datatofit)) #NA revision in the data

#SD
SDaBetulaA<-sd(coefficients_2$a) #standar deviation intercept
SDbBetulaA<-sd(coefficients_2$b)
SDcBetulaA<-sd(coefficients_2$c)

#Percentile
#50th percentile
QaBetulaA<-quantile(coefficients_2$a, probs = 0.5)
QbBetulaA<-quantile(coefficients_2$b, probs = 0.5)
QcBetulaA<-quantile(coefficients_2$c, probs = 0.5)


#hist(coefficients9_1$a)
#
hist(coefficients_2$a,
     main = "coefficient `a` Betula alleghaniensis",
     xlab = "Coefficient a",
     ylab = "Frequency",
     col = "skyblue",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)          # tamaño de números en ejes
# Add vertical line
abline(v = 9.370, col = "red", lwd = 2, lty = 2)
#

hist(coefficients_2$b,
     main = "Coefficient `b` Betula alleghaniensis",
     xlab = "Coefficient b",
     ylab = "Frequency",
     col = "green",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)   
# Add vertical line
abline(v = -0.449, col = "red", lwd = 2, lty = 2)
#
hist(coefficients_2$c,
     main = "Coefficient `c` Betula alleghaniensis",
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
plot(a~c,data=coefficients_2)
#
range(coefficients_2$a)

#View(coefficients9_1)


# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients_2) <- c("aBetulaA", "bBetulaA","cBetulaA")
# Adding the correlative 
coefficients_2$correlative <- seq_len(nrow(coefficients_2))
head(coefficients_2)

write.csv(coefficients_2, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/New_pseudodata/Coefficients/coefficients_2.csv", 
          row.names = FALSE)

head(coefficients_2)
# Specifies the full path to save the file
#write.csv(PseudoDataTsugaCa1, file = "TsugaCa.csv1", row.names = FALSE)
write.csv(PseudoData2, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/Biomass_Year/BetulaA.csv", 
          row.names = FALSE)


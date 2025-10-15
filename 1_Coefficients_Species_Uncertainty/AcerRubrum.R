# Hacer rubrum
#Monteith 1979 (New York), Range: 2.5 - 55 cm, Whole tree (above stump)

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

#First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH
# Specie: Red maple
set.seed(125)

# Equation 1 Monteith 1979 (New York)
# Units DBH mm and biomass kg 

rsqp_5<-0.971 ##Published R^2 value 
minDBH_5<-2.5*10 #From Jenkin´s *10 conversion to mm
maxDBH_5<-55*10 #From Jenkin´s *10 conversion to mm
a_5<- 6.115 
b_5<- -0.360
c_5<- 0.006 
d_5<- 2

##CREATE 10,000 matrix

test_5 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) #Old form 
test_5 <- pmin(pmax(test_5, -3), 3)  # truncar a ±3σ ->This works

##Create Random DBHs over the range sampled, dbh= mm unit

dbh_5 <- minDBH_5 + (maxDBH_5 - minDBH_5) * runif(10000, min = 0, max = 1)


## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = a+b*dia+c*(dia^d)

meany_5 <-a_5 + b_5*(dbh_5) + c_5*((dbh_5)^2)


#View(meany9_1)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys_5 <- matrix(rep(meany_5, times = 1000), nrow = length(meany_5), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomases. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
#stdevs9_1 <- seq(0.1, 100, length.out=1000)  #works better
stdevs_5 <- seq(0.05, 1, length.out=1000)
stdevs2_5 <- matrix(rep(stdevs_5, each = 10000), nrow = 10000, ncol = length(stdevs_5))  
dbh2_5 <- matrix(rep(dbh_5, times = 1000), nrow = length(dbh_5), ncol = 1000)

#psuedys9_1 <- ys9_1 + stdevs2_9_1 * test9_1  # this makes the new biomases if no heteroscedasticity #

psuedys_5 <- ys_5 + stdevs2_5 * test_5 * dbh2_5 # This makes the new biomasses if heteroscedasticity

#this makes the new biomasses with heteroscedasticity #

rsq2_5 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst_5 <- sum((psuedys_5[, i] - mean(psuedys_5[, i]))^2)  # Total sum of squares
  sse_5 <- sum((psuedys_5[, i] - meany_5)^2)  # Sum of squares of the error 
  rsq2_5[i] <- 1 - (sse_5 / sst_5)  # Coefficient of determination R²
}

## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop

diffs_5 <- abs(rsq2_5 - rsqp_5)
I_5 <- which.min(diffs_5)  # Find the index of the minimum value
BM_5<- psuedys_5[, I_5]  # Select corresponding column

I_5
diffs_5[217]
rsq2_5[217]

## Create figure for checking if result is reasonable ##

plot((dbh_5/10), BM_5, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Hacer Rubrum")

# Write the data in an Excel file

PseudoData_5 <- data.frame(dbh_5, BM_5)

# iteration 

noiter <- 10000
coefficients_5 <- data.frame(a = rep(NA, noiter), b = rep(NA, noiter), c = rep(NA, noiter))


for (i in 1:noiter) {
  
  datatofit_5 <- sample_n(PseudoData_5, 33, replace = FALSE)
  modelfit_5 <- lm(BM_5 ~ dbh_5 + I((dbh_5)^2), data = na.omit(datatofit_5))
  coefficients_5[i, ] <- unname(coef(modelfit_5))
}

#Preparing data to export
PseudoData_5$eq <-"eq_5"
PseudoData_5$dbh_5 <- PseudoData_5$dbh_5 / 10  #convertion to cm
head(PseudoData_5)

#Mean
aHacerRu<-mean(coefficients_5$a)
bHacerRu<-mean(coefficients_5$b)
cHacerRu<-mean(coefficients_5$c)


#any(is.na(datatofit)) #NA revision in the data

#SD
SDaHacerRu<-sd(coefficients_5$a) #standar deviation intercept
SDbHacerRu<-sd(coefficients_5$b)
SDcHacerRu<-sd(coefficients_5$c)

#Percentile
#50th percentile
QaHacerRu<-quantile(coefficients_5$a, probs = 0.5)
QbHacerRu<-quantile(coefficients_5$b, probs = 0.5)
QcHacerRu<-quantile(coefficients_5$c, probs = 0.5)


#hist(coefficients9_1$a)
#
hist(coefficients_5$a,
     main = "coefficient `a` Hacer Rubrum",
     xlab = "Coefficient a",
     ylab = "Frequency",
     col = "skyblue",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)          
abline(v = 6.115, col = "red", lwd = 2, lty = 2)
#

hist(coefficients_5$b,
     main = "Coefficient `b` Hacer Rubrum",
     xlab = "Coefficient b",
     ylab = "Frequency",
     col = "green",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)   
abline(v = -0.360, col = "blue", lwd = 2, lty = 2)
#
hist(coefficients_5$c,
     main = "Coefficient `c` Hacer Rubrum",
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
plot(a~c,data=coefficients_5)
#
range(coefficients_5$a)

#View(coefficients9_1)


# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients_5) <- c("aHacerRu", "bHacerRu","cHacerRu")
# Adding the correlative 
coefficients_5$correlative <- seq_len(nrow(coefficients_5))
#head(coefficients_5)
write.csv(coefficients_5, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/New_pseudodata/Coefficients/coefficients_5.csv", 
          row.names = FALSE)

# Specifies the full path to save the file

write.csv(PseudoData_5, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/Biomass_Year/HacerRu.csv", 
          row.names = FALSE)


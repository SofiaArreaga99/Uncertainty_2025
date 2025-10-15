# Tsuga Canadensis 

#Monteith 1979 (New York), Range: 2.5 - 55 cm, Whole tree (above stump)

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

#First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH
# Specie: Tsuga canadensis
set.seed(123)

# Equation 1 Monteith 1979 (New York)
# Units DBH mm and biomass kg 

rsqp4<-0.987 ##Published R^2 value 
minDBH4<-2.5*10 #From Jenkin´s *10 conversion to mm
maxDBH4<-55*10 #From Jenkin´s *10 conversion to mm
a_4<- 6.137 
b_4<- -0.278  
c_4<- 0.004
d_4<- 2

##CREATE 10,000 matrix

test4 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) #Old form 
#test9_4 <- matrix(rnorm(10000 * 1000, mean = 0, sd = 0.1), nrow = 10000, ncol = 1000) # one form 
#test9_4 <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000) / 10 # Other form 
test4 <- pmin(pmax(test4, -3), 3)  # truncar a ±3σ ->This works

##Create Random DBHs over the range sampled, dbh= mm unit

dbhTsugaCa_4 <- minDBH4 + (maxDBH4 - minDBH4) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS

## calculate the biomass using the published equation form 
## biomass(kg) = a+b*dia+c*(dia^d)

meany4 <-a_4 + b_4*(dbhTsugaCa_4) + c_4*((dbhTsugaCa_4)^d_4)


#View(meany9_4)
## Introduce Random Error into calculated biomass ##
## this next part fuzzies the biomasses to create 1000 different populations ## 

ys4 <- matrix(rep(meany4, times = 1000), nrow = length(meany4), ncol = 1000)

# creating the steps used to generate wider ranges of fuzzed biomases. This progression
# will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
# as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
#stdevs9_4 <- seq(0.1, 100, length.out=1000)  #works better
stdevs4 <- seq(0.05, 1, length.out=1000)
stdevs2_4 <- matrix(rep(stdevs4, each = 10000), nrow = 10000, ncol = length(stdevs4))  
dbh2_4 <- matrix(rep(dbhTsugaCa_4, times = 1000), nrow = length(dbhTsugaCa_4), ncol = 1000)

#psuedys9_4 <- ys9_4 + stdevs2_9_4 * test4  # this makes the new biomases if no heteroscedasticity #

psuedys4 <- ys4 + stdevs2_4 * test4 * dbh2_4 # This makes the new biomasses if heteroscedasticity

#this makes the new biomasses with heteroscedasticity #

rsq2_4 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst4 <- sum((psuedys4[, i] - mean(psuedys4[, i]))^2)  # Total sum of squares
  sse4 <- sum((psuedys4[, i] - meany4)^2)  # Sum of squares of the error 
  rsq2_4[i] <- 1 - (sse4 / sst4)  # Coefficient of determination R²
}

## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop

diffs4 <- abs(rsq2_4 - rsqp4)
I4 <- which.min(diffs4)  # Find the index of the minimum value
BMTsugaCa_4<- psuedys4[, I4]  # Select corresponding column

I4
diffs4[65]
rsq2_4[65]

## Create figure for checking if result is reasonable ##

plot((dbhTsugaCa_4/10), BMTsugaCa_4, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")

# Write the data in an Excel file

PseudoDataTsugaCa1 <- data.frame(dbhTsugaCa_4, BMTsugaCa_4)
#PseudoDataTsugaCa1 <- subset(PseudoDataTsugaCa1, BMTsugaCa_4>1)
#head(PseudoDataTsugaCa1)
#head(PseudoDataTsugaCa1)
## Logaritmic differences #meany9_4 <-a_9_4 + b_9_4*(dbhTsugaCa_4) + c_9_4*((dbhTsugaCa_4)^d_9_4)


noiter <- 10000
coefficients4 <- data.frame(a = rep(NA, noiter), b = rep(NA, noiter), c = rep(NA, noiter))


for (i in 1:noiter) {
  
 datatofit_4 <- sample_n(PseudoDataTsugaCa1, 33, replace = FALSE)
 modelfit_4 <- lm(BMTsugaCa_4 ~ dbhTsugaCa_4 + I((dbhTsugaCa_4)^2), data = na.omit(datatofit_4))
 coefficients4[i, ] <- unname(coef(modelfit_4))
}

View(coefficients4)

#Preparing data to export
PseudoDataTsugaCa1$eq <-"eq4_tsuga"
PseudoDataTsugaCa1$dbhTsugaCa_4 <- PseudoDataTsugaCa1$dbhTsugaCa_4 / 10  #convertion to cm
head(PseudoDataTsugaCa1)
# Muestra las primeras filas
#head(coefficients9_4)
#head(datatofit_4)

#Mean
aTsuga<-mean(coefficients4$a)
bTsuga<-mean(coefficients4$b)
cTsuga<-mean(coefficients4$c)

#any(is.na(datatofit)) #NA revision in the data

#SD
SDa<-sd(coefficients4$a) #standar deviation intercept
SDb<-sd(coefficients4$b)
SDc<-sd(coefficients4$c)

#Percentile
#50th percentile
QaTsuga<-quantile(coefficients4$a, probs = 0.5)
QbTsuga<-quantile(coefficients4$b, probs = 0.5)
QcTsuga<-quantile(coefficients4$c, probs = 0.5)


#hist(coefficients9_4$a)
#
hist(coefficients4$a,
     main = "coefficient `a` Tsuga",
     xlab = "Coefficient a",
     ylab = "Frequency",
     col = "skyblue",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)          # tamaño de números en ejes
# Add vertical line 
abline(v = 6.137, col = "blue", lwd = 2, lty = 2)
#

hist(coefficients4$b,
     main = "Coefficient `b` Tsuga",
     xlab = "Coefficient b",
     ylab = "Frequency",
     col = "green",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1) 
# Add vertical line 
abline(v = -0.278, col = "blue", lwd = 2, lty = 2)
#
hist(coefficients4$c,
     main = "Coefficient `c` Tsuga",
     xlab = "Coefficient c",
     ylab = "Frequency",
     col = "red",       
     border = "white",      
     breaks = 30,           
     cex.main = 1.5,        
     cex.lab = 1.2,         
     cex.axis = 1)       
# Add vertical line 
abline(v = 0.004, col = "blue", lwd = 2, lty = 2)
#

#
plot(a~c,data=coefficients4)
#
range(coefficients4$a)

#View(coefficients9_4)


# Name columns for clarity            Nombrar columnas para claridad
colnames(coefficients4) <- c("aTsuga", "bTsuga","cTsuga")
# Adding the correlative 
coefficients4$correlative <- seq_len(nrow(coefficients4))

write.csv(coefficients4, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/New_pseudodata/Coefficients/coefficients_4.csv", 
          row.names = FALSE)




#View(PseudoDataTsugaCa1)
#plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa, pch = 16, xlab = "DBH (mm)", ylab = "Biomass (kg)", main = "Tsuga Canadiensis")
#plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa,pch = 16, col = rgb(0.2, 0.4, 0.6, 0.5), azul semi-transparente, xlab = "DBH (mm)", ylab = "Biomass (kg)", main = "Tsuga canadensis",cex = 1.2)  # tamaño de los puntos
#grid()  # agrega rejilla

# Specifies the full path to save the file
#write.csv(PseudoDataTsugaCa1, file = "TsugaCa.csv1", row.names = FALSE)
write.csv(PseudoDataTsugaCa1, 
          file = "C:/Users/vanco/Desktop/ResearchR/Sim_Research/Code/Biomass_Year/TsugaCa.csv", 
          row.names = FALSE)



# Real Data #
Tsuga_Monteith <- data.frame(
  location = "New York",
  source   = "Monteith 1979",
  dbh      = seq(from = 2.5, to = 55, by = 2.5),
  kg       = c(2, 3, 9, 21, 38, 61, 89, 122, 160, 204,
               254, 308, 368, 434, 504, 581, 662, 749,
               841, 938, 1041, 1150)
)

Tsuga_Monteith

# pseudo data
plot(PseudoDataTsugaCa1$dbhTsugaCa, PseudoDataTsugaCa1$BMTsugaCa,
     pch = 16,
     col = rgb(0.2, 0.4, 0.6, 0.5),   # azul semi-transparente
     xlab = "DBH (cm)", 
     ylab = "Biomass (kg)", 
     main = "Tsuga canadensis",
     cex = 1.2)
grid()

# Monteith 1979
points(Tsuga_Monteith$dbh, Tsuga_Monteith$kg,
       pch = 17,   # triángulos
       col = rgb(0.8, 0.2, 0.2, 0.7), # rojo semi-transparente
       cex = 1.3)

legend("topleft",
       legend = c("Pseudo data", "Monteith 1979"),
       col = c(rgb(0.2,0.4,0.6,0.5), rgb(0.8,0.2,0.2,0.7)),
       pch = c(16, 17),
       bty = "n")




# Just testing the fit of the data #

set.seed(123)
dbh_test <- runif(1000, 25, 550) #  mm
biomasa_test <- 6.1371 - 0.2785*dbh_test + 0.004286*(dbh_test^2)

# fit
fit_test <- lm(biomasa_test ~ dbh_test + I(dbh_test^2))
coef(fit_test)

#It works 

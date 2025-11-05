# Merge of the coefficients with 10,000 simulations B0, B1 per specie 

#CFI1970


#Packages 
library(readxl)
library(plyr)
library(doBy)
library(dplyr)
library(knitr)
library(ggplot2)
library(RColorBrewer)
library(readr)


coefficients1 <- read.csv("1_Coefficients_Species_Uncertainty/coefficients_1.csv")
coefficients2 <- read.csv("1_Coefficients_Species_Uncertainty/coefficients_2.csv")
coefficients3 <- read.csv("1_Coefficients_Species_Uncertainty/coefficients_3.csv")
coefficients4 <- read.csv("1_Coefficients_Species_Uncertainty/coefficients_4.csv")
coefficients5 <- read.csv("1_Coefficients_Species_Uncertainty/coefficients_5.csv")
coefficients6 <- read.csv("1_Coefficients_Species_Uncertainty/coefficients_6.csv")
coefficients7 <- read.csv("1_Coefficients_Species_Uncertainty/coefficients_7.csv")


lista_MEGAMerge <- list(coefficients1, coefficients2, coefficients3, coefficients4, coefficients5, coefficients6, coefficients7)
View(lista_MEGAMerge)


# 2. Merge all of the elements, because they are more than 2 I need to use Reduce function
#Which is merging my elements by correlative that is just a number. 
library(dplyr)
library(purrr)

colnames(coefficients7)

MergeCov <- lista_MEGAMerge %>%
  reduce(full_join, by = "correlative")

View(MergeCov)


str(MergeCov)

# 1.     I wrote the excel but this is not neccesary technically 

write.csv(MergeCov, file = "4_General_Resources/MergeCov_2.csv", row.names = FALSE)


#RM01 Coursework - Testing whether the housing wealth effect is different in different regions

library(lmtest)
library(xlsx)
library(ggplot2)

setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

#load and prepare data
df1 <- read.xlsx('RM01_OptionA_data.xlsx', sheetIndex = 1)
df1 <- subset(df1, select = c(SCOTLAND, LONDON, FUELANNUAL, HSVAL, HSOWND,
                              HHINCOME, SAVINGSINCOME, HSBEDS, HHSIZE,
                              NKIDS_DV, HEATCH, FUELDUEL, SAVE, AGE, FINFUT,
                              ENVTEND))
df1 <- df1[complete.cases(df1),]
#in this subset there are 2335 observations

#LONDON

#run existing model without interaction term or dummy variable
mod7 <-  lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
            + HSBEDS + HHSIZE + NKIDS_DV + HEATCH + FUELDUEL
            + SAVE + AGE + FINFUT + ENVTEND, data = df1)

#run full model including interaction terms
mod8 <- lm(FUELANNUAL ~ LONDON * (HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
           + HSBEDS + HHSIZE + NKIDS_DV + HEATCH + FUELDUEL
           + SAVE + AGE + FINFUT + ENVTEND), data = df1)

#run a partial F test to see if the dummy term and its interactions are
#jointly significant
Fstat <- ((sum(mod7$residuals^2) - sum(mod8$residuals^2))/14)/(sum(mod8$residuals^2)/(2335-27-1))
pvalLondon <- 1- pf(Fstat,14,1448-27-1)
#also assess the models with an anova test
anova(mod7,mod8)

#summary of mod8 allows for investigation of the interaction terms
summary(mod8)

#now we do the same for SCOTLAND
mod9 <- lm(FUELANNUAL ~ SCOTLAND * (HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
                        + HSBEDS + HHSIZE + NKIDS_DV + HEATCH + FUELDUEL
                        + SAVE + AGE + FINFUT + ENVTEND), data = df1)
#run a partial F test to see if the dummy term and its interactions are
#jointly significant

Fstat <- ((sum(mod7$residuals^2) - sum(mod9$residuals^2))/14)/(sum(mod9$residuals^2)/(2335-27-1))
pvalScotland <- 1- pf(Fstat,14,1448-27-1)

#again compare the models using an anova test
anova(mod7, mod9)
#summary of mod9 allows for detailed investigation of individual interaction terms
summary(mod9)

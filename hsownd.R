#RM01 Coursework - Testing whether the housing wealth effect is
#altered by housing ownership


library(lmtest)
library(xlsx)
library(ggplot2)

setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

#load and prepare data
df1 <- read.xlsx('RM01_OptionA_data.xlsx', sheetIndex = 1)
df1 <- subset(df1, select = c(FUELANNUAL, HSVAL, HSOWND,
                              HHINCOME, SAVINGSINCOME, HSBEDS, HHSIZE,
                              NKIDS_DV, HEATCH, FUELDUEL, SAVE, AGE, FINFUT,
                              ENVTEND))
df1 <- df1[complete.cases(df1),]
#in this subset there are 2335 observations

#run existing model without interaction term or dummy variable
mod7 <-  lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
            + HSBEDS + HHSIZE + NKIDS_DV + HEATCH + FUELDUEL
            + SAVE + AGE + FINFUT + ENVTEND, data = df1)

#now we run the model with interaction terms between HSOWND and the other terms
mod8 <-  lm(FUELANNUAL ~ HSOWND * (HSVAL + log(HHINCOME) + SAVINGSINCOME
            + HSBEDS + HHSIZE + NKIDS_DV + HEATCH + FUELDUEL
            + SAVE + AGE + FINFUT + ENVTEND), data = df1)

#run a partial F test to see if the dummy term and its interactions are
#jointly significant
Fstat <- ((sum(mod7$residuals^2) - sum(mod8$residuals^2))/12)/(sum(mod8$residuals^2)/(2335-27-1))
pval <- 1- pf(Fstat,12,1448-27-1)
#and run an ANOVA test
anova(mod7,mod8)
#summary of mod8
summary(mod8)

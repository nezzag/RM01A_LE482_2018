#RM01 Testing for Heteroskedasticity within the models

library(lmtest)
library(xlsx)
install.packages('sandwich')
library(sandwich)
library(broom)


setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

#load the data
df1 <- read.xlsx('RM01_OptionA_data.xlsx', sheetIndex = 1)
df1 <- subset(df1, select = -c(NA.))
df2 <- subset(df1, select = c(FUELANNUAL, HSVAL, HSOWND, HHINCOME, SAVINGSINCOME,
                              HSBEDS, HHSIZE, NKIDS_DV, NEMP_DV, NPENS_DV, HEATCH, FUELDUEL,
                              SAVE, AGE, FINFUT, EDU, ENVTEND))

df2 <- df2[complete.cases(df2),]

#the linear model determined by manual variable selection
mod4 <- lm(FUELANNUAL ~ HSVAL + HSOWND + HHINCOME + SAVINGSINCOME
           + HSBEDS + HHSIZE + NKIDS_DV + NEMP_DV + NPENS_DV
           + HEATCH + FUELDUEL
           + SAVE + AGE + FINFUT + EDU + ENVTEND, data = df2)
summary(mod4)

#the logarithmic model which improves upon it
mod6 <- lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
           + HSBEDS + HHSIZE + NKIDS_DV +  NEMP_DV + NPENS_DV
           + HEATCH + FUELDUEL
           + SAVE + AGE + FINFUT + EDU + ENVTEND, data = df2)
summary(mod6)

#the logarithmic model which has been determined by stepwise variable selection
mod7 <- lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
           + HSBEDS + HHSIZE + NKIDS_DV + HEATCH + FUELDUEL
           + SAVE + AGE + FINFUT + ENVTEND, data = df2)
summary(mod7)


#we now test for heteroskedasticity in the discrete variables, and store the resultant p-values
#in a dataframe
pvalue1 <- c(gqtest(mod4, order.by = df2$HSVAL)$p.value,
             gqtest(mod4, order.by = df2$HHINCOME)$p.value,
             gqtest(mod4, order.by = df2$SAVINGSINCOME)$p.value,
             gqtest(mod4, order.by = df2$HSBEDS)$p.value,
             gqtest(mod4, order.by = df2$HHSIZE)$p.value,
             gqtest(mod4, order.by = df2$NKIDS_DV)$p.value,
             gqtest(mod4, order.by = df2$NEMP_DV)$p.value,
             gqtest(mod4, order.by = df2$NPENS_DV)$p.value) 

pvalue2 <- c(gqtest(mod6, order.by = df2$HSVAL)$p.value,
             gqtest(mod6, order.by = df2$HHINCOME)$p.value,
             gqtest(mod6, order.by = df2$SAVINGSINCOME)$p.value,
             gqtest(mod6, order.by = df2$HSBEDS)$p.value,
             gqtest(mod6, order.by = df2$HHSIZE)$p.value,
             gqtest(mod6, order.by = df2$NKIDS_DV)$p.value,
             gqtest(mod6, order.by = df2$NEMP_DV)$p.value,
             gqtest(mod6, order.by = df2$NPENS_DV)$p.value)

pvalue3 <- c(gqtest(mod7, order.by = df2$HSVAL)$p.value,
             gqtest(mod7, order.by = df2$HHINCOME)$p.value,
             gqtest(mod7, order.by = df2$SAVINGSINCOME)$p.value,
             gqtest(mod7, order.by = df2$HSBEDS)$p.value,
             gqtest(mod7, order.by = df2$HHSIZE)$p.value,
             gqtest(mod7, order.by = df2$NKIDS_DV)$p.value, NA, NA)

variable <- c('HSVAL', 'HHINCOME','SAVINGSINCOME','HSBEDS','HHSIZE','NKIDS','NEMP', 'NPENS')
results <- data.frame(variable, pvalue1, pvalue2, pvalue3)

#perform heteroskedastic robust coefficient estimates for the model, and store them

write.csv(tidy(coeftest(mod1, vcov = sandwich)), 'mod4HSR.csv')
write.csv(tidy(coeftest(mod2, vcov = sandwich)), 'mod6HSR.csv')
write.csv(tidy(coeftest(mod7, vcov = sandwich)), 'mod7HSR.csv')

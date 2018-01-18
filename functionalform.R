#RM01 Structural Stability Tests to determine variable selection

library(lmtest)
library(xlsx)
library(car)
library(ggplot2)
library(MASS)
library(broom)

setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

#load and prepare data
df1 <- read.xlsx('RM01_OptionA_data.xlsx', sheetIndex = 1)
df1 <- subset(df1, select = -c(NA.))


#generate the linear model we have been using so far
mod4 <- lm(FUELANNUAL ~ HSVAL + HSOWND + HHINCOME + SAVINGSINCOME
           + HSBEDS + HHSIZE + NKIDS_DV + NEMP_DV + NPENS_DV
           + HEATCH + FUELDUEL
           + SAVE + AGE + FINFUT + EDU + ENVTEND, data = df1)
summary(mod4)
write.csv(tidy(mod4),'mod4.csv')
#test whether nonlinear terms need to be added using the RESET test
reset(mod4, 2:3)

#use a model with polynomial terms in Household income
HHINCOME2 = df1$HHINCOME^2
HHINCOME3 = df1$HHINCOME^3
mod5 <- lm(FUELANNUAL ~ HSVAL + HSOWND + HHINCOME + HHINCOME2 + HHINCOME3 + SAVINGSINCOME
           + HSBEDS + HHSIZE + NKIDS_DV + NEMP_DV + NPENS_DV
           + HEATCH + FUELDUEL
           + SAVE + AGE + FINFUT + EDU + ENVTEND, data = df1)
summary(mod5)
write.csv(tidy(mod5),'mod5.csv')

#Testing this new model with a partial F test
Fstat <- ((sum(mod4$residuals^2)-sum(mod5$residuals^2))/2)/(sum(mod5$residuals^2)/(1474-20-1))
1 - pf(Fstat,2,1453)

#store data on the consumption coefficients
a <- summary(mod5)$coefficients[4]
b <- summary(mod5)$coefficients[5]
c <- summary(mod5)$coefficients[6]


#we also specify and estimate a logarithmic consumption function
mod6 <- lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
           + HSBEDS + HHSIZE + NKIDS_DV + NEMP_DV + NPENS_DV
           + HEATCH + FUELDUEL
           + SAVE + AGE + FINFUT + EDU + ENVTEND, data = df1)
summary(mod6)
write.csv(tidy(mod6),'mod6.csv')

#saving data on the consumption function
d <- summary(mod6)$coefficients[4]

#the two consumption functions that have been estimated here are then plotted below
HHINCOME <- seq(0, 7e4, 10)
dfplot <- data.frame(HHINCOME)
dfplot$HHINCOME2 <- dfplot$HHINCOME^2
dfplot$HHINCOME3 <- dfplot$HHINCOME^3

ggplot() +
  geom_line(data = dfplot, mapping=aes(HHINCOME,a*HHINCOME + b*HHINCOME2 + c*HHINCOME3), size = 0.1, colour = 'red') +
  geom_line(data = dfplot, mapping=aes(HHINCOME,d*log(HHINCOME)), size = 0.1, colour = 'blue') +
  ggtitle('Consumption Function: Dependance on Household Income') +
  xlab(expression(paste("Household Income / £ month"^"-1"))) +
  ylab(expression(paste("Annual Fuel Consumption / £ year"^"-1"))) +
  scale_colour_manual(values=c("red","blue"))

#taking the logarithmic functional form, we do a stepwise variable selection
#with the reduced variables to determine the final
df2 <- df1[complete.cases(df1),]
mod7 <- lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
           + HSBEDS + HHSIZE + NKIDS_DV + NEMP_DV + NPENS_DV
           + HEATCH + FUELDUEL
           + SAVE + AGE + FINFUT + EDU + ENVTEND, data = df3)
step(mod7, scope=mod7, direction = "both")

#this gives this model as the final model
mod7 <- lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
           + HSBEDS + HHSIZE + NKIDS_DV
           + HEATCH + FUELDUEL
           + SAVE + AGE + FINFUT + ENVTEND, data = df3)
#save model summary results
write.csv(tidy(mod7), 'mod7.csv')

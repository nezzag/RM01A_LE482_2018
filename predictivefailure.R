#Testing each of the three models under evaluation, mod4, mod6 and mod7, with the predictive failure test

library(lmtest)
library(xlsx)
library(ggplot2)

setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

#load and prepare data
df1 <- read.xlsx('RM01_OptionA_data.xlsx', sheetIndex = 1)
df1 <- subset(df1, select = -c(NA.,PERSONALINCOME, SEGROUP1, SEGROUP2, SEGROUP3, SEGROUP4))

#as the test randomly removes values, we run it 100 times to calculate the average result
#first we do this for mod4
pvalmod4 = integer(100)
for(k in 1:100){
  #first we do a regression with all 1471 data points
  df2 <- df1[complete.cases(df1),]
  
  mod4 <-  lm(FUELANNUAL ~ HSVAL + HSOWND + HHINCOME + SAVINGSINCOME
              + HSBEDS + HHSIZE + NKIDS_DV + NEMP_DV + NPENS_DV
              + HEATCH + FUELDUEL + EDU
              + SAVE + AGE + FINFUT + ENVTEND, data = df2)
  
  #then we remove some data points
  for (i in 1:150){
    j = sample(1:length(df2$HSVAL),1)
    df2 <- df2[-c(j),]
  }
  
  #and repeat the model, now with less datapoints
  mod4reduced <- lm(FUELANNUAL ~ HSVAL + HSOWND + HHINCOME + SAVINGSINCOME
                    + HSBEDS + HHSIZE + NKIDS_DV + NEMP_DV + NPENS_DV
                    + HEATCH + FUELDUEL + EDU
                    + SAVE + AGE + FINFUT + ENVTEND, data = df2)
  
  Fstat <- ((sum(mod4$residuals^2)-sum(mod4reduced$residuals^2))/150)/(sum(mod4reduced$residuals^2)/(1321-13-1))
  pvalmod4[k] =  pf(Fstat,150,1421-13-1, lower.tail=F)
}
pvalmod4 <- mean(pvalmod4)


#then for mod6
pvalmod6 <- integer(100)
for(k in 1:100){
  #first we do a regression with all 1471 data points
  df2 <- df1[complete.cases(df1),]
  
  mod6 <-  lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
              + HSBEDS + HHSIZE + NKIDS_DV + NEMP_DV + NPENS_DV + HEATCH + FUELDUEL
              + SAVE + AGE + EDU + FINFUT + ENVTEND, data = df2)
  
  #then we remove some data points
  for (i in 1:150){
    j = sample(1:length(df2$HSVAL),1)
    df2 <- df2[-c(j),]
  }
  
  #and repeat the model, now with less datapoints
  mod6reduced <- lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
                    + HSBEDS + HHSIZE + NKIDS_DV + NEMP_DV + NPENS_DV + HEATCH + FUELDUEL
                    + SAVE + AGE + EDU + FINFUT + ENVTEND, data = df2)
  
  Fstat <- ((sum(mod6$residuals^2)-sum(mod6reduced$residuals^2))/150)/(sum(mod6reduced$residuals^2)/(1321-13-1))
  pvalmod6[k] =  pf(Fstat,150,1421-13-1, lower.tail=F)
}
pvalmod6 <- mean(pvalmod6)

#and for mod7
pvalmod7 <- integer(100)
for(k in 1:100){
  #first we do a regression with all 1471 data points
  df2 <- df1[complete.cases(df1),]
  
  mod7 <-  lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
              + HSBEDS + HHSIZE + NKIDS_DV + HEATCH + FUELDUEL
              + SAVE + AGE + FINFUT + ENVTEND, data = df2)
  
  #then we remove some data points
  for (i in 1:150){
    j = sample(1:length(df2$HSVAL),1)
    df2 <- df2[-c(j),]
  }
  
  #and repeat the model, now with less datapoints
  mod7reduced <- lm(FUELANNUAL ~ HSVAL + HSOWND + log(HHINCOME) + SAVINGSINCOME
                    + HSBEDS + HHSIZE + NKIDS_DV + HEATCH + FUELDUEL
                    + SAVE + AGE + FINFUT + ENVTEND, data = df2)
  
  Fstat <- ((sum(mod7$residuals^2)-sum(mod7reduced$residuals^2))/150)/(sum(mod7reduced$residuals^2)/(1321-13-1))
  pvalmod7[k] =  pf(Fstat,150,1421-13-1, lower.tail=F)
}
pvalmod7 <- mean(pvalmod7)

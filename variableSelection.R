#RM01 Variable selection

library(lmtest)
library(xlsx)
library(car)
library(broom)

setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

#load and prepare data
df1 <- read.xlsx('RM01_OptionA_data.xlsx', sheetIndex = 1)
df1 <- subset(df1, select = -c(NA., SCOTLAND, LONDON, SEGROUP4))
df2 <- df1[complete.cases(df2),]

#number of observations
n = 1451

#full model, containing all variables
full <- lm(FUELANNUAL ~ . - FUELANNUAL, data = df2)
SSEfull <- sum(full$residuals^2)
adjRfull <- summary(full)$adj.r.squared

#this model has multicollinearity in the HHSIZE and subset of variables
#NUE_DV is removed to correct for this,creating mod1
vif(full)[4:8]
mod1 <- lm(FUELANNUAL ~ . - FUELANNUAL
           - NUE_DV, data = df2)
SSE1 <- sum(mod1$residuals^2)
adjR1 <- summary(mod1)$adj.r.squared


Fstat1 <- ((SSE1-SSEfull))/(SSEfull/(n-24-1))
critstat1 <- qf(0.9,2,n-24-1)

#in mod2, we remove the most insignificant variables, and
#validate this by seeing the change in the adjusted R-squared. We also
#check that these are not jointly significant using a partial F test
mod2 <- lm(FUELANNUAL ~ . - FUELANNUAL
           - EFFICIENT - FINNOW - NUE_DV, data = df2)
adjR2 <- summary(mod2)$adj.r.squared

Fstat2 <- ((sum(mod2$residuals^2)-sum(mod1$residuals^2))/2)/(sum(mod1$residuals^2)/n-23-1)
critstat2 <- qf(0.9,1,n-22-1)

#next we test for appliance use variables in mod3
mod3 <- mod3 <- lm(FUELANNUAL ~ . - FUELANNUAL
                   - EFFICIENT - FINNOW
                   - NUE_DV
                   - MOBUSE - INTFREQ, data = df2)
adjR3 <- summary(mod3)$adj.r.squared

Fstat3 <- ((sum(mod3$residuals^2)-sum(mod2$residuals^2))/2)/(sum(mod2$residuals^2)/(n-21-1))
critstat3 <- qf(0.9,2,n-21-1)
#these are seen to be insignificant and can be removed

#now we test for socioeconomic dummy variables
mod4 <- lm(FUELANNUAL ~ . - FUELANNUAL
           - EFFICIENT - FINNOW
           - NUE_DV
           - MOBUSE - INTFREQ
           - SEGROUP1 - SEGROUP2 - SEGROUP3, data = df2)
adjR4 <- summary(mod4)$adj.r.squared

Fstat4 <- ((sum(mod4$residuals^2)-sum(mod3$residuals^2))/3)/(sum(mod3$residuals^2)/(n-19-1))
critstat4 <- qf(0.9,3,n-20-1)
#these are seen to be insignificant and can be removed

#saving the results
model <- c('full','mod1','mod2','mod3','mod4')
Rsquare <- c(adjRfull, adjR1, adjR2, adjR3, adjR4)
Fstat <- c(NA, Fstat1, Fstat2, Fstat3, Fstat4)
critstat <- c(NA, critstat1, critstat2, critstat3, critstat4)
results <- data.frame(model,Rsquare,Fstat,critstat)
View(results)

write.xlsx(results,'variableSelection.xlsx')
write.csv(tidy(full), 'full.csv')
write.csv(tidy(mod4),'mod4.csv')
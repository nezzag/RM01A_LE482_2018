#Investigating the determinants of the ENVTEND variable

library(xlsx)
library(lmtest)


setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

#load and prepare data
df1 <- read.xlsx('RM01_Envtend_data.xlsx', sheetIndex = 1)
df1 <- subset(df1, select = -c(NA.))
#removing the variables that have too many missing values to be of any use
df2 <- subset(df1, select = -c(CARSHARE, COMTRAIN, COMBIKE, COMWALK, COMBUS))
df2 <- df2[complete.cases(df2),]

#ENVTEND PROBIT model
mod1 <- glm(ENVTEND ~ AGE + HSVAL + EFFICIENT +
              MOBUSE + INTFREQ + EDU +
              FINNOW + FINFUT + SAVE + ENVTENDCONTROL +
              SEGROUP1 + SEGROUP2 + SEGROUP3 +
              PERSONALINCOME + SAVINGSINCOME,
              family = binomial(link = "logit"),
              data = df2)
summary(mod1)

step(mod1, scope = mod1, direction = "both")

mod2 <- glm(ENVTEND ~ AGE + EFFICIENT + FINNOW + SAVE + ENVTENDCONTROL,
            family = binomial(link = "logit"),
            data = df2)
summary(mod2)
write.csv(tidy(mod2),'envtend.csv')

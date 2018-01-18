#RM01 Preliminary Data Analysis
setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

library(xlsx)
library(psych)
library(dplyr)

df1 <- read.xlsx('RM01_OptionA_data.xlsx', sheetIndex = 1)
df1 <- subset(df1, select = -c(NA.))


#Testing for multicollinearity between any of the explanatory variables
for (i in 2:length(df1)){
  for (j in 2:length(df1)){
    if (i != j &
      cor(df1[,i],df1[,j], use = "complete.obs") > 0.8){
      print(c('Multicollinearity detected between', names(df1[i]), 'and', names(df1[j])))
    }
  }
}

#Producing summary statistics for those variables which are not category variables
#first we need to produce a subset of them
discretevals <- c('HSBEDS','HHSIZE',
                  'NKIDS_DV','NPENS_DV', 'NEMP_DV', 'NUE_DV',
                  'AGE','HHINCOME','SAVINGSINCOME',
                  'HSVAL','FUELANNUAL','PERSONALINCOME')
df2 <- df1[discretevals]
#then we calculate and store the descriptive statistics
df3 <- describe(df2)
write.xlsx(df3, 'descriptive_statistics1UPDATED.xlsx')

#Producing summary statistics for category (dummy) variables
df4 <- subset(df1, select = - c(HSBEDS, HHSIZE, NKIDS_DV, NPENS_DV,
                                NEMP_DV, NUE_DV, AGE, HHINCOME, SAVINGSINCOME,
                                HSVAL, FUELANNUAL, PERSONALINCOME))

df5 <- data.frame(names(df4), integer(length(df4)))
df5 <- select(df5, DUMMYVARIABLE = names.df4., PERCENTAGESAMPLE = integer.length.df4..)
for(i in 2:length(df4)){
  df5[i,2] = sum(df4[,i], na.rm = T)/length(df4[,i][!is.na(df4[,i])])*100
}
write.xlsx(df5, 'descriptive_statistics2UPDATED.xlsx')


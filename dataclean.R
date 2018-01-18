#RM01 Data Cleaning and Processing
setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

library(readxl)
library(xlsx)
library(dplyr)
library(MASS)
library(ggplot2)

rawdata <- read_excel("RM01 Option A course work data 2017-2018.xlsx", sheet = 1, range = "A2:CM4536")

#replace all '.' with NA, which is the syntax R will use for missing values
df1 <- rawdata
df1[df1 == "."] <- NA

#create a subset with the maximum number of variables of interest
varsmax <- c('YEAR', 'HHSIZE','HSBEDS','HSOWND','HSVAL',
          'FUELDUEL','HEATCH', 'NKIDS_DV',
          'HHEAT','HHTYPE_DV', 'NPENS_DV','NEMP_DV','NUE_DV',
          'GOV_DV','FIHHMNGRS_DV','JBSTAT',
          'MOBUSE','NETPUSE','QFHIGH','JBSEC',
          'FINNOW','FINFUT','SAVE','ENVHABIT1_A',
          'ENVHABIT4_A','BIRTHY','MRJSEG_DV',
          'FIYRDIC_DV','FUELANNUAL','PAYGU_DV')
df2 <- df1[varsmax]

#We now need to create the relevant dummy variables for the system

#LONDON
df2$LONDON <- rep(NA, length(df2$HSVAL))
for (i in 1:length(df2$LONDON)){
  if ((is.na(df2$GOV_DV[i])==F) &
      df2$GOV_DV[i] == 7) {
    df2$LONDON[i] = 1
  }
}
for (i in 1:length(df2$LONDON)){
  if ((is.na(df2$GOV_DV[i])==F) &
      df2$GOV_DV[i] != 7) {
    df2$LONDON[i] = 0
  }
}

#SCOTLAND
df2$SCOTLAND <- rep(NA, length(df2$HSVAL))
for (i in 1:length(df2$SCOTLAND)){
  if ((is.na(df2$GOV_DV[i])==F) &
      df2$GOV_DV[i] == 11) {
    df2$SCOTLAND[i] = 1
  }
}
for (i in 1:length(df2$SCOTLAND)){
  if ((is.na(df2$GOV_DV[i])==F) &
      df2$GOV_DV[i] != 11) {
    df2$SCOTLAND[i] = 0
  }
}


#NETPUSE - replace with a binary variable
#1 represents frequent (daily) use, 0 infrequent (less than daily) use
for (i in 1:length(df2$NETPUSE)){
  if(df2$NETPUSE[i] != 1 &
     (is.na(df2$NETPUSE[i]) == F)){
    df2$NETPUSE[i] = 0}
}

#MOBUSE
#1 represents having a mobile, 0 represents not
for (i in 1:length(df2$MOBUSE)){
  if((is.na(df2$MOBUSE[i]) == F) &
     df2$MOBUSE[i] == 2){
    df2$MOBUSE[i] = 0}
}

#ENVHABIT1_A and ENVHABIT4_A. These show the environmental behaviour, with a
#low score indicating poor environmental tendencies for ENVHABIT1_A
#and a high score indicating poor environmental tendencies for ENVHABIT4_A
#A new variable ENVTEND is created. 
#1 represents strong environmental tendencies
#0 represents a lack of/poor environmental tendencies
df2$ENVTEND <- integer(length(df2$ENVHABIT1_A))
df2$ENVHABIT1_A <- as.numeric(as.character(df2$ENVHABIT1_A))
df2$ENVHABIT4_A <- as.numeric(as.character(df2$ENVHABIT4_A))
for (i in 1:length(df2$ENVHABIT1_A)){
  if((is.na(df2$ENVHABIT1_A[i]) == F) &
     (is.na(df2$ENVHABIT4_A[i]) == F) &
    (df2$ENVHABIT1_A[i]-df2$ENVHABIT4_A[i]) >= 0){
    df2$ENVTEND[i] = 1
  }
}
for (i in 1:length(df2$ENVTEND)){
  if((is.na(df2$ENVHABIT1_A[i]) == T) |
     (is.na(df2$ENVHABIT4_A[i]) == T)){
    df2$ENVTEND[i] = NA
  }
}

#SOCIO-ECONOMIC STATUS
#We create four dummy variables, that categorise people in 4 different socio-economic groups
#SEGROUP1 - managerial/professional
#SEGROUP2 - superivsory/non-manual/skilled manual workers
#SEGROUP3 - unskilled workers/agricultural workers/own account workers
#SEGROUP4 - students/unemployed/pensioners
df2$SEGROUP1 <- rep(NA, length(df2$HSVAL))
df2$SEGROUP2 <- rep(NA, length(df2$HSVAL))
df2$SEGROUP3 <- rep(NA, length(df2$HSVAL))
df2$SEGROUP4 <- rep(NA, length(df2$HSVAL))
for (i in 1:length(df2$MRJSEG_DV)){
  if ((is.na(df2$MRJSEG_DV[i]) == F) &
      (df2$MRJSEG_DV[i] == 1 |
      df2$MRJSEG_DV[i] == 2 |
      df2$MRJSEG_DV[i] == 3 |
      df2$MRJSEG_DV[i] == 4 |
      df2$MRJSEG_DV[i] == 5 |
      df2$MRJSEG_DV[i] == 6 |
      df2$MRJSEG_DV[i] == 16)){
    df2$SEGROUP1[i] = 1
    df2$SEGROUP2[i] = 0
    df2$SEGROUP3[i] = 0
    df2$SEGROUP4[i] = 0
  }
}

for (i in 1:length(df2$MRJSEG_DV)){
  if ((is.na(df2$MRJSEG_DV[i]) == F) &
      (df2$MRJSEG_DV[i] == 7 |
      df2$MRJSEG_DV[i] == 8 |
      df2$MRJSEG_DV[i] == 9 |
      df2$MRJSEG_DV[i] == 10 |
      df2$MRJSEG_DV[i] == 11 |
      df2$MRJSEG_DV[i] == 12 |
      df2$MRJSEG_DV[i] == 19)){
    df2$SEGROUP1[i] = 0
    df2$SEGROUP2[i] = 1
    df2$SEGROUP3[i] = 0
    df2$SEGROUP4[i] = 0
  }
}

for (i in 1:length(df2$MRJSEG_DV)){
  if ((is.na(df2$MRJSEG_DV[i]) == F) &
      (df2$MRJSEG_DV[i] == 13 |
       df2$MRJSEG_DV[i] == 14 |
       df2$MRJSEG_DV[i] == 15 |
       df2$MRJSEG_DV[i] == 17 |
       df2$MRJSEG_DV[i] == 18 )){
    df2$SEGROUP1[i] = 0
    df2$SEGROUP2[i] = 0
    df2$SEGROUP3[i] = 1
    df2$SEGROUP4[i] = 0
    }
}

for (i in 1:length(df2$JBSTAT)){
  if ((is.na(df2$JBSTAT[i]) == F) &
    (df2$JBSTAT[i] == 3 |
      df2$JBSTAT[i] == 4 |
      df2$JBSTAT[i] == 6 |
      df2$JBSTAT[i] == 7 |
      df2$JBSTAT[i] == 8)){
    df2$SEGROUP4[i] = 1
    df2$SEGROUP1[i] = 0
    df2$SEGROUP2[i] = 0
    df2$SEGROUP3[i] = 0
  }
}

#EDUCATION LEVEL
#Made into a binary system, where 1 indicates further education (16+) or more
#and 0 indicates education up to age 16 or less
df2$EDU <- rep(NA, nrow(df2))
for (i in 1:length(df2$EDU)){
  if((is.na(df2$QFHIGH[i]) == F) &
     (df2$QFHIGH[i] > 11)){
    df2$EDU[i] = 0
  }
}
for(i in 1:length(df2$EDU)){
  if((is.na(df2$QFHIGH[i]) == F) &
     (df2$QFHIGH[i] < 12)){
    df2$EDU[i] = 1
  }
}
#FUEL TOGETHER OR SEPARATE
#1 indicates together, 0 indicates separate
for (i in 1:length(df2$FUELDUEL)){
  if ((is.na(df2$FUELDUEL[i]) == F) &
    df2$FUELDUEL[i] == 2){
    df2$FUELDUEL[i] = 0
  }
}

#CENTRAL HEATING OR NOT
#1 indicates yes, 0 indicates no
for (i in 1:length(df2$HEATCH)){
  if ((is.na(df2$HEATCH[i]) == F) &
    df2$HEATCH[i] == 2){
    df2$HEATCH[i] = 0
  }
}

#WELL INSULATED HOUSE OR NOT
#Here we use the ability of homeowners to keep their home warm in winter
#as a proxy for energy efficiency.
#1 indicates adequate/good energy efficiency, 0 poor energy efficiency
for (i in 1:length(df2$HHEAT)){
  if ((is.na(df2$HHEAT[i]) == F) &
    df2$HHEAT[i] == 2){
    df2$HHEAT[i] = 0
  }
}

#CURRENT FINANCIAL SITUATION
#1 indicates good current financial situation
#0 indicates adequate/poor current financial sitauation
for (i in 1:length(df2$FINNOW)){
  if ((is.na(df2$FINNOW[i]) ==F) &
    df2$FINNOW[i] == 2 ){
    df2$FINNOW[i] = 1
  }
}
for (i in 1:length(df2$FINNOW)){
  if ((is.na(df2$FINNOW[i]) ==F) & 
      (df2$FINNOW[i] == 3 |
       df2$FINNOW[i] == 4 |
       df2$FINNOW[i] == 5 )){
    df2$FINNOW[i] = 0
  }
}

#FUTURE EXPECTATIONS
#Here we use FINFUT to determine whether someone expects to struggle
#financially in the future. A 1 indicates good future expectations, 
#a 0 represents adequate or poor future expectations
for (i in 1:length(df2$FINFUT)){
  if ((is.na(df2$FINFUT[i]) ==F) &
      (df2$FINFUT[i] == 1 |
       df2$FINFUT[i] == 2 )){
    df2$FINFUT[i] = 1
  }
}

for (i in 1:length(df2$FINFUT)){
  if ((is.na(df2$FINFUT[i]) ==F) &
    (df2$FINFUT[i] == 3 |
     df2$FINFUT[i] == 4 |
     df2$FINFUT[i] == 5)){
    df2$FINFUT[i] = 0
  }
}

#JBSEC - as another measure of future expectations
for (i in 1:length(df2$JBSEC)){
  if ((is.na(df2$JBSEC[i]) == F) &
      (df2$JBSEC[i] == 1 |
      df2$JBSEC[i] == 2)){
    df2$JBSEC[i] = 0
  }
}
for (i in 1:length(df2$JBSEC)){
  if ((is.na(df2$JBSEC[i]) == F) &
      (df2$JBSEC[i] == 3 |
      df2$JBSEC[i] == 4)){
    df2$JBSEC[i] = 1
  }
}

#SAVING
#1 represents a saver, 0 represents not a saver
for (i in 1:length(df2$SAVE)){
  if ((is.na(df2$SAVE[i]) ==F) &
      df2$SAVE[i] == 2){
    df2$SAVE[i] = 0
  }
}

#OWNERSHIP TYPE
#We want to compare those who own their house outright, to those who are 
#paying a mortgage/under shared ownership.
#A 1 indicates owning the house outright
for (i in 1:length(df2$HSOWND)){
  if ((is.na(df2$HSOWND[i]) ==F) &
     (df2$HSOWND[i] == 2 |
      df2$HSOWND[i] == 3)){
    df2$HSOWND[i] = 0
  }
}
#we also want to remove all datapoints with HSOWND = 4,5 or 97 - as these
#don't have any value in HSVAL
remrow = c()
for (i in 1:length(df2$HSVAL)){
  if ((is.na(df2$HSOWND[i]) == F) &
      (df2$HSOWND[i] == 4 |
       df2$HSOWND[i] == 5 |
       df2$HSOWND[i] == 97)){
    remrow <- c(remrow, i)
  }
}
df2 <- df2[-remrow,]

#AGE
df2$AGE <- integer(length(df2$BIRTHY))
df2$YEAR <- as.numeric(as.character(df2$YEAR))
df2$BIRTHY <- as.numeric(as.character(df2$BIRTHY))
for (i in 1:length(df2$BIRTHY)){
  df2$AGE[i] = df2$YEAR[i] - df2$BIRTHY[i]
}

#Extract the final variables that will be used in the regression
varsfin <- c('HSBEDS', 'HEATCH', 'HHEAT','LONDON', 'SCOTLAND',
             'HHSIZE', 'NKIDS_DV','NPENS_DV','NEMP_DV','NUE_DV',
             'AGE','SEGROUP1','SEGROUP2','SEGROUP3','SEGROUP4',
             'HSOWND','FUELDUEL','MOBUSE','NETPUSE',
             'EDU','FINNOW','FINFUT','SAVE', 'ENVTEND',
             'FIHHMNGRS_DV','FIYRDIC_DV','HSVAL','FUELANNUAL')
df3 <- df2[varsfin]

#we want to convert all data to numeric
df4 <- sapply(df3, as.numeric)
df4 <- data.frame(df4)

#and rename some of the columns
df4 <- rename(df4, EFFICIENT = HHEAT, HHINCOME = FIHHMNGRS_DV,
              SAVINGSINCOME = FIYRDIC_DV, INTFREQ = NETPUSE)


#We now test for outliers in the numerical data

#see how well the FUELANNUAL data fits a normal distribution
fitdistr(df4$FUELANNUAL[!is.na(df4$FUELANNUAL)], densfun = 'normal')
ggplot() + 
  geom_density(aes(x=rnorm(10000, mean = 1245, sd = 637),color='Model',fill='Model'),alpha=.4)+geom_density(aes(x=df4$FUELANNUAL,color='Observed',fill='Observed'),alpha=.4)+theme_grey()
#calculate the probability of getting the greatest outlier from this setting
pnorm(max(df4$FUELANNUAL,na.rm=T),mean = 1245,sd=637,lower.tail = F)

#do the same with HHINCOME
fitdistr(df4$HHINCOME[!is.na(df4$HHINCOME)], densfun = 'normal')
ggplot() + 
  geom_density(aes(x=rnorm(10000, mean = 4136, sd = 11913),color='Model',fill='Model'),alpha=.4)+geom_density(aes(x=df4$HHINCOME,color='Observed',fill='Observed'),alpha=.4)+theme_grey()
pnorm(max(df4$HHINCOME,na.rm=T),mean = 4136,sd=11913,lower.tail = F)

#and with SAVINGSINCOME
fitdistr(df4$SAVINGSINCOME[!is.na(df4$SAVINGSINCOME)], densfun = 'normal')
ggplot() + 
  geom_density(aes(x=rnorm(10000, mean = 522, sd = 4762),color='Model',fill='Model'),alpha=.4)+geom_density(aes(x=df4$SAVINGSINCOME,color='Observed',fill='Observed'),alpha=.4)+theme_grey()
pnorm(max(df4$SAVINGSINCOME,na.rm=T),mean=522,sd=4762,lower.tail=F)

#remove the outliers that have been detected
df4 <- df4[!(df4$FUELANNUAL>1e4),]
df4 <- df4[!(df4$HHINCOME>1e5),]
df4 <- df4[!(df4$SAVINGSINCOME>1.4e5),]

#Saving the cleaned and altered file
write.xlsx(df4, 'RM01_OptionA_data.xlsx')
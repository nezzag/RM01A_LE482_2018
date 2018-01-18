#RM01 Data Cleaning and Processing for the Environmental Tendencies Investigation
setwd("C:/Users/Neil/OneDrive/Documents/MPhil Environmental Policy/RM01")

library(readxl)
library(xlsx)
library(dplyr)

rawdata <- read_excel("RM01 Option A course work data 2017-2018.xlsx", sheet = 1, range = "A2:CM4536")

#replace all '.' with NA, which is the syntax R will use for missing values
df1 <- rawdata
df1[df1 == "."] <- NA

#create a subset with the maximum number of variables of interest
varsmax <- c('YEAR', 'BIRTHY','HSVAL','HHEAT','HHTYPE_DV','JBSTAT',
             'MOBUSE','NETPUSE','QFHIGH',
             'FINNOW','FINFUT','SAVE','ENVHABIT1_A',
             'ENVHABIT3_A','ENVHABIT4_A', 'ENVHABIT6_A','ENVHABIT8_A',
             'MRJSEG_DV','CARSHARE',
             'COMBUS','COMTRAIN','COMBIKE','COMWALK',
             'PAYGU_DV','FIYRDIC_DV','FUELANNUAL')
df3 <- df1[varsmax]

#We now need to create the relevant dummy variables for the system
#NETPUSE - replace with a binary variable
#1 represents frequent (daily) use, 0 infrequent (less than daily) use
for (i in 1:length(df3$NETPUSE)){
  if(df3$NETPUSE[i] != 1 &
     (is.na(df3$NETPUSE[i]) == F)){
    df3$NETPUSE[i] = 0}
}

#MOBUSE
#1 represents having a mobile, 0 represents not
for (i in 1:length(df3$MOBUSE)){
  if((is.na(df3$MOBUSE[i]) == F) &
     df3$MOBUSE[i] == 2){
    df3$MOBUSE[i] = 0}
}

#ENVHABIT1_A and ENVHABIT4_A. These show the environmental behaviour, with a
#low score indicating poor environmental tendencies for ENVHABIT1_A
#and a high score indicating poor environmental tendencies for ENVHABIT4_A
#A new variable ENVTEND is created. 
#1 represents strong environmental tendencies
#0 represents a lack of/poor environmental tendencies
df3$ENVTEND <- integer(length(df3$ENVHABIT1_A))
df3$ENVHABIT1_A <- as.numeric(as.character(df3$ENVHABIT1_A))
df3$ENVHABIT4_A <- as.numeric(as.character(df3$ENVHABIT4_A))
for (i in 1:length(df3$ENVHABIT1_A)){
  if((is.na(df3$ENVHABIT1_A[i]) == F) &
     (is.na(df3$ENVHABIT4_A[i]) == F) &
     (df3$ENVHABIT1_A[i]-df3$ENVHABIT4_A[i]) >= 0){
    df3$ENVTEND[i] = 1
  }
}
for (i in 1:length(df3$ENVTEND)){
  if((is.na(df3$ENVHABIT1_A[i]) == T) |
     (is.na(df3$ENVHABIT4_A[i]) == T)){
    df3$ENVTEND[i] = NA
  }
}

#other environmental control variables
df3$ENVTENDCONTROL <- integer(length(df3$ENVHABIT1_A))
df3$ENVHABIT3_A <- as.numeric(as.character(df3$ENVHABIT3_A))
df3$ENVHABIT6_A <- as.numeric(as.character(df3$ENVHABIT6_A))
df3$ENVHABIT8_A <- as.numeric(as.character(df3$ENVHABIT8_A))
for (i in 1:length(df3$ENVHABIT3_A)){
  if((is.na(df3$ENVHABIT3_A[i]) == F) &
     (is.na(df3$ENVHABIT6_A[i]) == F) &
     (is.na(df3$ENVHABIT8_A[i]) == F) &
     (df3$ENVHABIT3_A[i] - df3$ENVHABIT6_A[i]-df3$ENVHABIT8_A[i]) >= 0){
    df3$ENVTENDCONTROL[i] = 1
  }
}
for (i in 1:length(df3$ENVTEND)){
  if((is.na(df3$ENVHABIT3_A[i]) == T) |
     (is.na(df3$ENVHABIT6_A[i]) == T) |
     (is.na(df3$ENVHABIT8_A[i]) == T)){
    df3$ENVTENDCONTROL[i] = NA
  }
}

#SOCIO-ECONOMIC STATUS
#We create four dummy variables, that categorise people in 4 different socio-economic groups
#SEGROUP1 - managerial/professional
#SEGROUP2 - superivsory/non-manual/skilled manual workers
#SEGROUP3 - unskilled workers/agricultural workers/own account workers
#SEGROUP4 - students/unemployed/pensioners
df3$SEGROUP1 <- rep(NA, length(df3$HSVAL))
df3$SEGROUP2 <- rep(NA, length(df3$HSVAL))
df3$SEGROUP3 <- rep(NA, length(df3$HSVAL))
df3$SEGROUP4 <- rep(NA, length(df3$HSVAL))
for (i in 1:length(df3$MRJSEG_DV)){
  if ((is.na(df3$MRJSEG_DV[i]) == F) &
      (df3$MRJSEG_DV[i] == 1 |
       df3$MRJSEG_DV[i] == 2 |
       df3$MRJSEG_DV[i] == 3 |
       df3$MRJSEG_DV[i] == 4 |
       df3$MRJSEG_DV[i] == 5 |
       df3$MRJSEG_DV[i] == 6 |
       df3$MRJSEG_DV[i] == 16)){
    df3$SEGROUP1[i] = 1
    df3$SEGROUP2[i] = 0
    df3$SEGROUP3[i] = 0
    df3$SEGROUP4[i] = 0
  }
}

for (i in 1:length(df3$MRJSEG_DV)){
  if ((is.na(df3$MRJSEG_DV[i]) == F) &
      (df3$MRJSEG_DV[i] == 7 |
       df3$MRJSEG_DV[i] == 8 |
       df3$MRJSEG_DV[i] == 9 |
       df3$MRJSEG_DV[i] == 10 |
       df3$MRJSEG_DV[i] == 11 |
       df3$MRJSEG_DV[i] == 12 |
       df3$MRJSEG_DV[i] == 19)){
    df3$SEGROUP1[i] = 0
    df3$SEGROUP2[i] = 1
    df3$SEGROUP3[i] = 0
    df3$SEGROUP4[i] = 0
  }
}

for (i in 1:length(df3$MRJSEG_DV)){
  if ((is.na(df3$MRJSEG_DV[i]) == F) &
      (df3$MRJSEG_DV[i] == 13 |
       df3$MRJSEG_DV[i] == 14 |
       df3$MRJSEG_DV[i] == 15 |
       df3$MRJSEG_DV[i] == 17 |
       df3$MRJSEG_DV[i] == 18 )){
    df3$SEGROUP1[i] = 0
    df3$SEGROUP2[i] = 0
    df3$SEGROUP3[i] = 1
    df3$SEGROUP4[i] = 0
  }
}

for (i in 1:length(df3$JBSTAT)){
  if ((is.na(df3$JBSTAT[i]) == F) &
      (df3$JBSTAT[i] == 3 |
       df3$JBSTAT[i] == 4 |
       df3$JBSTAT[i] == 6 |
       df3$JBSTAT[i] == 7 |
       df3$JBSTAT[i] == 8)){
    df3$SEGROUP4[i] = 1
    df3$SEGROUP1[i] = 0
    df3$SEGROUP2[i] = 0
    df3$SEGROUP3[i] = 0
  }
}

#EDUCATION LEVEL
#Made into a binary system, where 1 indicates further education (16+) or more
#and 0 indicates education up to age 16 or less
df3$EDU <- rep(NA, nrow(df3))
for (i in 1:length(df3$EDU)){
  if((is.na(df3$QFHIGH[i]) == F) &
     (df3$QFHIGH[i] > 11)){
    df3$EDU[i] = 0
  }
}
for(i in 1:length(df3$EDU)){
  if((is.na(df3$QFHIGH[i]) == F) &
     (df3$QFHIGH[i] < 12)){
    df3$EDU[i] = 1
  }
}
#WELL INSULATED HOUSE OR NOT
#Here we use the ability of homeowners to keep their home warm in winter
#as a proxy for energy efficiency.
#1 indicates adequate/good energy efficiency, 0 poor energy efficiency
for (i in 1:length(df3$HHEAT)){
  if ((is.na(df3$HHEAT[i]) == F) &
      df3$HHEAT[i] == 2){
    df3$HHEAT[i] = 0
  }
}

#CURRENT FINANCIAL SITUATION
#1 indicates good current financial situation
#0 indicates adequate/poor current financial sitauation
for (i in 1:length(df3$FINNOW)){
  if ((is.na(df3$FINNOW[i]) ==F) &
      df3$FINNOW[i] == 2 ){
    df3$FINNOW[i] = 1
  }
}
for (i in 1:length(df3$FINNOW)){
  if ((is.na(df3$FINNOW[i]) ==F) & 
      (df3$FINNOW[i] == 3 |
       df3$FINNOW[i] == 4 |
       df3$FINNOW[i] == 5 )){
    df3$FINNOW[i] = 0
  }
}

#FUTURE EXPECTATIONS
#Here we use FINFUT to determine whether someone expects to struggle
#financially in the future. A 1 indicates good future expectations, 
#a 0 represents adequate or poor future expectations
for (i in 1:length(df3$FINFUT)){
  if ((is.na(df3$FINFUT[i]) ==F) &
      (df3$FINFUT[i] == 1 |
       df3$FINFUT[i] == 2 )){
    df3$FINFUT[i] = 1
  }
}

for (i in 1:length(df3$FINFUT)){
  if ((is.na(df3$FINFUT[i]) ==F) &
      (df3$FINFUT[i] == 3 |
       df3$FINFUT[i] == 4 |
       df3$FINFUT[i] == 5)){
    df3$FINFUT[i] = 0
  }
}

#SAVING
#1 represents a saver, 0 represents not a saver
for (i in 1:length(df3$SAVE)){
  if ((is.na(df3$SAVE[i]) ==F) &
      df3$SAVE[i] == 2){
    df3$SAVE[i] = 0
  }
}

#AGE
df3$AGE <- integer(length(df3$BIRTHY))
df3$YEAR <- as.numeric(as.character(df3$YEAR))
df3$BIRTHY <- as.numeric(as.character(df3$BIRTHY))
for (i in 1:length(df3$BIRTHY)){
  df3$AGE[i] = df3$YEAR[i] - df3$BIRTHY[i]
}

#CARSHARE - is someone willing to carshare? 1 indicates a willingness to carshare
for (i in 1:length(df3$CARSHARE)){
  if ((is.na(df3$CARSHARE[i]) ==F) &
      (df3$CARSHARE[i] == 1 |
       df3$CARSHARE[i] == 2 |
       df3$CARSHARE[i] == 6)){
    df3$CARSHARE[i] = 1
  }
}
for (i in 1:length(df3$CARSHARE)){
  if ((is.na(df3$CARSHARE[i]) ==F) &
      (df3$CARSHARE[i] == 3|
       df3$CARSHARE[i] == 4|
       df3$CARSHARE[i] == 5)){
    df3$CARSHARE[i] = 0
  }
}

#COMMUTING BY WALKING - 1 indicates a willingness to do so
for (i in 1:length(df3$COMWALK)){
  if ((is.na(df3$COMWALK[i]) ==F) &
      (df3$COMWALK[i] == 1 |
       df3$COMWALK[i] == 2 |
       df3$COMWALK[i] == 6)){
    df3$COMWALK[i] = 1
  }
}
for (i in 1:length(df3$COMWALK)){
  if ((is.na(df3$COMWALK[i]) ==F) &
      (df3$COMWALK[i] == 3|
       df3$COMWALK[i] == 4|
       df3$COMWALK[i] == 5)){
    df3$COMWALK[i] = 0
  }
}

#COMMUTING BY BIKE - 1 indicates a willingness to do so
for (i in 1:length(df3$COMBIKE)){
  if ((is.na(df3$COMBIKE[i]) ==F) &
      (df3$COMBIKE[i] == 1 |
       df3$COMBIKE[i] == 2 |
       df3$COMBIKE[i] == 6)){
    df3$COMBIKE[i] = 1
  }
}
for (i in 1:length(df3$COMBIKE)){
  if ((is.na(df3$COMBIKE[i]) ==F) &
      (df3$COMBIKE[i] == 3|
       df3$COMBIKE[i] == 4|
       df3$COMBIKE[i] == 5)){
    df3$COMBIKE[i] = 0
  }
}

#COMMUTING BY BUS - 1 indicates a willingness to do so
for (i in 1:length(df3$COMBUS)){
  if ((is.na(df3$COMBUS[i]) ==F) &
      (df3$COMBUS[i] == 1 |
       df3$COMBUS[i] == 2 |
       df3$COMBUS[i] == 6)){
    df3$COMBUS[i] = 1
  }
}
for (i in 1:length(df3$COMBUS)){
  if ((is.na(df3$COMBUS[i]) ==F) &
      (df3$COMBUS[i] == 3|
       df3$COMBUS[i] == 4|
       df3$COMBUS[i] == 5)){
    df3$COMBUS[i] = 0
  }
}

#COMMUTING BY TRAIN - 1 indicates a willingness to do so
for (i in 1:length(df3$COMBUS)){
  if ((is.na(df3$COMTRAIN[i]) ==F) &
      (df3$COMTRAIN[i] == 1 |
       df3$COMTRAIN[i] == 2 |
       df3$COMTRAIN[i] == 6)){
    df3$COMTRAIN[i] = 1
  }
}
for (i in 1:length(df3$COMTRAIN)){
  if ((is.na(df3$COMTRAIN[i]) ==F) &
      (df3$COMTRAIN[i] == 3|
       df3$COMTRAIN[i] == 4|
       df3$COMTRAIN[i] == 5)){
    df3$COMTRAIN[i] = 0
  }
}


#Extract the final variables that will be used in the regression
varsfin<- c('AGE','HSVAL','HHEAT',
            'MOBUSE','NETPUSE','QFHIGH',
            'FINNOW','FINFUT','SAVE',
            'ENVTEND','ENVTENDCONTROL',
            'CARSHARE','COMBUS','COMTRAIN','COMBIKE','COMWALK',
            'SEGROUP1','SEGROUP2','SEGROUP3','SEGROUP4',
            'PAYGU_DV','FIYRDIC_DV')

df4 <- df3[varsfin]

#we want to convert all data to numeric
df4 <- sapply(df4, as.numeric)
df4 <- data.frame(df4)

#and rename some of the columns
df4 <- rename(df4, EFFICIENT = HHEAT, EDU = QFHIGH,
              SAVINGSINCOME = FIYRDIC_DV, INTFREQ = NETPUSE,
              PERSONALINCOME = PAYGU_DV)

#remove outlier
df4 <- df4[!(df4$SAVINGSINCOME>1.4e6),]

#Saving the cleaned and altered file
write.xlsx(df4, 'RM01_Envtend_data.xlsx')

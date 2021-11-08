rm(list=ls())


library(openxlsx)
library(dplyr)
library(readxl)
library(tidyr)
library(tidyverse)
library(MortalitySmooth)
library(ungroup)
library(DemoDecomp)
library(readr)

defweb04 <- read_delim("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb04.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)


defweb05 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb05.csv")

defweb06 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb06.csv")


defweb08 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb08.csv")

defweb09 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb09.csv")

defweb10 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb10.csv")


defweb12 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb12.csv")

defweb13 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb13.csv")

defweb14 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb14.csv")


defweb17 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb17.csv")

defweb18 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb18.csv")

defweb19 <- read_csv("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/defweb19.csv")




defweb04 <- defweb04 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))

defweb05 <- defweb05 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))


defweb06 <- defweb06 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))


defweb08 <- defweb08 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))


defweb09 <- defweb09 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))


defweb10 <- defweb10 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))


defweb12 <- defweb12 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))


defweb13 <- defweb13 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))


defweb14 <- defweb14 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))



defweb17 <- defweb17 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))


defweb18 <- defweb18 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))


defweb19 <- defweb19 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  filter(SEXO!= 9) %>% 
  summarize(Def=sum(CUENTA))

defweb05 <- rbind(defweb04,defweb05,defweb06)
defweb09 <- rbind(defweb08,defweb09,defweb10)
defweb13 <- rbind(defweb12,defweb13,defweb14)
defweb18 <- rbind(defweb17,defweb18,defweb19)

defweb05 <- defweb05 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  summarize(Def=sum(Def))

defweb09 <- defweb09 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  summarize(Def=sum(Def))


defweb13 <- defweb13 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  summarize(Def=sum(Def))


defweb18 <- defweb18 %>% 
  group_by(SEXO,GRUPEDAD) %>% 
  summarize(Def=sum(Def))


sum(defweb05$Def)
sum(defweb09$Def)
sum(defweb13$Def)
sum(defweb18$Def)


PoblacionArg <- read_excel("C:/Users/obramajo/Desktop/Tesis Bramajo/Defunciones DEIS Argentina 2005 19/PoblacionArgCeladeTotal.xlsx")

PoblacionArg <- PoblacionArg %>% 
  pivot_longer( cols = starts_with("20"),
                 names_to = "Year",
                 values_to = "Population",
                 values_drop_na = TRUE
  )

PoblacionArg$Agenum [PoblacionArg$Agenum > 80] <- 82.5

PoblacionArg <- PoblacionArg %>% 
  filter(Agenum > 15, Year==2004|Year==2005|Year==2006|Year==2008|Year==2009|Year==2010|Year==2012|Year==2013|Year==2014|Year==2017|Year==2018|Year==2019) %>% 
  mutate(Year = case_when(Year==2004 ~ 2005,
                          Year==2005 ~ 2005,
                          Year==2006 ~ 2005,
                          Year==2008 ~ 2009,
                          Year==2009 ~ 2009,
                          Year==2010 ~ 2009,
                          Year==2012 ~ 2013,
                          Year==2013 ~ 2013,
                          Year==2014 ~ 2013,
                          Year==2017 ~ 2018,
                          Year==2018 ~ 2018,
                          Year==2019 ~ 2018)) %>% 
  group_by(Year,Sex,Agenum) %>% 
  summarize(Pop=sum(Population))


new.age <- c(seq(15,80,5))


alldef <- rbind(defweb05,defweb09,defweb13,defweb18)


old.age <- c(0,1,seq(10,80,5),99)

####Correct code here

alldef$Age <- rep(old.age,length.out=144)
alldef <- alldef %>% 
  filter(Age >10 & Age<90)


PoblacionArg$Agenum <- rep(new.age,length.out=112)
PoblacionArg$Def <- alldef$Def

PoblacionArg$DeathRate <- PoblacionArg$Def/PoblacionArg$Pop

#reading ENFRs

ENFR_2005 <- read_delim("C:/Users/obramajo/Desktop/Tesis Bramajo/ENFR Argentina/ENFR-2005 Base Usuario.txt", 
                        "|", escape_double = FALSE, trim_ws = TRUE)

ENFR_2009 <- read_delim("C:/Users/obramajo/Desktop/Tesis Bramajo/ENFR Argentina/ENFR-2009 Base Usuario.txt", 
                        "|", escape_double = FALSE, trim_ws = TRUE)


ENFR_2013 <- read_delim("C:/Users/obramajo/Desktop/Tesis Bramajo/ENFR Argentina/ENFR2013_baseusuario.txt", 
                        "|", escape_double = FALSE, trim_ws = TRUE)

ENFR_2018 <- read_delim("C:/Users/obramajo/Desktop/Tesis Bramajo/ENFR Argentina/ENFR 2018 - Base usuario.txt", 
                        "|", escape_double = FALSE, trim_ws = TRUE)



ENFR_2005 <-ENFR_2005 %>% 
  select(CHCH04,CHCH05,CISG06)

ENFR_2009 <-ENFR_2009 %>% 
select(BHCH04,BHCH05,BISG06)

ENFR_2013 <- ENFR_2013 %>% 
  select(BHCH04,BHCH05,BISG06)
  
ENFR_2018 <- ENFR_2018 %>% 
  select(bhch03,bhch04,bisg06)

colnames(ENFR_2005) <- c("Sex","Age","Dep")
colnames(ENFR_2009) <- c("Sex","Age","Dep")
colnames(ENFR_2013) <- c("Sex","Age","Dep")
colnames(ENFR_2018) <- c("Sex","Age","Dep")


#ENFR_2005$DepRate[ENFR_2005$Dep==3] <- 1 #Depression
#ENFR_2005$DepRate[is.na(ENFR_2005$DepRate)] <- 0 #No Depression

#ENFR_2009$DepRate[ENFR_2009$Dep==3] <- 1 #Depression
#ENFR_2009$DepRate[is.na(ENFR_2009$DepRate)] <- 0 #No Depression

#ENFR_2013$DepRate[ENFR_2013$Dep==3] <- 1 #Depression
#ENFR_2013$DepRate[is.na(ENFR_2013$DepRate)] <- 0 #No Depression

#ENFR_2018$DepRate[ENFR_2018$Dep==3] <- 1 #Depression
#ENFR_2018$DepRate[is.na(ENFR_2018$DepRate)] <- 0 #No Depression



ENFR_2005$DepRate[ENFR_2005$Dep==2|ENFR_2005$Dep==3] <- 1 #Depression
ENFR_2005$DepRate[is.na(ENFR_2005$DepRate)] <- 0 #No Depression

ENFR_2009$DepRate[ENFR_2009$Dep==2|ENFR_2009$Dep==3] <- 1 #Depression
ENFR_2009$DepRate[is.na(ENFR_2009$DepRate)] <- 0 #No Depression

ENFR_2013$DepRate[ENFR_2013$Dep==2|ENFR_2013$Dep==3] <- 1 #Depression
ENFR_2013$DepRate[is.na(ENFR_2013$DepRate)] <- 0 #No Depression

ENFR_2018$DepRate[ENFR_2018$Dep==2|ENFR_2018$Dep==3] <- 1 #Depression
ENFR_2018$DepRate[is.na(ENFR_2018$DepRate)] <- 0 #No Depression



summary(ENFR_2005)
summary(ENFR_2009)
summary(ENFR_2013)
summary(ENFR_2018)

mean1 <- mean(ENFR_2005$Age)
mean2 <- mean(ENFR_2009$Age)
mean3 <- mean(ENFR_2013$Age)
mean4 <- mean(ENFR_2018$Age)


mean5 <- mean(ENFR_2005$Sex)
mean6 <- mean(ENFR_2009$Sex)
mean7 <- mean(ENFR_2013$Sex)
mean8 <- mean(ENFR_2018$Sex)


mean9 <- mean(ENFR_2005$DepRate)
mean10 <- mean(ENFR_2009$DepRate)
mean11 <- mean(ENFR_2013$DepRate)
mean12 <- mean(ENFR_2018$DepRate)


sd1 <- sd(ENFR_2005$Age)
sd2 <- sd(ENFR_2009$Age)
sd3 <- sd(ENFR_2013$Age)
sd4 <- sd(ENFR_2018$Age)


sd5 <- sd(ENFR_2005$Sex)
sd6 <- sd(ENFR_2009$Sex)
sd7 <- sd(ENFR_2013$Sex)
sd8 <- sd(ENFR_2018$Sex)


sd9 <- sd(ENFR_2005$DepRate)
sd10 <- sd(ENFR_2009$DepRate)
sd11 <- sd(ENFR_2013$DepRate)
sd12 <- sd(ENFR_2018$DepRate)


size1 <- length(ENFR_2005$Age)
size2 <- length(ENFR_2009$Age)
size3 <- length(ENFR_2013$Age)
size4 <- length(ENFR_2018$Age)


size5 <- length(ENFR_2005$Sex)
size6 <- length(ENFR_2009$Sex)
size7 <- length(ENFR_2013$Sex)
size8 <- length(ENFR_2018$Sex)

size9 <- length(ENFR_2005$DepRate)
size10 <- length(ENFR_2009$DepRate)
size11 <- length(ENFR_2013$DepRate)
size12 <- length(ENFR_2018$DepRate)

class(size12)

CI95 =function(mean,sd,size){
  error <- qnorm(0.975)*sd/sqrt(size)
  left <- mean-error 
  right <- mean+error
  CI95 <- c(mean,left,right)
  return(CI95)
  }

CI95(mean1,sd1,size1)
CI95(mean2,sd2,size2)
CI95(mean3,sd3,size3)
CI95(mean4,sd4,size4)
CI95(mean5,sd5,size5)
CI95(mean6,sd6,size6)
CI95(mean7,sd7,size7)
CI95(mean8,sd8,size8)
CI95(mean9,sd9,size9)
CI95(mean10,sd10,size10)
CI95(mean11,sd11,size11)
CI95(mean12,sd12,size12)
  


table1 <- table(ENFR_2005$DepRate)
prop.table(table1)
table2 <-table(ENFR_2009$DepRate)
prop.table(table2)
table3 <-table(ENFR_2013$DepRate)
prop.table(table3)
table4 <-table(ENFR_2018$DepRate)
prop.table(table4)



Dep2005<- ENFR_2005 %>% 
  mutate(Age5 = case_when(Age >14 & Age<20 ~ 15,
                      Age >19 & Age<25 ~ 20,
                      Age >24 & Age<30 ~ 25,
                      Age >29 & Age<35 ~ 30,
                      Age >34 & Age<40 ~ 35,
                      Age >39 & Age<45 ~ 40,
                      Age >44 & Age<50 ~ 45,
                      Age >49 & Age<55 ~ 50,
                      Age >54 & Age<60 ~ 55,
                      Age >59 & Age<65 ~ 60,
                      Age >64 & Age<70 ~ 65,
                      Age >69 & Age<75 ~ 70,
                      Age >74 & Age<80 ~ 75,
                      Age >79 ~ 80)) %>% 
  group_by(Sex,Age5) %>% 
  summarize(NDep=sum(DepRate==0),
            Dep= sum(DepRate),
            Pop=sum(NDep+Dep),
            Prev=Dep/Pop)

Dep2009<- ENFR_2009 %>% 
  mutate(Age5 = case_when(Age >14 & Age<20 ~ 15,
                          Age >19 & Age<25 ~ 20,
                          Age >24 & Age<30 ~ 25,
                          Age >29 & Age<35 ~ 30,
                          Age >34 & Age<40 ~ 35,
                          Age >39 & Age<45 ~ 40,
                          Age >44 & Age<50 ~ 45,
                          Age >49 & Age<55 ~ 50,
                          Age >54 & Age<60 ~ 55,
                          Age >59 & Age<65 ~ 60,
                          Age >64 & Age<70 ~ 65,
                          Age >69 & Age<75 ~ 70,
                          Age >74 & Age<80 ~ 75,
                          Age >79 ~ 80)) %>% 
  group_by(Sex,Age5) %>% 
  summarize(NDep=sum(DepRate==0),
            Dep= sum(DepRate),
            Pop=sum(NDep+Dep),
            Prev=Dep/Pop)


Dep2013<- ENFR_2013 %>% 
  mutate(Age5 = case_when(Age >14 & Age<20 ~ 15,
                          Age >19 & Age<25 ~ 20,
                          Age >24 & Age<30 ~ 25,
                          Age >29 & Age<35 ~ 30,
                          Age >34 & Age<40 ~ 35,
                          Age >39 & Age<45 ~ 40,
                          Age >44 & Age<50 ~ 45,
                          Age >49 & Age<55 ~ 50,
                          Age >54 & Age<60 ~ 55,
                          Age >59 & Age<65 ~ 60,
                          Age >64 & Age<70 ~ 65,
                          Age >69 & Age<75 ~ 70,
                          Age >74 & Age<80 ~ 75,
                          Age >79 ~ 80)) %>% 
  group_by(Sex,Age5) %>% 
  summarize(NDep=sum(DepRate==0),
            Dep= sum(DepRate),
            Pop=sum(NDep+Dep),
            Prev=Dep/Pop)

Dep2018<- ENFR_2018 %>% 
  mutate(Age5 = case_when(Age >14 & Age<20 ~ 15,
                          Age >19 & Age<25 ~ 20,
                          Age >24 & Age<30 ~ 25,
                          Age >29 & Age<35 ~ 30,
                          Age >34 & Age<40 ~ 35,
                          Age >39 & Age<45 ~ 40,
                          Age >44 & Age<50 ~ 45,
                          Age >49 & Age<55 ~ 50,
                          Age >54 & Age<60 ~ 55,
                          Age >59 & Age<65 ~ 60,
                          Age >64 & Age<70 ~ 65,
                          Age >69 & Age<75 ~ 70,
                          Age >74 & Age<80 ~ 75,
                          Age >79 ~ 80)) %>% 
  group_by(Sex,Age5) %>% 
  summarize(NDep=sum(DepRate==0),
            Dep= sum(DepRate),
            Pop=sum(NDep+Dep),
            Prev=Dep/Pop)


AllDep <- rbind(Dep2005,Dep2009,Dep2013,Dep2018)

AllDep$Year <- PoblacionArg$Year


AllDep$Sex[AllDep$Sex==1] <- "Males"
AllDep$Sex[AllDep$Sex==2] <- "Females" 

AllDep2 <- AllDep %>% 
filter(Age5>19)

PrevYear <- AllDep2 %>% 
  group_by(Sex,Year) %>% 
  summarize(Prevalence=mean(Prev))
  
ggplot(transform(AllDep2,
                 Sex=factor(Sex,levels=c("Males","Females")))) + 
  aes(x=Age5,y=Prev*100,color= as.factor(Year)) +
  #geom_smooth()+
  geom_smooth(lwd=1)+ 
  theme_bw()+
  labs(color="Year")+
  labs(x="Age",y="% of Population reporting Anxiety/Depression")+
  scale_colour_grey(end=0.9)+
  facet_wrap(~Sex)

PoblacionArg$PrevDep <- AllDep$Prev 
PoblacionArg$PopDep <- AllDep$Pop 

PoblacionArg$Sex[PoblacionArg$Sex==1] <- "Hombres" 
PoblacionArg$Sex[PoblacionArg$Sex==2] <- "Mujeres" 

PoblacionArgW <- PoblacionArg %>% 
  select(Sex,Year,Agenum,Pop,PopDep,DeathRate,PrevDep) %>% 
  filter(Agenum>19) %>% 
  pivot_wider(names_from=c(Sex,Year),values_from=c(Pop,PopDep,DeathRate,PrevDep)) 

###Lifetable, only Mortality 

lifetable <- function(rates,age){
  mx <- rates
  #ax
  n <- c(diff(age),1)
  ax <- 0.5*n
  qx <- n*mx / (1+(n-ax)*mx)
  qx[qx>1] <- 1
  px <- 1 - qx
  ##lx
  lx <- c(100000,rep(0,(length(mx)-1)))
  for(i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i]
  }
  #dx
  dx <- lx*qx
  # Lx
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) - 1){
    Lx[i] <- lx[i+1]*n[i] + ax[i] *dx[i]
  }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  # Tx
  
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  
  lifetable <- data.frame(age=age,n=round(n,0),mx=round(mx,5),
                          qx=round(qx,5),px=round(px,5),
                          ax=round(ax,2),dx=round(dx,0),
                          lx=round(lx,0),Lx=round(Lx,0),
                          Tx=round(Tx,0),ex=round(ex,2))
  return(lifetable)
}



####LE function


LE20 <- function (ex) {
  e20 <- ex[1]
  return(e20)
}


###Lifetable, with disability to estimate error

lifetable.error <-  function(rates,pop,age=seq(20,80,5)){
  mx <- rates
  #ax
  n <- c(diff(age),1)
  ax <- 0.5*n
  qx <- n*mx / (1+(n-ax)*mx)
  qx[qx>1] <- 1
  px <- 1 - qx
  ##lx
  lx <- c(100000,rep(0,(length(mx)-1)))
  for(i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i]
  }
  #dx
  dx <- lx*qx
  # Lx
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) - 1){
    Lx[i] <- lx[i+1]*n[i] + ax[i] *dx[i]
  }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  # Tx
  
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  
  Pop <- pop
  V <- mx*(1-mx)/pop
  LSV <- V*Lx*Lx
  LSVT <-  rev(cumsum(rev(LSV)))
  SE <- sqrt(LSVT/(lx*lx))
  
  return(SE[1])
}



#Life expectancy

HombresMort2005<- lifetable(rates=PoblacionArgW$DeathRate_Hombres_2005,age=PoblacionArgW$Agenum)
LEHombresMort2005 <- LE20(HombresMort2005$ex)
HombresMort2005int <- lifetable.error(rates=PoblacionArgW$DeathRate_Hombres_2005,PoblacionArgW$Pop_Hombres_2005)*1.96
HombresMort2005tot <- c(LEHombresMort2005,LEHombresMort2005-HombresMort2005int,LEHombresMort2005+HombresMort2005int)

HombresMort2009<- lifetable(rates=PoblacionArgW$DeathRate_Hombres_2009,age=PoblacionArgW$Agenum)
LEHombresMort2009 <- LE20(HombresMort2009$ex)

HombresMort2013<- lifetable(rates=PoblacionArgW$DeathRate_Hombres_2013,age=PoblacionArgW$Agenum)
LEHombresMort2013 <- LE20(HombresMort2013$ex)

HombresMort2018<- lifetable(rates=PoblacionArgW$DeathRate_Hombres_2018,age=PoblacionArgW$Agenum)
LEHombresMort2018 <- LE20(HombresMort2018$ex)

MujeresMort2005<- lifetable(rates=PoblacionArgW$DeathRate_Mujeres_2005,age=PoblacionArgW$Agenum)
LEMujeresMort2005 <- LE20(MujeresMort2005$ex)

MujeresMort2009<- lifetable(rates=PoblacionArgW$DeathRate_Mujeres_2009,age=PoblacionArgW$Agenum)
LEMujeresMort2009 <- LE20(MujeresMort2009$ex)

MujeresMort2013<- lifetable(rates=PoblacionArgW$DeathRate_Mujeres_2013,age=PoblacionArgW$Agenum)
LEMujeresMort2013 <- LE20(MujeresMort2013$ex)

MujeresMort2018<- lifetable(rates=PoblacionArgW$DeathRate_Mujeres_2018,age=PoblacionArgW$Agenum)
LEMujeresMort2018 <- LE20(MujeresMort2018$ex)

LEvector <- c(LEHombresMort2005,LEHombresMort2009,LEHombresMort2013,LEHombresMort2018,
              LEMujeresMort2005,LEMujeresMort2009,LEMujeresMort2013,LEMujeresMort2018)


#### Healthy Life Expectancy 

### sullivan lt 

Sullivan.fun = function(rates,age=seq(20,80,5)){
  lengthvec <-length(rates)
  mx <- rates[1:(lengthvec/2)]
  wx <- rates[(lengthvec/2+1):lengthvec]
  #ax
  n <- c(diff(age),1)
  ax <- 0.5*n
  qx <- n*mx / (1+(n-ax)*mx)
  qx[qx>1] <- 1
  px <- 1 - qx
  ##lx
  lx <- c(100000,rep(0,(length(mx)-1)))
  for(i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i]
  }
  #dx
  dx <- lx*qx
  # Lx
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) - 1){
    Lx[i] <- lx[i+1]*n[i] + ax[i] *dx[i]
  }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  
  Lx.health <- Lx*(1-wx)
  ex.health <- sum(Lx.health)/lx[1]
  
  
  return(ex.health)
}


###Sullivan, error


Sullivan.error = function(rates,pop,age=seq(20,80,5)){
  lengthvec <-length(rates)
  mx <- rates[1:(lengthvec/2)]
  wx <- rates[(lengthvec/2+1):lengthvec]
  #ax
  n <- c(diff(age),1)
  ax <- 0.5*n
  qx <- n*mx / (1+(n-ax)*mx)
  qx[qx>1] <- 1
  px <- 1 - qx
  ##lx
  lx <- c(100000,rep(0,(length(mx)-1)))
  for(i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i]
  }
  #dx
  dx <- lx*qx
  # Lx
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) - 1){
    Lx[i] <- lx[i+1]*n[i] + ax[i] *dx[i]
  }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  
  Lx.health <- Lx*(1-wx)
  ex.health <- sum(Lx.health)/lx[1]
  
  Pop <- pop
  V <- wx*(1-wx)/pop
  LSV <- V*Lx*Lx
  LSVT <-  rev(cumsum(rev(LSV)))
  SE <- sqrt(LSVT/(lx*lx))
  
  return(SE[1])
}


Hombres20051sul <- Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2005,PoblacionArgW$PrevDep_Hombres_2005))
Hombres2005sulint <- Sullivan.error(c(PoblacionArgW$DeathRate_Hombres_2005,PoblacionArgW$PrevDep_Hombres_2005),PoblacionArgW$PopDep_Hombres_2005)*1.96
Hombres2005sultot <- c(Hombres20051sul ,Hombres20051sul-Hombres2005sulint,Hombres20051sul+Hombres2005sulint)


Hombres20091sul <- Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2009,PoblacionArgW$PrevDep_Hombres_2009))
Hombres2009sulint <- Sullivan.error(c(PoblacionArgW$DeathRate_Hombres_2009,PoblacionArgW$PrevDep_Hombres_2009),PoblacionArgW$PopDep_Hombres_2009)*1.96
Hombres2009sultot <- c(Hombres20091sul ,Hombres20091sul-Hombres2009sulint,Hombres20091sul+Hombres2009sulint)


Hombres20131sul <- Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2013,PoblacionArgW$PrevDep_Hombres_2013))
Hombres2013sulint <- Sullivan.error(c(PoblacionArgW$DeathRate_Hombres_2013,PoblacionArgW$PrevDep_Hombres_2013),PoblacionArgW$PopDep_Hombres_2013)*1.96
Hombres2013sultot <- c(Hombres20131sul ,Hombres20131sul-Hombres2013sulint,Hombres20131sul+Hombres2013sulint)


Hombres20181sul <- Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2018,PoblacionArgW$PrevDep_Hombres_2018))
Hombres2018sulint <- Sullivan.error(c(PoblacionArgW$DeathRate_Hombres_2018,PoblacionArgW$PrevDep_Hombres_2018),PoblacionArgW$PopDep_Hombres_2018)*1.96
Hombres2018sultot <- c(Hombres20181sul ,Hombres20181sul-Hombres2018sulint,Hombres20181sul+Hombres2018sulint)



Mujeres20051sul <- Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2005,PoblacionArgW$PrevDep_Mujeres_2005))
Mujeres2005sulint <- Sullivan.error(c(PoblacionArgW$DeathRate_Mujeres_2005,PoblacionArgW$PrevDep_Mujeres_2005),PoblacionArgW$PopDep_Mujeres_2005)*1.96
Mujeres2005sultot <- c(Mujeres20051sul ,Mujeres20051sul-Mujeres2005sulint,Mujeres20051sul+Mujeres2005sulint)


Mujeres20091sul <- Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2009,PoblacionArgW$PrevDep_Mujeres_2009))
Mujeres2009sulint <- Sullivan.error(c(PoblacionArgW$DeathRate_Mujeres_2009,PoblacionArgW$PrevDep_Mujeres_2009),PoblacionArgW$PopDep_Mujeres_2009)*1.96
Mujeres2009sultot <- c(Mujeres20091sul ,Mujeres20091sul-Mujeres2009sulint,Mujeres20091sul+Mujeres2009sulint)


Mujeres20131sul <- Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2013,PoblacionArgW$PrevDep_Mujeres_2013))
Mujeres2013sulint <- Sullivan.error(c(PoblacionArgW$DeathRate_Mujeres_2013,PoblacionArgW$PrevDep_Mujeres_2013),PoblacionArgW$PopDep_Mujeres_2013)*1.96
Mujeres2013sultot <- c(Mujeres20131sul ,Mujeres20131sul-Mujeres2013sulint,Mujeres20131sul+Mujeres2013sulint)


Mujeres20181sul <- Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2018,PoblacionArgW$PrevDep_Mujeres_2018))
Mujeres2018sulint <- Sullivan.error(c(PoblacionArgW$DeathRate_Mujeres_2018,PoblacionArgW$PrevDep_Mujeres_2018),PoblacionArgW$PopDep_Mujeres_2018)*1.96
Mujeres2018sultot <- c(Mujeres20181sul ,Mujeres20181sul-Mujeres2018sulint,Mujeres20181sul+Mujeres2018sulint)




LEvector <- c(LEHombresMort2005,LEHombresMort2009,LEHombresMort2013,LEHombresMort2018,
              LEMujeresMort2005,LEMujeresMort2009,LEMujeresMort2013,LEMujeresMort2018)

HLEvector <- c(Hombres2005sultot,Hombres2009sultot,Hombres2013sultot,Hombres2018sultot,
              Mujeres2005sultot,Mujeres2009sultot,Mujeres2013sultot,Mujeres2018sultot)

HLEvector <- round(HLEvector,1) 




tableofHLES <- round(data.frame(LEHombresMort2005,LEHombresMort2009,LEHombresMort2013,LEHombresMort2018,
                                LEMujeresMort2005,LEMujeresMort2009,LEMujeresMort2013,LEMujeresMort2018,
                                Hombres20051sul,Hombres20091sul,Hombres20131sul,Hombres20181sul,
                                Mujeres20051sul,Mujeres20091sul,Mujeres20131sul,Mujeres20181sul),2)


tableofHLES <- data.frame(dflelabel,tableofHLES)
#write.table(tableofHLES,"Values of LE35 and DFLE by ed Spain Decomposition.xls",sep=" ")


###Decomposition

library(remotes)
library(DemoDecomp)



HE_Decomp_swM1 <- stepwise_replacement ( func=Sullivan.fun,
                                         pars1 = c(PoblacionArgW$DeathRate_Hombres_2005,PoblacionArgW$PrevDep_Hombres_2005),
                                         pars2 = c(PoblacionArgW$DeathRate_Hombres_2009,PoblacionArgW$PrevDep_Hombres_2009))


HE_Decomp_swM2 <- stepwise_replacement ( func=Sullivan.fun,
                                         pars1 = c(PoblacionArgW$DeathRate_Hombres_2009,PoblacionArgW$PrevDep_Hombres_2009),
                                         pars2 = c(PoblacionArgW$DeathRate_Hombres_2013,PoblacionArgW$PrevDep_Hombres_2013))


HE_Decomp_swM3 <- stepwise_replacement ( func=Sullivan.fun,
                                         pars1 = c(PoblacionArgW$DeathRate_Hombres_2013,PoblacionArgW$PrevDep_Hombres_2013),
                                         pars2 = c(PoblacionArgW$DeathRate_Hombres_2018,PoblacionArgW$PrevDep_Hombres_2018))


HE_Decomp_swF1 <- stepwise_replacement ( func=Sullivan.fun,
                                         pars1 = c(PoblacionArgW$DeathRate_Mujeres_2005,PoblacionArgW$PrevDep_Mujeres_2005),
                                         pars2 = c(PoblacionArgW$DeathRate_Mujeres_2009,PoblacionArgW$PrevDep_Mujeres_2009))


HE_Decomp_swF2 <- stepwise_replacement ( func=Sullivan.fun,
                                         pars1 = c(PoblacionArgW$DeathRate_Mujeres_2009,PoblacionArgW$PrevDep_Mujeres_2009),
                                         pars2 = c(PoblacionArgW$DeathRate_Mujeres_2013,PoblacionArgW$PrevDep_Mujeres_2013))


HE_Decomp_swF3 <- stepwise_replacement ( func=Sullivan.fun,
                                         pars1 = c(PoblacionArgW$DeathRate_Mujeres_2013,PoblacionArgW$PrevDep_Mujeres_2013),
                                         pars2 = c(PoblacionArgW$DeathRate_Mujeres_2018,PoblacionArgW$PrevDep_Mujeres_2018))



##making a matrix
HE_swM1 <- matrix(HE_Decomp_swM1,nrow=(length(HE_Decomp_swM1)/2),ncol=2,byrow=F)
colnames(HE_swM1) <- colnames(HE_swM1) <- c("Mortality","Morbidity")

HE_swM2 <- matrix(HE_Decomp_swM2,nrow=(length(HE_Decomp_swM2)/2),ncol=2,byrow=F)
colnames(HE_swM2) <- colnames(HE_swM2) <- c("Mortality","Morbidity")

HE_swM3 <- matrix(HE_Decomp_swM3,nrow=(length(HE_Decomp_swM3)/2),ncol=2,byrow=F)
colnames(HE_swM3) <- colnames(HE_swM3) <- c("Mortality","Morbidity")

HE_swF1 <- matrix(HE_Decomp_swF1,nrow=(length(HE_Decomp_swF1)/2),ncol=2,byrow=F)
colnames(HE_swF1) <- colnames(HE_swF1) <- c("Mortality","Morbidity")

HE_swF2 <- matrix(HE_Decomp_swF2,nrow=(length(HE_Decomp_swF2)/2),ncol=2,byrow=F)
colnames(HE_swF2) <- colnames(HE_swF2) <- c("Mortality","Morbidity")

HE_swF3 <- matrix(HE_Decomp_swF3,nrow=(length(HE_Decomp_swF3)/2),ncol=2,byrow=F)
colnames(HE_swF3) <- colnames(HE_swF3) <- c("Mortality","Morbidity")




##create df
#Males

new.age <- c(seq(20,80,5))

HE_swM1_df <- mutate(as.data.frame(HE_swM1),Age=new.age)

HE_swM1_res<- HE_swM1_df %>%
  as.tibble() %>%
  pivot_longer(c(1,2),
               names_to = "Effect",
               values_to = "Contribution")

HE_swM1_res$type <- "2005-2009"



HE_swM2_df <- mutate(as.data.frame(HE_swM2),Age=new.age)

HE_swM2_res<- HE_swM2_df %>%
  as.tibble() %>%
  pivot_longer(c(1,2),
               names_to = "Effect",
               values_to = "Contribution")

HE_swM2_res$type <- "2009-2013"


HE_swM3_df <- mutate(as.data.frame(HE_swM3),Age=new.age)

HE_swM3_res<- HE_swM3_df %>%
  as.tibble() %>%
  pivot_longer(c(1,2),
               names_to = "Effect",
               values_to = "Contribution")

HE_swM3_res$type <- "2013-2018"


HE_swM16 <- rbind(HE_swM1_res,
                  HE_swM2_res,
                  HE_swM3_res)



HE_swF1_df <- mutate(as.data.frame(HE_swF1),Age=new.age)

HE_swF1_res<- HE_swF1_df %>%
  as.tibble() %>%
  pivot_longer(c(1,2),
               names_to = "Effect",
               values_to = "Contribution")

HE_swF1_res$type <- "2005-2009"



HE_swF2_df <- mutate(as.data.frame(HE_swF2),Age=new.age)

HE_swF2_res<- HE_swF2_df %>%
  as.tibble() %>%
  pivot_longer(c(1,2),
               names_to = "Effect",
               values_to = "Contribution")

HE_swF2_res$type <- "2009-2013"


HE_swF3_df <- mutate(as.data.frame(HE_swF3),Age=new.age)

HE_swF3_res<- HE_swF3_df %>%
  as.tibble() %>%
  pivot_longer(c(1,2),
               names_to = "Effect",
               values_to = "Contribution")

HE_swF3_res$type <- "2013-2018"


HE_swF16 <- rbind(HE_swF1_res,
                  HE_swF2_res,
                  HE_swF3_res)




#males 



plot1 <- ggplot(HE_swM16 ,  aes(x = as.factor(Age),
                       y = Contribution,
                       fill = Effect))+
  geom_bar(stat= "identity", position="stack") +
  ylim(-0.8, 0.8) +
  geom_hline(yintercept = 0) +
  labs(title = "Age-Specific Contribution in ADFLE20 
       Differentials in Males, 2005-18" ,size=0.5,
       x="Age",
       y="ADFLE20 Difference") +
  theme_bw()+
  scale_fill_manual(values=c("gray","black"))+
  scale_x_discrete(breaks=c(20, 40, 60,80))+
  facet_wrap(~ type)

#females

plot2 <- ggplot(HE_swF16 ,  aes(x = as.factor(Age),
                       y = Contribution,
                       fill = Effect))+
  geom_bar(stat= "identity", position="stack") +
  ylim(-0.8, 0.8) +
  geom_hline(yintercept = 0) +
    labs(title = "Age-Specific Contribution in ADFLE20 
    Differentials in Females, 2005-18" ,size=0.5,
       x="Age",
       y="ADFLE20 Difference") +
  theme_bw()+
  scale_fill_manual(values=c("gray","black"))+
  scale_x_discrete(breaks=c(20, 40, 60,80))+
  facet_wrap(~ type)

library(gridExtra)
grid.arrange(plot1, plot2, nrow=2)


###comprobación que las descomposiciones están bien...deberían dar 0 las diferencias.
sum(HE_swM1_res$Contribution) - ( Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2009,PoblacionArgW$PrevDep_Hombres_2009))- Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2005,PoblacionArgW$PrevDep_Hombres_2005)))                           
sum(HE_swM2_res$Contribution) - ( Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2013,PoblacionArgW$PrevDep_Hombres_2013))- Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2009,PoblacionArgW$PrevDep_Hombres_2009)))                           
sum(HE_swM3_res$Contribution) - ( Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2018,PoblacionArgW$PrevDep_Hombres_2018))- Sullivan.fun(c(PoblacionArgW$DeathRate_Hombres_2013,PoblacionArgW$PrevDep_Hombres_2013)))                           

sum(HE_swF1_res$Contribution) - ( Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2009,PoblacionArgW$PrevDep_Mujeres_2009))- Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2005,PoblacionArgW$PrevDep_Mujeres_2005)))                           
sum(HE_swF2_res$Contribution) - ( Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2013,PoblacionArgW$PrevDep_Mujeres_2013))- Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2009,PoblacionArgW$PrevDep_Mujeres_2009)))                           
sum(HE_swF3_res$Contribution) - ( Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2018,PoblacionArgW$PrevDep_Mujeres_2018))- Sullivan.fun(c(PoblacionArgW$DeathRate_Mujeres_2013,PoblacionArgW$PrevDep_Mujeres_2013)))                           
                          



sum(HE_swM1_df$Morbidity) 
sum(HE_swM2_df$Morbidity) 
sum(HE_swM3_df$Morbidity) 

sum(HE_swM1_df$Mortality) 
sum(HE_swM2_df$Mortality) 
sum(HE_swM3_df$Mortality) 

sum(HE_swF1_df$Morbidity) 
sum(HE_swF2_df$Morbidity) 
sum(HE_swF3_df$Morbidity) 


sum(HE_swF1_df$Mortality) 
sum(HE_swF2_df$Mortality) 
sum(HE_swF3_df$Mortality) 



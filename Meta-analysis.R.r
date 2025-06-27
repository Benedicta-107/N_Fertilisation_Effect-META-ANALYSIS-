date()
#"Thu Sep 02 10:37:56 2021"

#setwd("C:/BENY/")
library(Matrix)
library(lme4)
library(nlme)
library(lmerTest)
library(readxl)
library(tidyverse)
library(dplyr)
library(plotrix)

getwd()
#data=read.csv("C:\\BENY\\Benysdata.csv")
#data=read.csv("C:\\BENY\\Benysdata.csv",stringsAsFactors = F)
data=read.csv("C:/Dropbox/0-R/R data/Benysdata.csv",stringsAsFactors=F)

#data=read.csv("C:\\BENY\\Beny.csv",stringsAsFactors=F)
#data=read.csv("C:/Dropbox/0-R/R data/Beny.csv",stringsAsFactors=F)
#data=na.omit(data)
#data=read.csv("C:\\BENY\\Beny.csv",stringsAsFactors=F)

data$study=data$Study
data$country=data$Location
data$Ntype=data$N.or.Fertilizer.type
data$N=data$full.N.Rate.kg.ha.
data$SoilN=data$Soil.Nitrogen.Content..kg.ha.
data$F=data$Fungicide.input..L.ha.
data$Fname=data$fungicide.name
data$yield=data$Yield..t.ha.
data$T=data$seasonal.temperature..Farenheit.
data$C.C=data$clay.content
data$A=data$aridity
data$R=data$Seasonal.Rainfall.mm.
data$P=data$P.Input..kg.ha.
data$K=data$K.content.in.the.soil
data$SPH=data$Soil.Ph

head(data)
mean(data$yield)
mean(data$AUDPC)
max(data$AUDPC)
min(data$AUDPC)
data$AUDPC1=exp(data$AUDPC)
hist(data$AUDPC)
hist(data$AUDPC1)
summary(data$AUDPC1)

summary(is.na(data$AUDPC1))


max(data$AUDPC1)
max(data$AUDPC)
exp(max(data$AUDPC))
data$study[data$AUDPC==97.44] 
# "Matias Schierenbeck et al, 2019"

#  as well in "D.D Howard et al (2013)", # 21.3 is probably 2.13, or

min(data$AUDPC1)

hist(data$yield)
hist(data$AUDPC1)
hist(log(data$AUDPC+1))
log(0.01)
log(0)
#Data cleaning, check the data and missing values
class(data$N)
max(data$N)
min(data$N)
mean(data$N)
is.na(data$N)
summary(is.na(data$N))

class(data$F)
max(data$F)
min(data$F)
mean(data$F)
is.na(data$F)
summary(is.na(data$F))

data$aridity=na.omit(data$aridity)
class(data$aridity)
max(data$aridity)
min(data$aridity)
mean(data$aridity)
is.na(data$aridity)
summary(is.na(data$Aridity))

class(data$Rainfall)
max(data$Rainfall)
min(data$Rainfall)
mean(data$Rainfall)
is.na(data$Rainfall)
summary(is.na(data$Rainfall))

class(data$Temp..F.)
max(data$Temp..F.)
min(data$Temp..F.)
mean(data$Temp..F.)
is.na(data$Temp..F.)
summary(is.na(data$Temp..F.))


max(data$yield)
min(data$yield)
mean(data$yield)
is.na(data$yield)
summary(is.na(data$yield))

max(data$AUDPC)
min(data$AUDPC)
mean(data$AUDPC)
is.na(data$AUDPC)
summary(is.na(data$AUDPC))

# Hurray! no missing values :)

length(data$Study)
# 2501 rows

# To check for some general relationships
plot(data$yield~data$N)
abline(lm(data$yield~data$N))
summary(lm(data$yield~data$N))

plot(data$yield~data$AUDPC)
abline(lm(data$yield~data$AUDPC))
summary(lm(data$yield~data$AUDPC))

data$NUE[data$N!=0]=data$yield[data$N!=0]/data$N[data$N!=0]*1000
mean(data$NUE[data$N!=0])


plot(data$NUE[data$N!=0]~data$N[data$N!=0])
boxplot(data$NUE[data$N!=0])
abline(lm(data$NUE[data$N!=0]~data$N[data$N!=0]))
summary(lm(data$NUE[data$N!=0]~data$N[data$N!=0]))

mean(data$yield[data$N==0])
mean(data$yield[data$N!=0])
mean(data$N[data$N!=0])

mean(data$SoilN,na.rm=T)
min(data$SoilN,na.rm=T)
max(data$SoilN,na.rm=T)
unique(data$study[data$SoilN==0.02])
#"Benedicta Y. Fosu-Mensah, 2016"
unique(data$study[data$SoilN==167])
#"P.M Berry et al., (2010)"

quantile(data$N,1/3)
quantile(data$N,2/3)

# To define the N levels based on the N input quantiles
data$N1[data$N>quantile(data$N,2/3)]="high"
data$N1[data$N>quantile(data$N,1/3)&data$N<=quantile(data$N,2/3)]="medium"
data$N1[data$N<=quantile(data$N,1/3)]="low"

unique(data$N1)
class(data$N1)
data$N1=as.factor(data$N1)
levels(data$N1)
# To re-level the factors in good order
data$N1=factor(data$N1,levels=c("low","medium","high"))
levels(data$N1)

mean(data$N)
unique(data$N)
length(data$N[data$N==0])

is.na(data$N)
unique(is.na(data$N))

plot(data$AUDPC~data$N)
abline(lm(data$AUDPC~data$N))
summary(lm(data$AUDPC~data$N))

#data$f=data$F
plot(data$yield~data$F)
abline(lm(data$yield~data$F))
summary(lm(data$yield~data$F))


#data$f=data$F
plot(data$AUDPC~data$F)
abline(lm(data$AUDPC~data$F))
summary(lm(data$AUDPC~data$F))

#data$a=data$Aridity
plot(data$AUDPC~data$A)
abline(lm(data$AUDPC~data$A))
summary(lm(data$AUDPC~data$A))


#data$N=as.numeric(as.character(data$N.rate..kg.ha.))
class(data$N)
max(data$N)

head(data)

length(data$N)
#data$N=as.numeric(as.character(data$N))
#data$N=as.numeric(data$N)
max(data$F)
min(data$F)
length(data$F)
data$F=as.numeric(data$F)
data$AUDPC=as.numeric(data$AUDPC)


# compute lnR for yield and AUDPC, respectively 
uniq= unique(data$study)
uniq

length(uniq)
# 158 studies, .

data1=c() 
data2=c()

0%in%data$N

# Using for loop to calculate the Log Response Ratio lnr

for (i in 1:length(uniq)) 
{
  subdata1=subset(data, study==uniq[i])
  uniq_y=unique(subdata1$Year)
  for (j in 1:length(uniq_y))
       {
        subdata=subset(subdata1,Year==uniq_y[j])
  if (0%in%unique(subdata$N) & length(unique(subdata$N))>1)
  {
    ref=mean(subdata$yield[subdata$N==0])
    #min(data$yield[data$N==0])
    ref_d=mean(subdata$AUDPC1[subdata$N==0])
    log_ratio_y=log(subdata$yield/ref)# this is to calculate lnR for yield
    log_ratio_d=log((subdata$AUDPC1)/ref_d) # this is to calculate lnR for disease
    #summary(log_ratio_d)
    #summary(log_ratio_y)
    data1=cbind(subdata,log_ratio_y,log_ratio_d)
    data2=rbind(data2,data1)
  }
  else 
  {
    subdata=c()
  }
}}
#}
#data2

head(data2)
tail(data2)

# to check the data if necessary
#write.csv(data2,"C:/Dropbox/0-R/R data/check data-beny.csv")
# OR:
#(write.csv(data2,"C:\\BENY\\check data-beny.csv")

uniq= unique(data$study)
uniq

length(uniq)


unique(data2$study)
length(unique(data2$study))

unique(data2$N_level)
#unique(data2$f_level)
#unique(data$a_level)
#unique(data$r_level)
#unique(data$t_level)

min(data2$log_ratio_d)
max(data2$log_ratio_d)

min(data2$log_ratio_y)
max(data2$log_ratio_y)
summary(data2$log_ratio_y)

head(data2)
unique(data2$N_level)

max(data2$log_ratio_d)
min(data2$log_ratio_d)
summary(data2$log_ratio_d)

# lnR ~ N level
#model_d=lme(log_ratio_d~A,random=~1|study,data2)# mixed-effect model
#model_d=lme(log_ratio_d~N_level-1,random=~1|study,data2)# mixed-effect model

model_d=lme(log_ratio_d~N1-1,random=~1|study,data2)# mixed-effect model
#model_d=lme(log_ratio_d~f_level+r_level+t_level,random=~1|study,data2)# mixed-effect model
summary(model_d)

model_y=lme(log_ratio_y~N1-1,random=~1|study,data2)# mixed-effect model
summary(model_y)

#lnR ~ f level
#model_d=lme(log_ratio_d~f_level,random=~1|study,data)# mixed-effect model
#summary(model_d)
#model_y=lme(log_ratio_y~f_level,random=~1|study,data)# mixed-effect model
#summary(model_y)

#extract coefficients of the disease model
summary(model_y)$tTable # try to understand the results
coef<-data.frame(summary(model_y)$tTable[,c(1,2,5)]);coef 
# c(1,2) are row numbers, c(1,2,5) are column numbers
t_crit<-qt(0.025,summary(model_y)$tTable[,3]);t_crit 
# to determine the value from t distribution 
confd<-coef[,2]*abs(t_crit)
#coef[1,3]
confdMin <- coef[,1] - confd # lower limit
confdMax <- coef[,1] + confd # upper limit
#comparID=c(1:2)
comparID=c(1:3)

#comparID<-c("-N", "+N")
comparID<-c( "low","medium","high")

Meancoef<-cbind(comparID, coef,confd,confdMin,confdMax);Meancoef

#2 Exponentially transformed and calculate changes in %

UnlogMean <- (exp(Meancoef[ ,2])*100)-100
UnlogConfdMax <- (exp(Meancoef[ ,7])*100)-100
UnlogConfdMin <- (exp(Meancoef[ ,6])*100)-100

unlogcombied <- cbind(Meancoef,UnlogMean,UnlogConfdMin,UnlogConfdMax)
Combine=rbind(unlogcombied)
length(Combine$comparID)
#comparID_new=c(1:2)
comparID_new=c(1:3)

result_y=cbind(comparID_new,Combine);result_y

#unlogcombied=unlogcombied[with(unlogcombied, order(-UnlogMean)),]
#write.csv (unlogcombied, file="unlogcombied")

#extract coefficients from the yield model
summary(model_d)$tTable # try to understand the results
coef<-data.frame(summary(model_d)$tTable[,c(1,2,5)]);coef 
# c(1,2) are row numbers, c(1,2,5) are column numbers
t_crit<-qt(0.025,summary(model_d)$tTable[,3]);t_crit 
# to determine the value from t distribution 
confd<-coef[,2]*abs(t_crit)
confdMin <- coef[,1] - confd # lower limit
confdMax <- coef[,1] + confd # upper limit
comparID=c(1:3)
comparID<-c("low", "medium","high")
Meancoef<-cbind(comparID, coef,confd,confdMin,confdMax);Meancoef

#2 Exponentially transformed and calculate changes in %
#UnlogMean <- (exp(Meancoef[ ,2])*100)-100
#UnlogConfdMax <- (exp(Meancoef[ ,7])*100)-100
#UnlogConfdMin <- (exp(Meancoef[ ,6])*100)-100

UnlogMean <- ((Meancoef[ ,2])*100)-100
UnlogConfdMax <- ((Meancoef[ ,7])*100)-100
UnlogConfdMin <- ((Meancoef[ ,6])*100)-100

unlogcombied <- cbind(Meancoef,UnlogMean,UnlogConfdMin,UnlogConfdMax)
Combine=rbind(unlogcombied)
length(Combine$comparID)
comparID_new=c(1:3)
result_d=cbind(comparID_new,Combine);result_d

#unlogcombied=unlogcombied[with(unlogcombied, order(-UnlogMean)),]
#write.csv (unlogcombied, file="unlogcombied")

Combine_new=rbind(result_y,result_d);Combine_new
#Combine_new=rbind(result_y);Combine_new
# 3 plot the figure (start)
library(plotrix)
#win.graph(7,8)
#?plotCI
par(mfrow = c(1, 1),oma = c(2, 5, 2, 5))
#comparID=factor(comparID,levels=c(1,3,2))
#par()
plotCI(x=Combine_new$UnlogMean,
       #y=c(1:4),
       y=c(1:6),
       
       ui=Combine_new$UnlogConfdMax,
       li=Combine_new$UnlogConfdMin, 
       yaxt="n", 
       err="x",
       pch=19, 
       #xlim=c(-150,100),
       ylim=c(0,7),
       #ylim=c(1,3),
       
       xlab=list("Change in percent (%)",font=2), ylab ="",
       main="Effect of N input")

axis(2,at=c(2,5),
     las=1, 
     font=2, 
     oma = c(4, 8, 4, 4), 
     labels=c("Yield","Disease"))

axis(4,at=c(6:1),
     las=1, 
     font=1, 
     oma = c(4, 8, 4, 4), 
     labels=c("High", "Medium","Low","High", "Medium","Low"))

abline(v=0,lty=1)
abline(h=c(3.5),lty=2)

#text(65,3.5,cex = 1,c("Maize"))
#text(65,7.5, cex = 1,c("Wheat"))
#text(-10,c(4:1),cex = 0.8,c("N+", "N-","N+", "N-"))

#axis(4,at=c(1:5),
#    las=1, 
#   font=2, 
#oma = c(4, 8, 4, 4), 
#  labels=c())

abline(v=0,lty=1)

#axis(4,at=c(4:1),
 #    las=1,tck=0, oma = c(4, 8, 4, 4), 
  #   font=1, cex.axis =1, 
   #  labels=c("N+", "N-","N+", "N-"))

head(data2)
data2$N2[data2$N==0]="ck"
data2$N2[data2$N>0]="N"


model_Y=lme(log_ratio_y~N2,random=~1|study,data2)# mixed-effect model
summary(model_N)
model_D=lme(log_ratio_d~N2,random=~1|study,data2)# mixed-effect model
summary(model_D)
summary(model_y)
summary(model_d)



data$Continent
unique(data$Continent)
boxplot(log_ratio_y~Continent,data2)
boxplot(log_ratio_y~Crop,data2)
head(data$Crop)
length(data2$yield[data2$Continent=="Africa"])

#extract coefficients from all models
summary(model_d)$tTable # try to understand the results
coef_d<-data.frame(summary(model_d)$tTable);coef_d
coef_y<-data.frame(summary(model_y)$tTable);coef_y
coef_D<-data.frame(summary(model_D)$tTable);coef_D
coef_Y<-data.frame(summary(model_Y)$tTable);coef_Y
coef=rbind(coef_d,coef_D,coef_y,coef_Y);coef

# c(1,2) are row numbers, c(1,2,5) are column numbers
t_crit<-qt(0.025,coef[,3]);t_crit 
# to determine the value from t distribution 
confd<-coef[,2]*abs(t_crit)
confdMin <- coef[,1] - confd # lower limit
confdMax <- coef[,1] + confd # upper limit
comparID=c(1:10)
comparID<-c("low", "medium","high","ck","N","low", "medium","high","ck1","N1")
Meancoef<-cbind(comparID, coef,confd,confdMin,confdMax);Meancoef
exp(0.21)
#2 Exponentially transformed and calculate changes in %
UnlogMean[c(1:5)] <- ((Meancoef[c(1:5),2])*100)-100
UnlogConfdMax[c(1:5)] <- ((Meancoef[c(1:5) ,9])*100)-100
UnlogConfdMin[c(1:5)] <- ((Meancoef[c(1:5) ,8])*100)-100

UnlogMean[c(6:10)] <- (exp(Meancoef[c(6:10) ,2])*100)-100
UnlogConfdMax[c(6:10)] <- (exp(Meancoef[c(6:10) ,9])*100)-100
UnlogConfdMin[c(6:10)] <- (exp(Meancoef[c(6:10) ,8])*100)-100

unlogcombied <- cbind(Meancoef,UnlogMean,UnlogConfdMin,UnlogConfdMax)
Combine_new=unlogcombied;Combine_new

library(plotrix)
#win.graph(7,8)
#?plotCI
par(mfrow = c(1, 1),oma = c(2, 5, 2, 8))
#comparID=factor(comparID,levels=c(1,3,2))
#par()
plotCI(x=Combine_new$UnlogMean,
       #y=c(1:4),
       y=c(10:1),
       ui=Combine_new$UnlogConfdMax,
       li=Combine_new$UnlogConfdMin, 
       yaxt="n", 
       err="x",
       pch=19, 
       #xlim=c(-150,100),
       ylim=c(1,10),
       #ylim=c(1,3),
       
       xlab=list("Change in percent (%)",font=2), ylab ="",
       main="Effect of N input")

axis(2,at=c(3,7),
     las=1, 
     font=2, 
     oma = c(4, 8, 4, 4), 
     labels=c("Yield","Disease"))

axis(4,at=c(10:1),
     las=1, 
     font=1, 
     oma = c(4, 8, 4, 4), 
     labels=c("low", "medium","high","ck","N","low", "medium","high","ck1","N1"))

abline(v=0,lty=1)
abline(h=c(5.5),lty=2)

#text(65,3.5,cex = 1,c("Maize"))
#text(65,7.5, cex = 1,c("Wheat"))
#text(-10,c(4:1),cex = 0.8,c("N+", "N-","N+", "N-"))

#axis(4,at=c(1:5),
#    las=1, 
#   font=2, 
#oma = c(4, 8, 4, 4), 
#  labels=c())

abline(v=0,lty=1)

#axis(4,at=c(4:1),
#    las=1,tck=0, oma = c(4, 8, 4, 4), 
#   font=1, cex.axis =1, 
#  labels=c("N+", "N-","N+", "N-"))

# 3 plot the figure (end)
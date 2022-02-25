
## Code for Stats
setwd('C:/Users/ntara/Documents/Berghia Excel Files')
library(car)


## These show the mean and standard deviation of fertilization of egg masses laid by animals 
##  isolated @10 wpf (fert_f), 12 wpf (fert_g), and Never isolated (fert_p)
## Expt 1: Age
## Fig 3F
fert.df <- read.csv('Isolation Fertilization Experiment.csv')[1:57,1:10]
fert_f <- fert.df[fert.df$wpfnev==10,]
fert_g <- fert.df[fert.df$wpfnev==12,]
fert_p <- fert.df[fert.df$wpfnev=="Never",]

c(mean(fert_f$pFert), sd(fert_f$pFert))
c(mean(fert_g$pFert), sd(fert_g$pFert))
c(mean(fert_p$pFert), sd(fert_p$pFert))

## Mean and standard deviation of below10 of each fert category (J9/LJs and As)
## Expt #2: Size
## Fig 4B
isodata <- read.csv('ABCDTUV Isolation 2.csv')[1:306,1:11]
isoferts<-isodata[isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL',]
isobelow10<-isodata[isodata$whorln<11&(isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL'),]
isobelow10largexlarge<-isobelow10[isobelow10$categ=='LL',]
isobelow10largexadult<-isobelow10[isobelow10$categ=='LA',]
isobelow10adultxlarge<-isobelow10[isobelow10$categ=='AL',]
isobelow10adultxadult<-isobelow10[isobelow10$categ=='AA',]

c(mean(isobelow10largexlarge$pFert), sd(isobelow10largexlarge$pFert))
c(mean(isobelow10largexadult$pFert), sd(isobelow10largexadult$pFert))
c(mean(isobelow10adultxlarge$pFert), sd(isobelow10adultxlarge$pFert))
c(mean(isobelow10adultxadult$pFert), sd(isobelow10adultxadult$pFert))


## These are the linear models used to determine if there was a significant correlation
##  between the whorl order (whorln) and percent fertilization (pFert)
##  with the goal of showing how fertilization rate changed over time
## This was done for animals isolated @10 wpf (fert_f), 12 wpf (fert_g), and Never isolated (fert_p)
## Expt 1: Age
## Fig 4A
fert.df <- read.csv('Isolation Fertilization Experiment.csv')[1:57,1:10]
fert_f <- fert.df[fert.df$wpfnev==10,]
fert_g <- fert.df[fert.df$wpfnev==12,]
fert_p <- fert.df[fert.df$wpfnev=="Never",]

summary(lm(pFert~whorln, data=fert_f))
summary(lm(pFert~whorln, data=fert_g))
summary(lm(pFert~whorln, data=fert_p))

## This is the info that shows how a quadratic is better fitting of 4B than a linear regression
## Expt 1: Age
## Fig 4B
f.df <- read.csv('Isolation Fertilization.csv')[1:60,1:11]
f_g <- f.df[f.df$wpfnev==12,]

summary(lm(pFert~whorln,data= f_g))
whorlz<-f_g$whorln
whorl2<-(f_g$whorln)^2
quadratic<-lm(f_g$pFert~whorlz + whorl2)
summary(quadratic)

## This next test shows whether there is a difference between egg masses laid during the 5 week period by 
##  animals @10 wpf (fert_f), 12 wpf (fert_g), and Never isolated (fert_p) regardless of time
## Expt 1: Age
## Fig 3F
fert.df <- read.csv('Isolation Fertilization Experiment.csv')[1:57,1:10]
fert_noe <- fert.df[fert.df$wpfnev==10|fert.df$wpfnev==12|fert.df$wpfnev=="Never",]

TukeyHSD(aov(pFert~wpfnev, data=fert_noe))


## Anova and TukeyHSD of the first 10 egg masses from the J9/LJs and As
## Expt #2: Size
## Fig 5B
isodata <- read.csv('ABCDTUV Isolation 2.csv')[1:306,1:11]
isobelow10<-isodata[isodata$whorln<11&(isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL'),]

x<-aov(pFert~categ, data = isobelow10)
summary(x)
Anova(x)
TukeyHSD(x)

## Individual variation: Anova and TukeyHSD of the first 10 egg masses from the J9/LJs
## Expt #2: Size
## Fig 5C
isodata <- read.csv('ABCDTUV Isolation 2.csv')[1:306,1:11]
isobelow10<-isodata[isodata$whorln<11&(isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL'),]
isobelow10larges<-isobelow10[isobelow10$group=='L',]

z<-aov(pFert~animal, data = isobelow10larges)
Anova(z)
TukeyHSD(z)

## Individual variation: Anova and TukeyHSD of the first 10 egg masses from the As
## Expt #2: Size
## Fig 5C
isodata <- read.csv('ABCDTUV Isolation 2.csv')[1:306,1:11]
isobelow10<-isodata[isodata$whorln<11&(isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL'),]
isobelow10adults<-isobelow10[isobelow10$group=='A',]

z<-aov(pFert~animal, data = isobelow10adults)
Anova(z)
TukeyHSD(z)

## To make our data equivalent to Rutowski 1983 Table 8 I believe 
## Table on line 127
## "forno" means fertilized or not, "fert" means fertilized, "anotfert" meants not
## Expt 2: Size
isodata <- read.csv('ABCDTUV Isolation 2.csv')[1:306,1:11]

table<-data.frame(1:38,c(rep("fert",38), rep("anotfert",38)),0,0)
colnames(table)<- c("whorln","forno","n","pFert")
for(j in 1:38){
  for (i in 1:length(isoferts[,1])){
    if(isoferts[i,8]==table[j,1]){
      if(isoferts[i,7]<50){
        table[j+38,3] = table[j+38,3]+1
      } else {
        table[j,3] = table[j,3]+1
      }
    }
  }  
}
for (i in 1:38){
  table[i,4]<-round(100*table[i,3]/(table[i+38,3]+table[i,3]))
  table[i+38,4]<-table[i,4]
}
## Here's the table, mentioned in the paper about how fertilization
## goes below 50% at the 14th egg mass
table

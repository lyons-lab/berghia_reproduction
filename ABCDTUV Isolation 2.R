####################################
# ABCDTUV Isolation 2.R
# Written by: Neville Taraporevala
# Last Updated by: Jessica A. Goodheart
# Last Updated: 20 May 2022
# Purpose: To analyze the data from the second experiment (with mating for 1 week then isolation again)
# Inputs used: '../CSV Files/ABCDTUV Isolation 2.csv'
####################################

####################################
# Initial setup
####################################
# Call necessary libraries
require(ggplot2)
require(car)

# Set directory
#setwd('C:/Users/ntara/Documents/Berghia Excel Files')
setwd("/Users/jessicagoodheart/Dropbox (Personal)/Research/4Projects/berghia_repro_neville/Raw Data Files/R Scripts")

####################################
# Data from Size-Based Experiment 
####################################
# Read in raw data file
isodata <- read.csv('../CSV Files/ABCDTUV Isolation 2.csv')[1:306,1:11]

# Subset into multiple data matrices for different analyses
isoadultxadult <-isodata[isodata$categ=='AA',]
isoadultxsmall <-isodata[isodata$categ=='AS',]
isoadultxlarge <-isodata[isodata$categ=='AL',]
isoisos <-isodata[isodata$categ=='AN',]
isosmallxadult <-isodata[isodata$categ=='SA',]
isolargexadult <-isodata[isodata$categ=='LA',]
isosmallxsmall <-isodata[isodata$categ=='SS',]
isolargexlarge <-isodata[isodata$categ=='LL',]
isoadultferts<-isodata[isodata$categ=='AL'|isodata$categ=='AA',]
isoadults<-isodata[isodata$group=='A',]
isosmalls<-isodata[isodata$group=='S',]
isolarges<-isodata[isodata$group=='L',]
isoferts<-isodata[isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL',]
isobelow10<-isodata[isodata$whorln<11&(isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL'),]
isoabove10<-isodata[isodata$whorln>=11&(isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL'),]
isobelow10all<-isodata[isodata$whorln<11,]
isobelow10largexlarge<-isobelow10[isobelow10$categ=='LL',]
isobelow10largexadult<-isobelow10[isobelow10$categ=='LA',]
isobelow10adultxlarge<-isobelow10[isobelow10$categ=='AL',]
isobelow10adultxadult<-isobelow10[isobelow10$categ=='AA',]
isobelow10larges<-isobelow10[isobelow10$group=='L',]
isobelow10adults<-isobelow10[isobelow10$group=='A',]
isofirst<-isodata[isodata$whorln==1,]

## Mean and standard deviation of below10 of each fert category (J9/LJs and As)
c(mean(isobelow10largexlarge$pFert), sd(isobelow10largexlarge$pFert))
c(mean(isobelow10largexadult$pFert), sd(isobelow10largexadult$pFert))
c(mean(isobelow10adultxlarge$pFert), sd(isobelow10adultxlarge$pFert))
c(mean(isobelow10adultxadult$pFert), sd(isobelow10adultxadult$pFert))


## Anova and TukeyHSD of the first 10 egg masses from the J9/LJs and As
x<-aov(pFert~categ+animal, data = isobelow10)
summary(x)
Anova(x)
TukeyHSD(x)

## Anova and TukeyHSD of the first 10 egg masses from the J9/LJs
z<-aov(pFert~animal, data = isobelow10larges)
Anova(z)
TukeyHSD(z)

## Anova and TukeyHSD of the first 10 egg masses from the As
z<-aov(pFert~animal, data = isobelow10adults)
Anova(z)
TukeyHSD(z)

## Not used in the paper, just for fun
## This is similar to the Todd et al. 1997 paper graph
ggplot(data=isofirst, aes(x=categ, y=pFert))+
  geom_point(stat="identity",size = 2) +
  theme_classic()+ylim(0,100)+
  #geom_line()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Whorl Number", y="Percent Fertilized") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="right",legend.text=element_text(size=12),
        legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))

## Not used for paper. Lines for the first 10 egg masses and then 10+
ggplot(data=isobelow10, aes(x=whorln, y=pFert, color=categ))+
  stat_smooth(method="lm", se=F,show.legend = T)+
  geom_point(aes(size=nEggs), stat="identity", alpha = .3)+
  stat_smooth(data=isoabove10,method="lm", se=F,show.legend = T)+
  geom_point(data=isoabove10,aes(size=nEggs), stat="identity", alpha = .3)+
  theme_classic()+ylim(0,100)+xlim(1,40)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Egg Mass Number", y = "Percent Fertilized per Egg Mass")+#y="Percent Eggs Fertilized") + 
  #stat_summary(fun=mean, aes(), geom="line", colour="blue", size = 1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position='right',legend.text=element_text(size=12),
        legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
        legend.background = element_rect(fill="white",size=0.5, linetype="solid",colour ="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))
  
## Graph not used in the paper, just for fun.
## Shows all animals 
ggplot(data=isoferts, aes(x=ndays/7, y=pFert))+
  stat_smooth(method = "lm", se=F,show.legend = T, color = 'black')+
  geom_point(aes(size=nEggs), stat="identity", alpha = 0.3)+
  geom_point(data=isoferts[isoferts$animal=='A2',], aes(size=nEggs), alpha=.7, color = 'red')+
  geom_point(data=isoferts[isoferts$animal=='B6',], aes(size=nEggs), alpha=.7, color = 'orange')+
  geom_point(data=isoferts[isoferts$animal=='T6',], aes(size=nEggs), alpha=.7, color = 'blue')+
  theme_classic()+ ylim(0,100)+xlim(-.5,16)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Weeks Post Isolation", y = "Percent Fertilized per Egg Mass")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position='right',legend.text=element_text(size=12),
        legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",colour ="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))

# Old Version of Fig. 5A
ggplot(data=isobelow10all, aes(x=categ, y=pFert))+
  geom_boxplot(outlier.shape=NA)+ geom_jitter(aes(size=nEggs), width = 0.3, alpha = 0.3) +
  theme_classic()+
  #scale_x_discrete(limits = c("8","10","12","Never"))+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = element_blank(), y="Percent Fertilized per Egg Mass") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",#legend.text=element_text(size=12),
        #legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))



## Here's where I tried to do t-tests with weights based on number of eggs
install.packages("weights")
library(weights)

#LA x LL
wtd.t.test(isobelow10largexadult$pFert, isobelow10largexlarge$pFert,isobelow10largexadult$nEggs, isobelow10largexlarge$nEggs, samedata=F,
           alternative="two.tailed", mean1=TRUE, bootse=FALSE, bootp=FALSE,
           bootn=1000, drops="pairwise")
#AL x AA
wtd.t.test(isobelow10adultxlarge$pFert, isobelow10adultxadult$pFert,isobelow10adultxlarge$nEggs, isobelow10adultxadult$nEggs, samedata=F,
           alternative="two.tailed", mean1=TRUE, bootse=FALSE, bootp=FALSE,
           bootn=1000, drops="pairwise")
#LL x AL
wtd.t.test(isobelow10largexlarge$pFert, isobelow10adultxlarge$pFert,isobelow10largexlarge$nEggs, isobelow10adultxlarge$nEggs, samedata=F,
           alternative="two.tailed", mean1=TRUE, bootse=FALSE, bootp=FALSE,
           bootn=1000, drops="pairwise")
#AA x LA
wtd.t.test(isobelow10adultxadult$pFert, isobelow10largexadult$pFert,isobelow10adultxadult$nEggs, isobelow10largexadult$nEggs, samedata=F,
           alternative="two.tailed", mean1=TRUE, bootse=FALSE, bootp=FALSE,
           bootn=1000, drops="pairwise")


#Old version of Fig 5B
ggplot(data=isobelow10, aes(x=categ, y=pFert))+
  geom_boxplot(outlier.shape=NA)+ geom_jitter(aes(size = nEggs), width = 0.3, alpha = .3) +
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = element_blank(), y="Percent Fertilized per Egg Mass") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position= "top",legend.text=element_text(size=12),
        legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))

#Old version of Fig 5C
ggplot(data=isobelow10, aes(x=animal, y=pFert, color = categ))+
  geom_boxplot(outlier.shape=NA)+ geom_jitter(aes(size = nEggs), width = 0.3, alpha = .2) +
  theme_classic()+ylim(0,100)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = element_blank(), y="Percent Fertilized per Egg Mass") + 
  scale_x_discrete(limits = c("","A2","A3","D2","D3","","B1","B6","C1","C2","","T4","T5","W6","","T6","W1","W3","W4"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position= "top",legend.text=element_text(size=12),
        legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))


## Graph not used in paper, but data is 
## To make our data equivalent to Rutowski 1983 Table 8 I believe 
## Table on line 196
## "forno" means fertilized or not, "fert" means fertilized, "anotfert" meants not
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

## Here's the table, mentioned in the paper about 14 egg masses
table

## Here's the graph, not in the paper
ggplot(data=table, aes(fill=forno, x=whorln, y=n))+
  #geom_point(stat="identity",size = 2) +
  geom_bar(stat="identity", position = "fill")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top",legend.text=element_text(size=12),
        legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))

## This was a side project to determine if there was a critical point at 
##  which the linear regression had a significant slope 
##  (spoiler alert: its the ~13th egg mass in both directions)
x<-lm(pFert~whorln, data=isolargexlarge[isolargexlarge$whorln<5,])
summary(x)
names(summary(x))
str(summary(x))
pf(5.959436,1,10,lower.tail = F)


fstattable<- data.frame(1:30,0,0)
fstattable
for(i in 2:30){
  x<-lm(pFert~whorln, data=isolargexlarge[isolargexlarge$whorln<=i,])
  fstattable[i,2]<-pf(summary(x)$fstatistic[1],summary(x)$fstatistic[2],summary(x)$fstatistic[3],lower.tail = F)
  y<-lm(pFert~whorln, data=isolargexlarge[isolargexlarge$whorln>=i,])
  fstattable[i,3]<-pf(summary(y)$fstatistic[1],summary(y)$fstatistic[2],summary(y)$fstatistic[3],lower.tail = F)
  
}

fstattable

summary(x)$fstatistic[1]

## This is not in the paper
## To see how many each animal laid, not sure if it's even valid
isoferts$whorln
whorlns<-0
for (i in 2:length(isoferts$whorln)){
  if(isoferts$whorln[i]==1&&isoferts$pFert[i-1]>0){
    whorlns<-c(whorlns,isoferts$whorln[i-1])
  }
}
whorlns
mean(whorlns)
sd(whorlns)


whorlns<-c(38,5,3,4,21,23,9,10,20,19,27,18,19,15,11)
sort(whorlns)




## Not in paper
## Essentially Fig 4C with egg masses grouped by first 5, next 5, etc.
isobindata <- read.csv('ABCDTUV Isolation 2 with bin.csv')[1:306,1:13]
isobindata<-isobindata[(isobindata$categ=='LA'|isobindata$categ=='LL'|isobindata$categ=='AA'|isobindata$categ=='AL'),]

ggplot(data=isobindata[isobindata$categ=='LL',], aes(x=whorln, y=pFert, color = categ))+
  geom_boxplot(aes(group = whorlnbin),outlier.shape=NA)+ geom_point(aes(size = nEggs), alpha = .3) +
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = "Egg Mass Order", y="Percent Fertilized per Egg Mass") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position= "top",legend.text=element_text(size=12),
        legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))

## Anova and Tukey test on the bins from prev graph
z<-aov(pFert~binletter, data = isobindata[isobindata$categ=='LL',])
Anova(z)
TukeyHSD(z)

## Not in paper, for doing quadratic regression on isolargexlarge
## Basically the exact same as a linear regression lol
fourc<-ggplot(data=isolargexlarge, aes(x=whorln, y=pFert/100))+
  stat_smooth(method="lm", se=F,show.legend = T, color = "#619CFF")+
  geom_point(color = "#619CFF", size = 2, alpha = 0.4)+#aes(size = nEggs),stat="identity", width = .15, alpha = 0.3, color = "#619CFF")+
  theme_classic()+xlim(1,30)+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Egg Mass Number", y = "% Eggs Fertilized per Egg Mass    ")+
  scale_size(limits = c(1,800), range= c(0,5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position='none',legend.text=element_text(size=12),
        legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",colour ="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))#+
#scale_colour_manual(values = "#619CFF", breaks = c("LL"),
#      labels = c("LJ x LJ"), name = "Sizes of\nMating Pair")

whorlz<-isolargexlarge$whorln
whorl2<-(isolargexlarge$whorln)^2
quadratic<-lm(isolargexlarge$pFert~whorlz + whorl2)
summary(quadratic)
summary(lm(pFert~whorln, data=isolargexlarge))

  # Here's the graph with linear and quadratic regressions
fourc + geom_smooth(method = "lm", formula = y ~ x + I((x)^2), se = F, col = "#00BFC4")

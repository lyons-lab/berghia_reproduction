####################################
# Isolation Fertilization.R
# Written by: Neville Taraporevala
# Last Updated by: Jessica A. Goodheart
# Last Updated: 20 May 2022
# Purpose: The main goal of this one is to be for the animals isolated at 12 wpf and the egg masses
#          they laid over the full course of their lives
# Inputs used: '../CSV Files/Isolation Fertilization.csv'
####################################

####################################
# Initial setup
####################################
#Load packages for plotting and Anovas
require(ggplot2)
require(car)

#Set working directory
#setwd('C:/Users/ntara/Documents/Berghia Excel Files')
setwd("/Users/jessicagoodheart/Dropbox (Personal)/Research/4Projects/berghia_repro_neville/Raw Data Files/R Scripts")

####################################
# ALL Data from Age-Based Experiment 
####################################
# Define datasets including e,f,g,p which correspond to animals
#  isolated at 8(e),10(f),12(g) wpf and Never isolated or Pairs(p)
#  noe is all those without e, first is the first egg mass laid by each animal
f.df <- read.csv('../CSV Files/Isolation Fertilization.csv')[1:60,1:11]
f_iso <-f.df[f.df$wpfnev==8|f.df$wpfnev==10|f.df$wpfnev==12,]
f_e <- f.df[f.df$wpfnev==8,]
f_f <- f.df[f.df$wpfnev==10,]
f_g <- f.df[f.df$wpfnev==12,]
f_p <- f.df[f.df$wpfnev=="Never",]
f_noe <- f.df[f.df$wpfnev==10|f.df$wpfnev==12|f.df$wpfnev=="Never",]

## This is the info that shows how a quadratic is better fitting of 4B than a linear regression
summary(lm(pFert~whorln+Animal+whorln:Animal,data= f_g))
whorlz<-f_g$whorln
whorl2<-(f_g$whorln)^2
quadratic<-lm(pFert~whorlz+whorl2+Animal+whorlz:Animal+whorl2:Animal, data=f_g)
summary(quadratic)

## This a precursor for Fig 4B is for the 12 wpf animals that increase then decrease
ggplot(data=f_g, aes(x=whorln, y=pFert, color=as.factor(Animal)))+
  geom_point(aes(size = nEggs), stat="identity", alpha = 0.3) +
  theme_classic()+ ylim(0,100)+xlim(1,26.3)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Egg Mass Number", y="Percent Eggs Fertilized") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.position= "right", #c(0.15,0.8),
    legend.text=element_text(size=12),
    legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
    legend.background = element_rect(fill="white", size=0.5, linetype="solid",colour ="black"),
    axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+
  scale_shape_manual(values = c(16,17),
    breaks = c("G1","G3"),
    labels = c("Individual 1","Individual 2"),
    name = "Isolated at 12 wpf")+ 
  geom_smooth(method = "lm", formula = y ~ x + I((x)^2), se = F)

## Not in the paper

## Uses data from first 2 and last 2 whorls in the data set and how that changed over time
## Attempt to shorten Fig 4A-B but it's unnecessary 
fert_plot_firstlast<-ggplot(data=f_noe,aes(x=forlthree, y=pFert, color= wpfn))+
  geom_point(stat="identity",size = 3) + theme_classic()+
  #ggtitle("Percent Eggs Fertilized by Whorl Order")+
  ylim(0,100)+ scale_x_discrete(limits = c("1","2"), labels=c("First","Last"))+
  stat_smooth(method="lm", se=F,show.legend = T)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = element_blank(), y="Percent Fertilized") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="right",legend.text=element_text(size=12),
        legend.box = "horizontal",
        legend.title=element_text(size=12), 
        legend.background = element_rect(fill="white",
            size=0.5, linetype="solid",colour ="black"),
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))
fert_plot_firstlast+scale_colour_manual(values = c("#7CAE00", "#00BFC4","#F8766D"),
            breaks = c("10 wpf","12 wpf",">16 wpf"),
            labels = c("10","12","Never"),
            name = "Time of Isolation (w.p.f.)")

## Never used this graph, just a fun random line graph of each individual
ggplot(data=f.df,aes(x=whorln, y=pFert, color=Animal))+
  geom_point(stat="identity",size = 2) +
  theme_classic()+ geom_line()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Whorl Number", y="Percent Fertilized") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="right",legend.text=element_text(size=12),
        legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))

## Precursor to precursor of Fig. 4A
## Dot plot of 10,12,never over time shows all the percentages by whorl order
fert_plot_fitline<-ggplot(data=f_noe,
                          aes(x=whorln, y=pFert, color= wpfn
                          ))+
  geom_point(stat="identity",size = 3) +
  theme_classic()+ xlim(1,15)+ylim(0,100)+
  #scale_x_discrete(limits = c("1","","","4","","","7","","","10","","","13"))+
  #stat_smooth(method="lm", se=F,show.legend = T)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Whorl Order", y="Percent Fertilized") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(0.85, 0.19),legend.text=element_text(size=12),
        legend.box = "horizontal",
        legend.title=element_text(size=12), 
        legend.background = element_rect(fill="white",
          size=0.5, linetype="solid",colour ="black"),
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))
fert_plot_fitline+scale_colour_manual(values = c("#7CAE00", "#00BFC4","#F8766D"),
        breaks = c("10 wpf","12 wpf",">16 wpf"),
        labels = c("10","12","Never"),
        name = "Time of Isolation (w.p.f.)")


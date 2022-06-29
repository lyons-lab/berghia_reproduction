## Data from 5-week period of age-based experiment

## Load packages for plotting and Anovas
require(ggplot2)
require(car)

#Set working directory
#setwd('C:/Users/ntara/Documents/Berghia Excel Files')
setwd("/Users/jessicagoodheart/Dropbox (Personal)/Research/4Projects/berghia_repro_neville/Raw Data Files/R Scripts")

# Define datasets including e,f,g,p which correspond to animals
#  isolated at 8(e),10(f),12(g) wpf and Never isolated or Pairs(p)
#  noe is all those without e, first is the first egg mass laid by each animal
fert.df <- read.csv('../CSV Files/Isolation Fertilization Experiment.csv')[1:57,1:10]
fert_stack <- read.csv('../CSV Files/Isolation Fertilization Experiment.csv')[1:12,12:14]
fert_iso <-fert.df[fert.df$wpfnev==8|fert.df$wpfnev==10|fert.df$wpfnev==12,]
fert_e <- fert.df[fert.df$wpfnev==8,]
fert_f <- fert.df[fert.df$wpfnev==10,]
fert_g <- fert.df[fert.df$wpfnev==12,]
fert_p <- fert.df[fert.df$wpfnev=="Never",]
fert_noe <- fert.df[fert.df$wpfnev==10|fert.df$wpfnev==12|fert.df$wpfnev=="Never",]
fert_first<-fert.df[fert.df$whorln==1,]

## These show the mean and standard deviation of fertilization of egg masses laid by animals 
##  isolated @10 wpf (fert_f), 12 wpf (fert_g), and Never isolated (fert_p)
c(mean(fert_f$pFert), sd(fert_f$pFert))
c(mean(fert_g$pFert), sd(fert_g$pFert))
c(mean(fert_p$pFert), sd(fert_p$pFert))

## These are the linear models used to determine if there was a significant correlation
##  between the whorl order (whorln) and percent fertilization (pFert)
##  with the goal of showing how fertilization rate changed over time
## This was done for animals isolated @10 wpf (fert_f), 12 wpf (fert_g), and Never isolated (fert_p)
summary(lm(pFert~whorln+Animal, data=fert_f))
summary(lm(pFert~whorln+Animal, data=fert_g))
summary(lm(pFert~whorln+Animal, data=fert_p))

## This next test shows whether there is a difference between egg masses laid during the 5 week period by 
##  animals @10 wpf (fert_f), 12 wpf (fert_g), and Never isolated (fert_p) regardless of time
TukeyHSD(aov(pFert~wpfnev+Animal, data=fert_noe))

## this is using just the first egg mass from each animal that laid one
## this is just for fun to match the similar graph in Todd et al. 1997 
ggplot(data=fert_first,aes(x=wpf, y=pFert))+
  geom_point(stat="identity",size = 2) +theme_classic()+
  labs(x="Time of Isolation (wpf)", y="Percent Fertilized") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top",legend.text=element_text(size=12),
        legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))

## This is a precursor to Fig. 4A in the paper, mostly for me
ggplot(data=fert.df,aes(x=whorln, y=pFert, color= wpfn ))+
  #geom_point(stat="identity",size = 3) +
  #ggtitle("Percent Eggs Fertilized by Whorl Order")+
  theme_classic()+xlim(1,26.7)+ylim(0,100)+
  #scale_x_discrete(limits = c("1","","","4","","","7","","","10","","","13"))+
  stat_smooth(method="lm", se=F,show.legend = T)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Whorl Order", y="Percent Fertilized") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="right", #c(0.86, 0.23),
        legend.text=element_text(size=12),legend.box = "vertical", legend.title=element_text(size=12), 
        legend.background = element_rect(fill="white",size=0.5, linetype="solid",colour ="black"),
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+ 
  geom_jitter(aes(size = nEggs),width = .2, alpha = 0.3)+
  scale_colour_manual(values = c("#619CFF","#7CAE00", "#00BFC4","#F8766D"),
        breaks = c("8 wpf", "10 wpf","12 wpf",">16 wpf"),
        labels = c("8 wpf","10 wpf","12 wpf","Never"),
        name = "Time of Isolation")

## This is a precursor to Fig. 3F, again mostly for me
ggplot(data=fert.df,aes(x=wpfnev, y=pFert, color=wpfnev))+
  geom_boxplot()+  #geom_point(stat="identity",size = 2) +
  theme_classic()+
  #ggtitle("Percent Eggs Fertilized by Whorl Order in \n Animals Fertilized at Different Timepoints")+
  scale_x_discrete(limits = c("8","10","12","Never"))+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Time of Isolation (w.p.f.)", y="Percent Fertilized") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",#legend.text=element_text(size=12),
        #legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+
  scale_colour_manual(values = c("#C77CFF","#7CAE00", "#00BFC4","#F8766D"),
  breaks = c("8", "10","12","Never"),
  name = "Time of Isolation (w.p.f.)") + geom_jitter(width = .2)

## This was an old graph showing how there is a switch from unfertilized to fertilized egg masses
## This would eventually be ignored, since Fig. 3F shows it better
ggplot(fert_stack, aes(fill=condition, y=number, x=tIso)) + 
  geom_bar(position="fill", stat="identity")+
  scale_x_discrete(limits = c("4","6","8","10","12","Never"))+
  scale_y_continuous(expand = c(0,0), labels = scales::percent)+
  xlab("Time of Isolation (w.p.f.)")+ylab("Proportion of Egg Masses")+
  ggtitle("")+
  scale_fill_manual(values =c("tomato1","steelblue1"),
      breaks = c("Not Fertilized", "Fertilized"), 
      labels = c("Not Fertilized", "Fertilized"), name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(), axis.line.y = element_line(colour = "gray"),
      axis.ticks.x = element_blank())+
  geom_text(aes(label = number, x = tIso, y = number), data = fert_stack, position = position_fill(vjust=.5))

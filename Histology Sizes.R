#setwd('C:/Users/ntara/Documents/Berghia Excel Files')
setwd("/Users/jessicagoodheart/Dropbox (Personal)/Research/4Projects/berghia_repro_neville/Raw Data Files/R Scripts")
laidsizes <- read.csv('../CSV Files/Histology Sizes.csv')[1:14,1:12]
prematesizes <- read.csv('../CSV Files/Histology Sizes.csv')[17:32,1:12]
head(prematesizes)



colos<- c("#7CAE00", "#00BFC4")
colors<- c("#C77CFF","#7CAE00", "#00BFC4","#F8766D")


#box plot of sizes
ggplot(data=laidsizes, aes(x=categ, y=avg5, color = categ))+
  geom_boxplot(outlier.shape=NA)+ geom_jitter(width = 0.2) +
  theme_classic()+#ylim(0,20)+
  #ggtitle("Percent Eggs Fertilized by Whorl Order in \n Animals Fertilized at Different Timepoints")+
  scale_x_discrete(limits = c("Small","Large"))+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = "Size Category", y="Length at First Laying (mm)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",#legend.text=element_text(size=12),
        #legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))
prematesizes
t.test(avg5~categ, data = prematesizes)
x<-aov(avg5~categ, data = prematesizes)

summary(x)
Anova(x)
TukeyHSD(x)

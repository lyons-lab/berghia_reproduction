## Code for Figures
## Code written by Neville Taraporevala
## All graphs were saved as a JPEG 702x351 (only 3F was 1053x351)

## Figure 1 "Staging system" has NO CODE associated with it
## Figure 2 "Exptl design" has NO CODE associated with it
## ----------------------------------------------
library(ggplot2)

## Figure 3 "Later fertilization" has the following code 
## For 3F:
setwd('C:/Users/ntara/Documents/Berghia Excel Files')
fert.df <- read.csv('Isolation Fertilization Experiment.csv')[1:99,1:12]

ggplot(data=fert.df,aes(x=wpfnev, y=pFert/100, color=wpfnev))+
  geom_boxplot()+theme_classic()+
  scale_x_discrete(limits = c("4","6","8","10","12","Never"))+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Time of Isolation (wpf)", y="Percent Eggs Fertilized per Egg Mass              ") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",legend.text=element_text(size=12),
        legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+
  geom_jitter(width = .2)+ scale_y_continuous(labels = scales::percent)+
  scale_colour_manual(values = c("gray","gray","#C77CFF","#7CAE00", "#00BFC4","#F8766D"), 
                      breaks = c("4","6","8", "10","12","Never"))




## Figure 4 "Fertilization rate" has the following code:
## This is 4A.
setwd('C:/Users/ntara/Documents/Berghia Excel Files')
fert.df <- read.csv('Isolation Fertilization Experiment.csv')[16:58,1:12]
colors4<- c("#C77CFF","#7CAE00", "#00BFC4","#F8766D")
colors3<-c("#7CAE00", "#00BFC4","#F8766D")
colors1<-c("white", "white","#F8766D")
colors2<-c("#7CAE00", "white","#F8766D")
ggplot(data=fert.df,aes(x=whorln, y=pFert/100, color= wpfn))+
  theme_classic()+xlim(0,30)+
  #scale_x_continuous(limits = c(1,30), breaks = c(0,10,20,), labels = c(0,10,20))+#   xlim(1,26.7)+ #with legend on right
  stat_smooth(method="lm", se=F,show.legend = T)+
  labs(x="Egg Mass Number", y="% Eggs Fertilized per Egg Mass    ") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none", #c(0.86, 0.5),
        legend.text=element_text(size=12),
        legend.box = "vertical",
        legend.title=element_text(size=12), 
        legend.background = element_rect(fill="white",
            size=0.5, linetype="solid",colour ="black"),
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+ 
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  geom_point(size = 2, alpha = 0.4)+#aes(size = nEggs), alpha = 0.3)+ 
  scale_size(limits = c(1,800), range= c(0,5), name = c("# of Eggs"))+
  scale_colour_manual(values = colors3,
                      breaks = c("10 wpf","12 wpf",">16 wpf"),
                      labels = c("10 wpf","12 wpf","Never"),
                      name = "Time of Isolation")

## This is 4B.
## This is for G1 and G3 going up then down
setwd('C:/Users/ntara/Documents/Berghia Excel Files')
f_g <- read.csv('Isolation Fertilization.csv')[12:42,1:10]

ggplot(data=f_g, aes(x=whorln, y=pFert/100))+
  geom_point(color = "#00BFC4", size = 2, alpha = 0.4)+#aes(size = nEggs), stat="identity", alpha = 0.3, color = "#00BFC4") +
  theme_classic()+xlim(1,30)+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Egg Mass Number", y="% Eggs Fertilized per Egg Mass    ") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position= "none", #c(0.85,0.7),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
        legend.background = element_rect(fill="white",
              size=0.5, linetype="solid",colour ="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+
  scale_size(limits = c(1,800), range= c(0,5))+
  scale_shape_manual(values = c(16,17),
        breaks = c("G3","G1"))+ 
  geom_smooth(method = "lm", formula = y ~ x + I((x)^2), se = F, col = "#00BFC4")#+
  #scale_colour_manual(values = "#00BFC4", breaks = c("12 wpf"),
  #  labels = c("12 wpf"), name = "Time of Isolation")


## This is 4C.
setwd('C:/Users/ntara/Documents/Berghia Excel Files')
isodata <- read.csv('ABCDTUV Isolation 2.csv')[1:306,1:11]
isolargexlarge <-isodata[isodata$categ=='LL',]

#fourc<-
ggplot(data=isolargexlarge, aes(x=whorln, y=pFert/100))+
  #stat_smooth(method="lm", se=F,show.legend = T, color = "#619CFF")+
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


## This is for Fig 5A all compare
setwd('C:/Users/ntara/Documents/Berghia Excel Files')
isodata <- read.csv('ABCDTUV Isolation 2.csv')[1:306,1:11]

ggplot(data=isodata, aes(x=categ, y=pFert/100, color = group))+
  geom_boxplot(outlier.shape=NA)+ geom_jitter(aes(size=nEggs), width = 0.3, alpha = 0.3) +
  theme_classic()+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = "Mating Pairs", y="% Eggs Fertilized per Egg Mass    ") + 
  scale_size(limits = c(0,1600), name = "# of Eggs")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="right",legend.text=element_text(size=12),
        legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16)) +
  scale_x_discrete(limits = c("SS", "SA","AS","LL","LA", "AL","AA"),
        labels = c("SJ x SJ","SJ x A","A x SJ","LJ x LJ","LJ x A","A x LJ","A x A"))+
  scale_colour_manual(values = c("#00BA38","#619CFF","#FF61C3","white"),
        breaks = c("S","L","A","J"),
        labels = c("SJ = Small Juvs", "LJ = Large Juvs","A = Adults",""),
        name = "Animal Laying\nEgg Mass")


## This is for Fig 5B below10 compare LJs and As
setwd('C:/Users/ntara/Documents/Berghia Excel Files')
isodata <- read.csv('ABCDTUV Isolation 2.csv')[1:306,1:11]
isobelow10<-isodata[isodata$whorln<11&(isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL'),]

ggplot(data=isobelow10, aes(x=categ, y=pFert/100, color = group))+
  geom_boxplot(outlier.shape=NA)+ geom_jitter(aes(size = nEggs), width = 0.3, alpha = .3) +
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = "Mating Pairs", y="% Eggs Fertilized per Egg Mass     ") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  scale_size(limits = c(0,1600))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position= "none",legend.text=element_text(size=12),
        legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+
  #labs(size = "SJ = 6 mm Juv\nLJ = 9 mm Juv\nJ = Juvenile\nA = Adult\n\n# of Eggs")+
  scale_x_discrete(limits = c("LL", "LA","AL","AA"),
                   labels = c("LJ x LJ","LJ x A","A x LJ","A x A"))+
  scale_colour_manual(values = c("#619CFF","#FF61C3"),
                      breaks = c("L","A"),
                      labels = c("Large Juvs","Adults"))


## This is for Fig 5C individual variation
setwd('C:/Users/ntara/Documents/Berghia Excel Files')
isodata <- read.csv('ABCDTUV Isolation 2.csv')[1:306,1:11]
isobelow10<-isodata[isodata$whorln<11&(isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL'),]

ggplot(data=isobelow10, aes(x=animal, y=pFert/100, color = group))+
  geom_boxplot(outlier.shape=NA)+ geom_jitter(aes(size = nEggs), width = 0.3, alpha = .2) +
  theme_classic()+scale_size(limits = c(0,1600))+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(color = "Animal Laying\nEgg Mass",size = "Number \nof Eggs", x = "Mating Pairs", y="% Eggs Fertilized per Egg Mass     ") + 
  scale_x_discrete(limits = c("T6","W1","W3","W4","","T4","T5","W6","","","B1","B6","C1","C2","","A2","A3","D2","D3"),
                   labels = c("","         LJ x LJ","","","",""," LJ x A","","","","","         A x LJ","","","","","         A x A","",""))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position= "none",legend.text=element_text(size=12),
        legend.title=element_text(size=12), 
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+
  scale_colour_manual(values = c("#619CFF","#FF61C3"),
        breaks = c("L","A"),
        labels = c("Large Juvs","Adults"))


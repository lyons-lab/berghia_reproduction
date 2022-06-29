####################################
# Figures.R
# Written by: Neville Taraporevala
# Last Updated by: Jessica A. Goodheart
# Last Updated: 23 May 2022
# Purpose: The main goal of this one is to be for the animals isolated at 12 wpf and the egg masses
#          they laid over the full course of their lives
# Inputs used: '../CSV Files/Isolation Fertilization.csv'
####################################

####################################
# Initial setup
####################################
## Code for Stats and Graphs for Figures
## Code for Graphs for Figures begins on Line 135
## All graphs were saved as a JPEG 702x351 (only 2F was 1053x351)

# Load packages 
require(ggplot2)
require(car)
require(emmeans)
require(multcomp)

# Set working directory
#setwd('C:/Users/ntara/Documents/Berghia Excel Files')
#setwd("/Users/jessicagoodheart/Dropbox (Personal)/Research/4Projects/berghia_repro_neville/Raw Data Files/R Scripts")

# Set the prefixes for each output file name
agePrefix <- "AgeExp_"
sizePrefix <- "SizeExp_"

####################################
# Age-Based Experiment Analysis
####################################
## These show the mean and standard deviation of fertilization of egg masses laid by animals 
## isolated @10 wpf (fert_f), 12 wpf (fert_g), and Never isolated (fert_p)
## Fig 2F
fert.df <- read.csv('../CSV Files/Isolation Fertilization Experiment.csv')[1:57,1:10]

## 10 wpf
fert_f <- fert.df[fert.df$wpfnev==10,]
fert_f_meanall <- c(mean(fert_f$pFert), sd(fert_f$pFert))

# By individual animal
fert_f_ind.m <- aggregate(pFert~Animal, fert_f, mean)
fert_f_ind.sd <- aggregate(pFert~Animal, fert_f, sd)
fert_f_ind <- cbind(fert_f_ind.m, fert_f_ind.sd$pFert)
colnames(fert_f_ind) <- c("Animal", "pFert.mean", "pFert.sd")
fert_f_ind.df <- rbind(fert_f_ind, paste=c(NA, mean(fert_f_ind$pFert.mean, na.rm=TRUE), sd(fert_f_ind$pFert.mean, na.rm=TRUE)))
write.table(fert_f_ind.df, paste("../table_outputs/", agePrefix,"percFert_10wpf.tab"), sep="\t", quote=FALSE, row.names=FALSE, na="-")

## 12 wpf
fert_g <- fert.df[fert.df$wpfnev==12,]
fert_g_meanall <- c(mean(fert_g$pFert), sd(fert_g$pFert))

# By individual animal
fert_g_ind.m <- aggregate(pFert~Animal, fert_g, mean)
fert_g_ind.sd <- aggregate(pFert~Animal, fert_g, sd)
fert_g_ind <- cbind(fert_g_ind.m, fert_g_ind.sd$pFert)
colnames(fert_g_ind) <- c("Animal", "pFert.mean", "pFert.sd")
fert_g_ind.df <- rbind(fert_g_ind, paste=c(NA, mean(fert_g_ind$pFert.mean, na.rm=TRUE), sd(fert_g_ind$pFert.mean, na.rm=TRUE)))
write.table(fert_g_ind.df, paste("../table_outputs/", agePrefix,"percFert_12wpf.tab"), sep="\t", quote=FALSE, row.names=FALSE, na="-")

## Never isolated
fert_p <- fert.df[fert.df$wpfnev=="Never",]
fert_p_meanall <- c(mean(fert_p$pFert), sd(fert_p$pFert))

# By individual animal
fert_p_ind.m <- aggregate(pFert~Animal, fert_p, mean)
fert_p_ind.sd <- aggregate(pFert~Animal, fert_p, sd)
fert_p_ind <- cbind(fert_p_ind.m, fert_p_ind.sd$pFert)
colnames(fert_p_ind) <- c("Animal", "pFert.mean", "pFert.sd")
fert_p_ind.df <- rbind(fert_p_ind, paste=c(NA, mean(fert_p_ind$pFert.mean, na.rm=TRUE), sd(fert_p_ind$pFert.mean, na.rm=TRUE)))
write.table(fert_p_ind.df, paste("../table_outputs/", agePrefix,"percFert_neverIsol.tab"), sep="\t", quote=FALSE, row.names=FALSE, na="-")

## These are the linear models used to determine if there was a significant correlation
##  between the whorl order (whorln) and percent fertilization (pFert)
##  with the goal of showing how fertilization rate changed over time
## This was done for animals isolated @10 wpf (fert_f), 12 wpf (fert_g), and Never isolated (fert_p)
## Fig 3A
fert.sub <- subset(fert.df, !is.na(whorln))[-1,]
fert.glm <- glm(pFert/100~whorln*wpfnev+Animal, data=fert.sub)
fert.aov <- aov(fert.glm)
summary(fert.aov)
write.table(data.frame(unclass(summary(fert.aov))), paste("../table_outputs/", agePrefix,"percFert_glmANOVA.tab"), sep="\t", quote=FALSE, row.names=FALSE, na="-")

# R-squared
summary(lm(pFert/100~whorln*wpfnev+Animal, data=fert.sub))

## This is the info that shows how a quadratic is better fitting of 4B than a linear regression
## Fig 4B
f.df <- read.csv('../CSV Files/Isolation Fertilization.csv')[1:60,1:11]
f_g <- f.df[f.df$wpfnev==12,]

summary(lm(pFert~whorln+Animal,data=f_g))
whorlz<-f_g$whorln
whorl2<-(f_g$whorln)^2
linear <- lm(f_g$pFert~whorlz + f_g$Animal)
quadratic<-lm(f_g$pFert~whorlz + whorl2 + f_g$Animal)
summary(quadratic)

write.table(data.frame(unclass(summary(aov(quadratic)))), paste("../table_outputs/", agePrefix,"percFert_12wpf_quadratic.tab"), sep="\t", quote=FALSE, row.names=FALSE, na="-")

## This next test shows whether there is a difference between egg masses laid during the 5 week period by 
##  animals @10 wpf (fert_f), 12 wpf (fert_g), and Never isolated (fert_p) regardless of time
## Fig 3F
fert.df <- read.csv('../CSV Files/Isolation Fertilization Experiment.csv')[1:57,1:10]
fert_noe <- fert.df[fert.df$wpfnev==10|fert.df$wpfnev==12|fert.df$wpfnev=="Never",]

write.table(data.frame(unclass(summary(aov(quadratic)))), paste("../table_outputs/", agePrefix,"percFert_wpf_ANOVA.tab"), sep="\t", quote=FALSE, row.names=FALSE, na="-")

wpf_tukey <- TukeyHSD(aov(pFert~wpfnev*Animal, data=fert_noe))

####################################
# Size-Based Experiment Analysis
####################################
## Mean and standard deviation of below10 of each fert category (J9/LJs and As)
## Fig 4B
isodata <- read.csv('../CSV Files/ABCDTUV Isolation 2.csv')[1:306,1:11]
isoferts<-isodata[isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL',]
isobelow10<-isodata[isodata$whorln<11&(isodata$categ=='LA'|isodata$categ=='LL'|isodata$categ=='AA'|isodata$categ=='AL'),]
isobelow10largexlarge<-isobelow10[isobelow10$categ=='LL',]
isobelow10largexadult<-isobelow10[isobelow10$categ=='LA',]
isobelow10adultxlarge<-isobelow10[isobelow10$categ=='AL',]
isobelow10adultxadult<-isobelow10[isobelow10$categ=='AA',]

# Means for egg massess in different combinations
c(mean(isobelow10largexlarge$pFert), sd(isobelow10largexlarge$pFert))
c(mean(isobelow10largexadult$pFert), sd(isobelow10largexadult$pFert))
c(mean(isobelow10adultxlarge$pFert), sd(isobelow10adultxlarge$pFert))
c(mean(isobelow10adultxadult$pFert), sd(isobelow10adultxadult$pFert))

## Anova and TukeyHSD of the first 11 egg masses from all combinations
## Fig 4B
isobelow12<-isodata[isodata$whorln<12,]
below12.aov <-aov(pFert~categ+animal, data = isobelow12)
summary(below12.aov)
anova(below12.aov)
TukeyHSD(below12.aov)$categ

## Individual variation: Anova and TukeyHSD of the first 11 egg masses
## Fig 4C
isobelow12largest <- isobelow12[isobelow12$group=="L"|isobelow12$group=="A"&(isobelow12$mate=="L"|isobelow12$mate=="A"),]
animals <- unique(data.frame(isobelow12largest$animal, isobelow12largest$group))

# From the juveniles
isobelow12largest.L <- isobelow12largest[isobelow12largest$group=="L",]
leveneTest(pFert~animal, data = isobelow12largest.L)
zL<-kruskal.test(pFert~animal, data = isobelow12largest.L)
zL
dunnTest(pFert~animal, data = isobelow12largest.L)

# From the Adults
isobelow12largest.A <- isobelow12largest[isobelow12largest$group=="A",]
leveneTest(pFert~animal, data = isobelow12largest.A)
zA<-kruskal.test(pFert~animal, data = isobelow12largest.A)
zA
dunnTest(pFert~animal, data = isobelow12largest.A)

## To make our data equivalent to Rutowski 1983 Table 8 I believe 
## Table on line 127
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
## Here's the table, mentioned in the paper about how fertilization
## goes below 50% at the 14th egg mass
table

# Print  
write.table(table, paste("../table_outputs/", sizePrefix,"percFert_avg_whorlnumber.tab"), sep="\t", quote=FALSE, row.names=FALSE, na="-")

####################################
# Code for Figures 
####################################
## Figure 1 "Exptl design" has NO CODE associated with it
## ----------------------------------------------

## Figure 2 "Later fertilization" has the following code 
## For 2F: geom_point
fert.df <- read.csv('../CSV Files/Isolation Fertilization Experiment.csv')[1:99,1:12]
fert.df.agg <- aggregate(pFert~Animal+wpfnev, fert.df, FUN=mean)

fig2F <- ggplot(data=fert.df.agg,aes(x=wpfnev, y=pFert/100, color=wpfnev))+
  geom_boxplot()+theme_classic()+
  scale_x_discrete(limits = c("4","6","8","10","12","Never"))+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Time of Isolation (wpf)", y="% Eggs Fertilized per Egg Mass \n per Individual") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",legend.text=element_text(size=12),
        legend.title=element_text(size=12), 
        axis.text=element_text(size=36,colour="black"),
        axis.title=element_text(size=36),plot.title = element_text(face="bold", size=16))+
  scale_y_continuous(labels = scales::percent) + #,limits=c(0,1)
  geom_jitter(width = .2, aes(size=12)) +
  scale_colour_manual(values = c("gray","gray","#C77CFF","#7CAE00", "#00BFC4","#F8766D"), 
                      breaks = c("4","6","8", "10","12","Never"))

# Print png of figure
png("../figure_images/Figure_2F_mpl.png", width=2085, height=695)
fig2F
dev.off()

pdf(file="../figure_images/Figure_2F_mpl.pdf", width=20.85, height=8)
fig2F
dev.off()

## Figure 3 "Fertilization rate" has the following code:

# specify colours for plots
colors4<- c("#C77CFF","#7CAE00", "#00BFC4","#F8766D")
colors3<-c("#7CAE00", "#00BFC4","#F8766D")
colors1<-c("white", "white","#F8766D")
colors2<-c("#7CAE00", "white","#F8766D")

## This is 3A.
# Predict lines based on glm for plotting
fert.sub <- subset(fert.df, !is.na(whorln))[-1,]
fert.sub$predglm <- predict.glm(glm(pFert/100~whorln*wpfnev, data=fert.sub))

# Plot

fig3A <- ggplot(data=fert.sub,aes(x=whorln, y=pFert/100, color= factor(wpfn)))+
  theme_classic()+xlim(0,30)+
  geom_line(aes(y = predglm), size = 1) +
  scale_linetype_manual(values = rep(c("solid","dashed"),3)) + 
  labs(x="Egg Mass Number", y="% Eggs Fertilized per Egg Mass    ") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(0.9,0.8), 
        legend.text=element_text(size=12),
        legend.box = "vertical",
        legend.title=element_text(size=12), 
        #legend.background = element_rect(fill="white",
        #    size=0.5, linetype="solid",colour ="black"),
        axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+ 
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  geom_point(size = 2, alpha = 0.4)+
  scale_size(limits = c(1,800), range= c(0,5), name = c("# of Eggs"))+
  scale_colour_manual(values = colors3,
                      breaks = c("10 wpf","12 wpf",">16 wpf"),
                      labels = c("10 wpf","12 wpf","Never"),
                      name = "Time of\n Isolation")

# Print png of figure
png("../figure_images/Figure_3A.png", width=702, height=351)
fig3A
dev.off()

pdf(file="../figure_images/Figure_3A.pdf", width=7.02, height=3.51)
fig3A
dev.off()

## This is 3B.
## This is for G1 and G3 going up then down
f_g <- read.csv('../CSV Files/Isolation Fertilization.csv')[12:42,1:10]

# Plot
fig3B <- ggplot(data=f_g, aes(x=whorln, y=pFert/100, color=Animal))+
  geom_point(size = 2, alpha = 0.4)+
  theme_classic()+xlim(1,30)+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Egg Mass Number", y="% Eggs Fertilized per Egg Mass    ", colour="Animal ID") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(0.9,0.8) , #'none',
        legend.text=element_text(size=12),
        legend.title=element_text(size=12), axis.text=element_text(size=12,colour="black"),
        #legend.background = element_rect(fill="white",
         #     size=0.5, linetype="solid",colour ="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+
  scale_size(limits = c(1,800), range= c(0,5))+
  scale_color_manual(values = c("#00BFC4", "#00B0F6"),
                     breaks = c("G1","G3"))+
  scale_shape_manual(values = c(16,17),
                     breaks = c("G1","G3"),
                     labels = c("Individual 1","Individual 2"),
                     name = "Isolated at 12 wpf")+ 
  geom_smooth(method = "lm", formula = y ~ x + I((x)^2), se = F)
  
# Print png of figure
png("../figure_images/Figure_3B.png", width=702, height=351)
fig3B
dev.off()

pdf(file="../figure_images/Figure_3B.pdf", width=7.02, height=3.51)
fig3B
dev.off()

## This is Figure 3C.
# Pull in data
isodata <- read.csv('../CSV Files/ABCDTUV Isolation 2.csv')[1:306,1:11]
isolargexlarge <-isodata[isodata$categ=='LL',]

#-Plot
fig3C <-ggplot(data=isolargexlarge, aes(x=whorln, y=pFert/100, color=animal))+
  geom_point(size = 2, alpha = 0.4)+
  theme_classic()+xlim(1,30)+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Egg Mass Number", y = "% Eggs Fertilized per Egg Mass    ",colour="Animal ID")+
  scale_size(limits = c(1,800), range= c(0,5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(0.9,0.82),legend.text=element_text(size=12),
        legend.title = element_text(size=12), axis.text=element_text(size=12,colour="black"),
        #legend.background = element_rect(fill="white",
        #    size=0.5, linetype="solid",colour ="black"),
        axis.title=element_text(size=14),plot.title = element_text(face="bold", size=16))+
  geom_vline(xintercept=11, linetype="dashed", color = "red")

# Print png of figure
png("../figure_images/Figure_3C.png", width=702, height=351)
fig3C
dev.off()

pdf(file="../figure_images/Figure_3C.pdf", width=7.02, height=3.51)
fig3C
dev.off()

## Figure 4 "Mating tests" has the following code:
## This is for Fig 4A all compare
fig4A <- ggplot(data=isodata, aes(x=categ, y=pFert/100, color = group))+
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
        name = "Dam")

# Print png of figure
png("../figure_images/Figure_4A.png", width=702, height=351)
fig4A
dev.off()

pdf(file="../figure_images/Figure_4A.pdf", width=7.02, height=3.51)
fig4A
dev.off()

## This is for Fig 4B below12 compare LJs and As
fig4B <- ggplot(data=isobelow12largest, aes(x=categ, y=pFert/100, color = group))+
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
  scale_x_discrete(limits = c("LL", "LA","AL","AA"),
                   labels = c("LJ x LJ","LJ x A","A x LJ","A x A"))+
  scale_colour_manual(values = c("#619CFF","#FF61C3"),
                      breaks = c("L","A"),
                      labels = c("Large Juvs","Adults"))

# Print png of figure
png("../figure_images/Figure_4B.png", width=702, height=351)
fig4B
dev.off()

pdf(file="../figure_images/Figure_4B.pdf", width=7.02, height=3.51)
fig4B
dev.off()

## This is for Fig 4C individual variation
fig4C <- ggplot(data=isobelow12largest, aes(x=animal, y=pFert/100, color = group))+
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

# Print png of figure
png("../figure_images/Figure_4C.png", width=702, height=351)
fig4C
dev.off()

pdf(file="../figure_images/Figure_4C.pdf", width=7.02, height=3.51)
fig4C
dev.off()

############################################
# Supplementary Figure - Figure 2F with individuals data
fert.df.inds <- subset(fert.df, !is.na(dayn))
ggplot(data=fert.df.inds,aes(x=Animal, y=pFert/100, color=wpfnev))+
  geom_boxplot()+theme_classic()+
  #scale_x_discrete(limits = c("4","6","8","10","12","Never"))+
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


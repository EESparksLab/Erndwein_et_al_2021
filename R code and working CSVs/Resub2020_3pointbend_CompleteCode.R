#Resub2020_3-point bend paper_Complete Code
#Lindsay E

#Cleanup and LoadWorkspace
cat("\014") #clear working space
rm(list=ls()) #clean enviornment
ls() #checks to see if enviornment is clear
setwd("~/R")
getwd()

#Library load ####
library("ggplot2")
library("scales")
library("grid")
library("reshape2")
library("vegan")
library("goeveg")
library("effects")
library("PerformanceAnalytics")
library("car")
library("psych")
library("ggpubr")

#Mastersheet_Resub2020_Everything.csv - contains all genotypes and conditions data

#Mastersheet_Resub2020_Everythingmodfied.csv - is the same as above but has true MOI added 

#FIGURE 2 - Mechanical properties of B73 maize brace roots
data1 <- read.csv("Mastersheet_Resub2020_Everythingmodified.csv")
data3 <- subset(data1, Geno == "B73")
    #Change depending on condition
data2 <- subset(data3, Condition =="Dry Greenhouse")
data2 <- subset(data3, Condition =="Senesced Field")
data2 <- subset(data3, Condition =="Fresh Field")
head(data2) #first six lines of data

ggplot(data2, aes (WR, K, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.02), size = .75, colour = "black")+
  ylim(0,400)+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("K [N/mm^2]") + xlab(NULL)

ggplot(data2, aes (WR, UL, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.02), size = .75, colour = "black")+
  ylim(0,70)+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("UL [N]") + xlab(NULL)

ggplot(data2, aes (WR, BL, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.02), size = .75, colour = "black")+
  ylim(0,60)+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("BL [N]") + xlab(NULL)

    #Statistics - Change factor for  K, UL, BL
    #Remove stage for senesced field and fresh field

data1 <- read.csv("Mastersheet_Resub2020_Everything.csv")
data3 = subset(data2, Geno = "B73") 
data2 = subset(data3, Condition =="Dry Greenhouse")
#Change depending on condition and genotype

test1<-aov(K~Stage/Plant.ID/WR, data=data2) #Dry greenhouse only

test1<-aov(K~Plant.ID/WR, data=data2) #all other conditions
anova(test1)

test1<-aov(K~WR, data=data2) #Tukey post hoc  Test 
t1<-TukeyHSD(test1)
t1
describe(data2$K,type=2)

data2 = subset(data3, Condition =="Senesced Field") #changing condition

    #Statistics - Change factor for  K, UL, BL
    #Remove stage for senesced field and fresh field
test1<-aov(K~Plant.ID/WR, data=data2)
test1<-aov(K~Plant.ID/WR, data=data2)
test1<-aov(K~Stage/Plant.ID/WR, data=data2)
anova(test1)
test1<-aov(K~WR, data=data2)
t1<-TukeyHSD(test1)
t1
describe(data2$K,type=2)

data2 = subset(data3, Condition =="Fresh Field")
head(data2) #first six lines of data

    #Statistics - Change factor for  K, UL, BL
    #Remove stage for senesced field and fresh field
test1<-aov(K~Plant.ID/WR, data=data2)
test1<-aov(K~Plant.ID/WR, data=data2)
test1<-aov(K~Stage/Plant.ID/WR, data=data2)
anova(test1)
test1<-aov(K~WR, data=data2)
t1<-TukeyHSD(test1)
t1
describe(data2$K,type=2)

#FIGURE 3 - Diameter 
ggplot(data2, aes (WR, MajorD, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.02), size = .75, colour = "black")+
  ylim(0,7)+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("Major D [mm]") + xlab(NULL)

ggplot(data2, aes (WR, MinorD, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.02), size = .75, colour = "black")+
  ylim(0,7)+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("Minor D [mm]") + xlab(NULL)

#Statistics - Change factor for MinorD and MajorD
test1<-aov(MinorD~Stage/Plant.ID/WR, data=data2)
anova(test1)
test1<-aov(MinorD~WR, data=data2)
t1<-TukeyHSD(test1)
t1
describe(data2$MinorD,type=2)

#FIGURE S2 - CT vs. caliper correlations
data1 <- read.csv("MastersheetV2_Resub2020_OverlapErrorDiscarded_P6P7Discarded.csv")
data2 = subset(data1, Section =="S1")
head(data2) #first six lines of data
colnames(data1)
str(data1)
data2 = data1[,c(16,21,17,20)] #picks out columns
chart.Correlation(data2, histogram = TRUE, method = "pearson", pch=1) #correlationchart

#FIGURE S3 - Wall Thickness
ggplot(data2, aes (WR, MajorThicc2, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.02), size = .75, colour = "black")+
  ylim(0,3.1)+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("MajorThicc2 [mm]") + xlab(NULL)

ggplot(data2, aes (WR, MinorThicc2, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.02), size = .75, colour = "black")+
  ylim(0,3.1)+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("MajorThicc2 [mm]") + xlab(NULL)

    #Statistics - Change factor for MajorThicc2 and MinorThicc2
    #Remove stage for senesced field
test1<-aov(MajorThicc2~Stage/Plant.ID/WR, data=data2)
anova(test1)
test1<-aov(MajorThicc2~WR, data=data2)
t1<-TukeyHSD(test1)
t1
describe(data2$MajorThicc2,type=2)

#FIGURE 4 - Bending Moduli B73 Dry Greenhouse

data1 <- read.csv("Mastersheet_Resub2020_Everythingmodified.csv")
data3 <- subset(data1, Geno == "B73")
    #Change depending on condition
data2 <- subset(data3, Condition =="Dry Greenhouse")
data2 <- subset(data3, Condition =="Senesced Field")
data2 <- subset(data3, Condition =="Fresh Field")
head(data2) #first six lines of data

ggplot(data2, aes (WR, E_GPa_solid, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.02), size = .75, colour = "black")+
  ylim(0,11)+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("Bending Modulus [GPa]") + xlab(NULL)


    #Figure 4 Statistics - Remove stage for senesced field
test1<-aov(E_GPa_solid~Stage/Plant.ID/WR, data=data2)
anova(test1)
test1<-aov(E_GPa_solid~WR, data=data2)
t1<-TukeyHSD(test1)
t1
describe(data2$E_GPa_solid,type=2)

#EXTRA - Mechanical Properties Correlation Chart
Data <- read.csv("Mastersheet_Resub2020_Everything_RemovedVoids.csv") #Removed blanks spaces for correlation chart
data1 = subset(Data, Geno =="B73")
data2 = subset(data1, Condition =="Dry Greenhouse")
data2 = subset(data1, Condition =="Senesced Field")
data2 = subset(data1, Condition =="Fresh Field")
colnames(data2)
str(data2)
data2 = data2[,c(12,11,10,27)] #picks out columns
chart.Correlation(data2, histogram = TRUE, method = "pearson", pch=1) #correlationchart

#FIGURE 5 - Mechanical properties of with two other genotypes (A632 and Oh43)
Data <- read.csv("Mastersheet_Resub2020_Everything.csv")
data1 = subset(Data, Figure_5 =="Yes")
#9x12 pdf size

ggplot(data1, aes (WR, K, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, Condition), nrow = 2, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("K") + xlab(NULL)

ggplot(data1, aes (WR, UL, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, Condition), nrow = 2, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("UL") + xlab(NULL)

ggplot(data1, aes (WR, BL, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, Condition), nrow = 2, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("BL") + xlab(NULL)

ggplot(data1, aes (WR, E_GPa_solid, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, Condition), nrow = 2, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("E [GPa]") + xlab(NULL)

ggplot(data1, aes (WR, E_GPa_solid, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, Condition), nrow = 2, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("E (solid)") + xlab(NULL)

#Figure 5 - Statistics
data2 = subset(data1, Geno =="A632") #Replace with Oh43
data3 = subset(data2, Condition =="Fresh Field") #Replace with Senesced Field

test1 <- aov(E_GPa_solid~Plant.ID/WR, data=data3)#replace with UL, BL, K, E_GPa
anova(test1)
t1 <- aov(E_GPa_solid~WR, data=data3)
tukey.t1 <- TukeyHSD(t1)
tukey.t1
describe(data3$E_GPa_solid,type=2)

#Figure S5
Data <- read.csv("Mastersheet_Resub2020_Everything.csv")
data1 = subset(Data, Figure_5 =="Yes")

ggplot(data1, aes (WR, MajorD, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, Condition), nrow = 2, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("MajorD") + xlab(NULL)

ggplot(data1, aes (WR, MinorD, group = WR)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, Condition), nrow = 2, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("MinorD") + xlab(NULL)

data2 = subset(data1, Geno =="A632") #Replace with Oh43
data3 = test1 <- aov(MajorD~Plant.ID/WR, data=data3)#replace with UL, BL, K, E_GPa
anova(test1)
t1 <- aov(MajorD~WR, data=data3)
tukey.t1 <- TukeyHSD(t1)
tukey.t1
describe(data3$K,type=2)
ubset(data2, Condition =="Fresh Field") #Replace with Senesced Field


#FIGURE 6 - Mechanical properties across conditions
Data <- read.csv("Mastersheet_Resub2020_Everything.csv")
data1 = subset(Data, Figure_6 =="Yes")
#12x10 pdf size

ggplot(data1, aes (Condition, K, group = Condition)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, WR), nrow = 3, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("K") + xlab(NULL)

ggplot(data1, aes (Condition, UL, group = Condition)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, WR), nrow = 3, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("UL") + xlab(NULL)


ggplot(data1, aes (Condition, BL, group = Condition)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, WR), nrow = 3, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("BL") + xlab(NULL)

ggplot(data1, aes (Condition, E_GPa_solid, group = Condition)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, WR), nrow = 3, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("E") + xlab(NULL)

#EXTRA - Diameter by Condition

ggplot(data1, aes (Condition, MajorD, group = Condition)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, WR), nrow = 3, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("MajorD") + xlab(NULL)

ggplot(data1, aes (Condition, MinorD, group = Condition)) + 
  geom_violin(fill = "grey", colour = "grey")+
  geom_jitter(position = position_jitter(width=0.03), size = 1, colour = "black")+
  facet_wrap(vars(Geno, WR), nrow = 3, ncol = 2, scales = "fixed", shrink = TRUE, drop = TRUE, dir = "h", strip.position = c("top"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=36, vjust=3), 
        axis.text=element_text(size=36), 
        axis.title = element_text(size=36), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 35, colour = "red")) +
  ylab("MinorD") + xlab(NULL)

#FIGURE 7 - Ls means, normalizing 2 Geno by B73

cat("\014")
rm(list=ls()) 
ls() 
setwd("~/R")
getwd()

install.packages("lsmeans")
install.packages("RColorBrewer")
library(lsmeans)
library(RColorBrewer)
# Only fresh and senesced genotypes
LE <- read.csv(file = "Mastersheet_Resub2020_FreshandSenesced_3Geno_Combined.csv", header = TRUE, strip.white=TRUE,na.strings = "NA")
attach(LE)
lsm <- {} #creates an empty 
trait <- {}
lslist <- {}
test <- {}
for (i in 11:13){ #run loop on data columns
  trait_name = colnames(LE)[i]
  lsm[[i]] <- lm(LE[,i] ~ Geno * WR * Condition, data = LE)
  lslist[[i]] <- lsmeans(lsm[[i]], ~Geno*WR*Condition)
  test[[i]] <- as.data.frame(lslist[[i]])
  names(test[[i]])[names(test[[i]]) == "lsmean"] = trait_name
}
lsm_comb <- cbind(test[[11]],test[[12]],test[[13]])
colnames(lsm_comb)
lsm_comb2 <- lsm_comb[,c(1,2,3,4,5,12,13,20,21)]
write.csv(lsm_comb2, file = "LSmeans_normfig7_new.csv", quote = F, row.names = F) #writes a CSV file
detach(LE)

cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "R")
getwd()

LE_lsm <- read.csv(file = "LSmeans_normfig7_new.csv", header = TRUE, strip.white=TRUE,na.strings = "NA")
library(ggplot2)
head(LE_lsm)
str(LE_lsm)
colnames(LE_lsm)

#save as 6x15 inch PDF

K_lsm = LE_lsm[,c(1:6)]
K_lsm$ID = paste(K_lsm$WR, K_lsm$Condition, sep="_")
k = ggplot(K_lsm, aes(x=ID, y=K_norm, color=Geno, shape = Geno)) + 
  geom_point(size=10) + 
  scale_color_manual(values = c("A632" = "indianred", "B73" = "palegreen4", "Oh43" = "cornflowerblue")) +
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=25), 
        axis.title = element_text(size=25), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("K_Norm") + xlab(NULL)
k

BL_lsm = LE_lsm[,c(1:3,7:9)]
BL_lsm$ID = paste(BL_lsm$WR, BL_lsm$Condition, sep="_")
BL = ggplot(BL_lsm, aes(x=ID, y=BL_norm, color=Geno, shape = Geno))+
  geom_point(size=10) +
  scale_color_manual(values = c("A632" = "indianred", "B73" = "palegreen4", "Oh43" = "cornflowerblue")) +
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("BL_Norm") + xlab(NULL)
BL

UL_lsm = LE_lsm[,c(1:3,10:12)]
UL_lsm$ID = paste(UL_lsm$WR, UL_lsm$Condition, sep="_")
UL = ggplot(UL_lsm, aes(x=ID, y=UL_norm, color=Geno, shape = Geno)) + 
  geom_point(size=10) +
  scale_color_manual(values = c("A632" = "indianred", "B73" = "palegreen4", "Oh43" = "cornflowerblue"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("UL_Norm") + xlab(NULL)
UL

E_lsm = LE_lsm[,c(1:3,13:15)]
E_lsm$ID = paste(E_lsm$WR, E_lsm$Condition, sep="_")
E = ggplot(E_lsm, aes(x=ID, y=E_GPa_norm, color=Geno, shape=Geno)) + 
  geom_point(size=10) + 
  scale_color_manual(values = c("A632" = "indianred", "B73" = "palegreen4", "Oh43" = "cornflowerblue"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=25), 
        axis.title = element_text(size=25), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("E_Norm") + xlab(NULL)
E

MinorD_lsm = LE_lsm[,c(1:3,19:21)]
MinorD_lsm$ID = paste(MinorD_lsm$WR, MinorD_lsm$Condition, sep="_")
MinorD = ggplot(MinorD_lsm, aes(x=ID, y=MinorD_norm, color=Geno, shape=Geno)) + 
  geom_point(size=10) + 
  scale_color_manual(values = c("A632" = "indianred", "B73" = "palegreen4", "Oh43" = "cornflowerblue"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("MinorD_Norm") + xlab(NULL)
MinorD

MajorD_lsm = LE_lsm[,c(1:3,16:18)]
MajorD_lsm$ID = paste(MajorD_lsm$WR, MajorD_lsm$Condition, sep="_")
MajorD = ggplot(MajorD_lsm, aes(x=ID, y=MajorD_norm, color=Geno, shape=Geno)) + 
  geom_point(size=10) + 
  scale_color_manual(values = c("A632" = "indianred", "B73" = "palegreen4", "Oh43" = "cornflowerblue"))+
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=24),
        plot.title=element_text(size=20, vjust=3), 
        axis.text=element_text(size=20), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"), strip.background = element_rect(fill="grey"), strip.text.x = element_text(size = 20, colour = "black"),strip.text.y = element_text(size = 20, colour = "black")) +
  ylab("MajorD_norm") + xlab(NULL)
MajorD

#FIGURE S6 - E/MOI_solid regressed by E/MOI_hollow
Data <- read.csv("Mastersheet_Resub2020_Everythingmodified.csv")
data1 = subset(Data, Geno == "B73")
data2 = subset(data1, Sample =="Dry")


MOI_hollow <- data2$MOI_hollow
MOI_solid <- data2$MOI_solid
cor(MOI_solid, MOI_hollow) #calculates r value

E_GPa_solid <- data2$E_GPa_solid 
E_GPa_hollow <- data2$E_GPa_hollow
cor(E_GPa_solid, E_GPa_hollow) #calculates r value

    #MOI_solid MOI_hollow
test1 <- cor.test (data2$MOI_solid, data2$MOI_hollow2, method = c("pearson"))

m=summary(glm(data2$MOI_hollow ~ data2$MOI_solid, data=data2))
b0 = coef(m)[1]
m$coefficients

m=summary(glm(data2$E_GPa_hollow ~ data2$E_GPa_solid, data=data2))
b0 = coef(m)[1]
m$coefficients

eq <- function(x,y) {
  m <- glm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

ggplot (data2, aes(x = MOI_solid, y = MOI_hollow))+
  geom_point(size = 1, colour = data2$Condcolor) +
  geom_smooth(method="glm", se = TRUE)+
  stat_regline_equation(label.y = 50, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 40, aes(label = ..rr.label..)) +
theme(axis.text.x = element_text(size=20),
      axis.text.y = element_text(size=20),
      plot.title=element_text(size=20, vjust=3),
      axis.text=element_text(size=20),
      axis.title = element_text(size=20),
      axis.title.y= element_text(vjust=2.5),
      axis.title.x= element_text(vjust=-1.4),
      axis.ticks.length = unit(.2,"cm"),
      strip.background = element_rect(fill="grey"),
      strip.text.x = element_text(size = 20, colour = "blue"),
      strip.text.y = element_text(size = 20, colour = "black"))

    #E hollow_Esolid
test1 <- cor.test (data2$E_GPa_solid, data2$E_GPa_hollow2, method = c("pearson"))

m=summary(glm(data2$E_GPa_solid ~ data2$E_GPa_hollow, data=data2))
b0 = coef(m)[1]
m$coefficients

m=summary(glm(data2$E_GPa_hollow ~ data2$E_GPa_solid, data=data2))
b0 = coef(m)[1]
m$coefficients

eq <- function(x,y) {
  m <- glm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

ggplot (Data, aes(x = E_GPa_solid, y = E_GPa_hollow))+
  geom_point(size = 1, colour = Data$Condcolor) +
  geom_smooth(method="glm", se = TRUE)+
  stat_regline_equation(label.y = 50, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 40, aes(label = ..rr.label..)) +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title=element_text(size=20, vjust=3),
        axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        axis.title.y= element_text(vjust=2.5),
        axis.title.x= element_text(vjust=-1.4),
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 20, colour = "blue"),
        strip.text.y = element_text(size = 20, colour = "black"))



Correl = lm(E_GPa_solid ~ E_GPa_hollow, data=data2)
summary(Correl)$r.squared 


Correl = lm(MOI_solid ~ MOI_hollow, data=data2)
summary(Correl)$r.squared 

ggplot (data, aes(x=E_GPa_solid, fill=Condition))

#FIGURE 8 - Bending moduli of maize brace roots compared to those of other maize organs

data <- read.csv("Mastersheet_Resub2020_Everythingmodified.csv")

ggplot (data, aes(x=E_GPa_solid, fill=Condition))+
  geom_density(alpha=0.3)+ #density plot
  xlim(-1, 23) + ylim(0,1.75) +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title=element_text(size=20, vjust=3),
        axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        axis.title.y= element_text(vjust=2.5),
        axis.title.x= element_text(vjust=-1.4),
        axis.ticks.length = unit(.2,"cm"),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size=20),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 20, colour = "blue"),
        strip.text.y = element_text(size = 20, colour = "black"))

ggplot (data, aes(x=E_GPa, fill=Condition))+
  geom_density(alpha=0.3)+
  xlim(0, 16) + ylim(0,1.) +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title=element_text(size=20, vjust=3),
        axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        axis.title.y= element_text(vjust=2.5),
        axis.title.x= element_text(vjust=-1.4),
        axis.ticks.length = unit(.2,"cm"),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size=20),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 20, colour = "blue"),
        strip.text.y = element_text(size = 20, colour = "black"))
library(psych)
data1 = subset(data, Condition =="Dry Greenhouse") #Change with condition
describe(data1$E_GPa_solid,type=2)


#EXTRA - Comparing Python and BLuehill

data2 <- read.csv("PythonBluehill_Comparison.csv")
                 
test1 <- cor.test (data2$BL_Blue, data2$BL_Py, method = c("pearson"))           
summary(test1)

data2 <- read.csv("PythonBluehill_Comparison.csv")
m=summary(glm(data2$UL_Blue ~ data2$UL_Py, data=data2))
b0 = coef(m)[1]
m$coefficients

ggplot (data2, aes(x = UL_Blue, y = UL_Py))+
  geom_point(size = 1, colour = "black") +
  geom_smooth(method="glm", se = TRUE)+
  stat_regline_equation(label.y = 50, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 40, aes(label = ..rr.label..)) +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title=element_text(size=20, vjust=3),
        axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        axis.title.y= element_text(vjust=2.5),
        axis.title.x= element_text(vjust=-1.4),
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 20, colour = "blue"),
        strip.text.y = element_text(size = 20, colour = "black"))


data2 <- read.csv("PythonBluehill_Comparison.csv")
m=summary(glm(data2$BL_Blue ~ data2$BL_Py, data=data2))
b0 = coef(m)[1]
m$coefficients

test1 <- cor.test (data2$BL_Blue, data2$BL_Py, method = c("pearson"))           
ggplot (data2, aes(x = BL_Blue, y = BL_Py))+
  geom_point(size = 1, colour = "black") +
  geom_smooth(method="glm", se = TRUE)+
  stat_regline_equation(label.y = 50, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 40, aes(label = ..rr.label..)) +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title=element_text(size=20, vjust=3),
        axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        axis.title.y= element_text(vjust=2.5),
        axis.title.x= element_text(vjust=-1.4),
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 20, colour = "blue"),
        strip.text.y = element_text(size = 20, colour = "black"))

#EXTRA - True MOI vs. hollow vs. solid

Data <- read.csv("MOIcomparison.csv")

    
ggplot(Data, aes(x=Geometry, y=MOI)) + 
  geom_dotplot(binaxis='y', stackdir='center') #dot plot


data2 <- read.csv("MOITrueRegression.csv") #regression 
m=summary(glm(data2$MOI_true ~ data2$MOI_solid, data=data2))

MOI_true <- data2$MOI_true 
MOI_solid <- data2$MOI_solid
cor(MOI_solid, MOI_true) #calculates R value

test1 <- cor.test (data2$MOI_true, data2$MOI_solid, method = c("pearson"))           
ggplot (data2, aes(x = MOI_true, y = MOI_solid))+
  geom_point(size = 1, colour = "black") +
  #geom_smooth(method="glm", se = TRUE)+
  ylim(0,25)+ xlim(0,25)+
 # stat_regline_equation(label.y = 50, aes(label = ..eq.label..)) +
#  stat_regline_equation(label.y = 40, aes(label = ..rr.label..)) +
   theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title=element_text(size=20, vjust=3),
        axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        axis.title.y= element_text(vjust=2.5),
        axis.title.x= element_text(vjust=-1.4),
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 20, colour = "blue"),
        strip.text.y = element_text(size = 20, colour = "black"))

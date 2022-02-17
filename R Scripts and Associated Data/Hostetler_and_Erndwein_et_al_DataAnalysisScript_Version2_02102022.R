#Script Associated with Hostetler_and_Erndwein_Version2_02102022.docx
library(tidyverse)
library(rcompanion) #Tukey transformations
library(agricolae) #TukeyHSD
library(cowplot) #Plotting
library(ggplot2) #Plotting
library(dplyr)
#Methods - Comparison of I####
#R6 Data
##FigureS3: The true second moment of area is correlated with the simplified second moment of area####
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashleyhostetler/Desktop/Hostetler_Erndwein_et_al_2021/R Scripts and Associated Data/")
getwd()
data = read.csv(file= "ProcessedData_2019_02102022.csv", header = TRUE, na.strings = "NA")
#Remove plants that only have 1 whorl of data
head(data)
x = data %>%    #identify how many unique 'ID's only appear once, suggesting there is only 1 observation for this plant, plot, rep
  group_by(ID) %>%
  dplyr::filter(n() == 1) 
x = as.data.frame(x)
x = x$ID
x = as.list(x)
data <- data[ ! data$ID %in% x, ] #remove rows from list (list was plants that only had 1 observation)

#Determine if truex is Major or Minor
data$MOItruex2 = ""
data$MOItruey2 = ""
for (i in 1:length(data$MOItruex)){
  if (data$MOItruex[i] > data$MOItruey[i]) {
    data$MOItruex2[i] = "Major"
  } else if (data$MOItruey[i] > data$MOItruex[i]) {
    data$MOItruex2[i] = "Minor"
  }
}
for (i in 1:length(data$MOItruex)){
  if (data$MOItruex[i] > data$MOItruey[i]) {
    data$MOItruey2[i] = "Minor"
  } else if (data$MOItruey[i] > data$MOItruex[i]) {
    data$MOItruey2[i] = "Major"
  }
}
data$MOI_Major = ""
data$MOI_Minor = ""
for (i in 1:length(data$MOItruex)){
  if (data$MOItruex2[i] == "Major") {
    data$MOI_Major[i] = data$MOItruex[i]
  } else if (data$MOItruex2[i] == "Minor") {
    data$MOI_Minor[i] = data$MOItruex[i]
  }
}
for (i in 1:length(data$MOItruex)){
  if (data$MOItruey2[i] == "Major") {
    data$MOI_Major[i] = data$MOItruey[i]
  } else if (data$MOItruey2[i] == "Minor") {
    data$MOI_Minor[i] = data$MOItruey[i]
  }
}

colnames(data)
data$MOItruex = NULL
data$MOItruey = NULL
data$MOItruex2 = NULL
data$MOItruey2 = NULL
data$MOI_True_AH = NULL
str(data)
data$MOI_Major = as.numeric(data$MOI_Major)
data$MOI_Minor = as.numeric(data$MOI_Minor)
data$MOI_True_AH = (data$MOI_Major + data$MOI_Minor)/2

###Correlations between True and Solid I ###
colnames(data)
FigS3A=ggplot(data, aes(x=MOI_solid_AH, y=MOI_Major)) + 
  geom_point(aes(colour = factor(Geno)))+
  geom_smooth(method=lm, color="black")+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  labs(x="I Solid Assumption",y="I Major True")
FigS3A
cor.test (data$MOI_solid_AH, data$MOI_Major, use = "everything", method = c("pearson"))

FigS3B=ggplot(data, aes(x=MOI_solid_AH, y=MOI_Minor)) + 
  geom_point(aes(colour = factor(Geno)))+
  geom_smooth(method=lm, color="black")+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  labs(x="I Solid Assumption",y="I Minor True")
FigS3B
cor.test (data$MOI_solid_AH, data$MOI_Minor, use = "everything", method = c("pearson"))

FigS3C=ggplot(data, aes(x=MOI_solid_AH, y=MOI_True_AH)) + 
  geom_point(aes(colour = factor(Geno)))+
  geom_smooth(method=lm, color="black")+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  labs(x="I Solid Assumption",y="I Average True")
FigS3C
cor.test (data$MOI_solid_AH, data$MOI_True_AH, use = "everything", method = c("pearson"))

FigureS3=plot_grid(FigS3A, FigS3B, FigS3C, labels = c('A', 'B', 'C'), nrow = 1)
FigureS3

pdf("FigureS3.pdf", width = 8.5, height = 4.5)
plot(FigureS3)
dev.off()
#Results####
###Table S5-S6: The bending modulus from the true I does not result in differences between whorls ####
#Convert MOI to Bending Modulus
head(data)
data$E_Mpa_Major_AH = ((data$K)*(17^3))/(48*data$MOI_Major)
data$E_GPa_Major_AH = ((data$E_Mpa_Major_AH)*0.001)

data$E_Mpa_Minor_AH = ((data$K)*(17^3))/(48*data$MOI_Minor)
data$E_GPa_Minor_AH = ((data$E_Mpa_Minor_AH)*0.001)

data$E_Mpa_True_AH = ((data$K)*(17^3))/(48*data$MOI_True_AH)
data$E_GPa_True_AH = ((data$E_Mpa_True_AH)*0.001)

####ANOVAs: Does the true bending modulus differ between whorls and genotypes? 
attach(data)
#E Major
lm_x <- lm(E_GPa_Major_AH ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$E_GPa_Major_AH)
hist(data$E_GPa_Major_AH)
shapiro.test(data$E_GPa_Major_AH)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data$tukey <- transformTukey(
  data$E_GPa_Major_AH,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
hist(data$tukey)
lm_x2 <- lm(data$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR"), console = TRUE)
HSD.test(lm_x2_aov2, trt = c("Geno"), console = TRUE)
data$tukey = NULL

#E Minor
lm_x <- lm(E_GPa_Minor_AH ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$E_GPa_Minor_AH)
hist(data$E_GPa_Minor_AH)
shapiro.test(data$E_GPa_Minor_AH)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data$tukey <- transformTukey(
  data$E_GPa_Minor_AH,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR"), console = TRUE)
HSD.test(lm_x2_aov2, trt = c("Geno"), console = TRUE)
data$tukey = NULL

#E Average
lm_x <- lm(E_GPa_True_AH ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$E_GPa_True_AH)
hist(data$E_GPa_True_AH)
shapiro.test(data$E_GPa_True_AH)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data$tukey <- transformTukey(
  data$E_GPa_True_AH,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR"), console = TRUE)
HSD.test(lm_x2_aov2, trt = c("Geno"), console = TRUE)
data$tukey = NULL
detach(data)

###Brace root whorls closer to the ground have a greater contribution to anchorage####
cat("\014")
rm(list=ls())
ls()
setwd(dir = "/Users/ashleyhostetler/Desktop/Hostetler_Erndwein_et_al_2021/R Scripts and Associated Data/")
getwd()
####Figure3A-B, TableS1-S2: Overall Contribution not broken down by whorl#####
data6 = read.csv(file = "/Users/ashleyhostetler/Desktop/Hostetler_Erndwein_et_al_2021/R/Inbred_Subpop_2Years_Broot_Ratio.csv", header = TRUE, na.strings = "NA")
data6 = data6[,c(1,3,5,6,12,13)]
head(data6)
data = subset(data6, Year=="2019")
A632 = subset(data, Accession=="A632")
B73 = subset(data, Accession=="B73")
Oh43 = subset(data, Accession=="Oh43")
data6=rbind(A632, B73, Oh43)

data6$Brace.Root.Whorls.in.the.Soil = as.character(data6$Brace.Root.Whorls.in.the.Soil)
data6$Accession = factor(data6$Accession,levels = c("B73","Oh43","A632"))

####ANOVAs: Does the number of whorls in the ground impact the None/All ratio?
attach(data6)
lm_x <- lm(Ratio..None.All....IMU ~ Brace.Root.Whorls.in.the.Soil*Accession)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Brace.Root.Whorls.in.the.Soil","Accession"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(1,1))
shapiro.test(data6$Ratio..None.All....IMU)
#transformation data to meet assumptions
data6$tukey <- transformTukey(
  data6$Ratio..None.All....IMU,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
hist(data6$tukey)
lm_x2 <- lm(data6$tukey ~ Brace.Root.Whorls.in.the.Soil*Accession)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("Brace.Root.Whorls.in.the.Soil"), console = TRUE)
HSD.test(lm_x2_aov2, trt = c("Accession"), console = TRUE)
data6$tukey = NULL
detach(data6)
####Figure Generation
data6$Accession = factor(data6$Accession,levels = c("B73","Oh43","A632"))
Fig2A = ggplot(data6, aes(x=Accession, y=Ratio..None.All....IMU, color=Accession)) +
  geom_jitter(width = 0.1, height = 0.1,size=2)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Accession",y="None/All Ratio")+
  theme(axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=17), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 15, colour = "blue"),
        strip.text.y = element_text(size = 15, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig2A

Fig2B = ggplot(data6, aes(x=Brace.Root.Whorls.in.the.Soil, y=Ratio..None.All....IMU, color=Accession)) +
  geom_jitter(width = 0.1, height = 0.1,size=2)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Brace Root WRs in Soil",y="None/All Ratio")+
  theme(axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=17), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 15, colour = "blue"),
        strip.text.y = element_text(size = 15, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig2B
####Figure3C, TableS1-S2: Individual whorl ratio####
data = read.csv(file = "/Users/ashleyhostetler/Desktop/Hostetler_Erndwein_et_al_2021/R/Inbred_Subpop_2Years_EI_All.csv", header = TRUE, na.strings = "NA")
data$ID = paste(data$Plot.ID, data$Plant.Number, sep="_")
data = data[,c(1,3,5,6,28,8,14,15)]
head(data)
#Subset data to only genotypes of interest
A632 = subset(data, Accession=="A632")
B73 = subset(data, Accession=="B73")
Oh43 = subset(data, Accession=="Oh43")
data=rbind(A632, B73, Oh43)

data1 = subset(data, Brace.Root.Whorls.in.the.Soil == "1")
n_distinct(data1$ID)
data2 = subset(data, Brace.Root.Whorls.in.the.Soil == "2")
n_distinct(data2$ID)
data3 = subset(data, Brace.Root.Whorls.in.the.Soil == "3")
n_distinct(data3$ID)

data1 = data1 %>%
  spread(Brace.Root.Status, Flexural.Rigidity..EI....IMU)
data2 = data2 %>%
  spread(Brace.Root.Status, Flexural.Rigidity..EI....IMU)
data3 = data3 %>%
  spread(Brace.Root.Status, Flexural.Rigidity..EI....IMU)

data1$C = "NA"
data1$D = "NA"
data1$Bottom = data1$B/data1$A
data1$Middle = "NA"
data1$Top = "NA"

data2$D = "NA"
data2$Top = "NA"
data2$Middle = data2$B/data2$A
data2$Bottom = data2$C/data2$B

data3$Top = data3$B/data3$A
data3$Middle = data3$C/data3$B
data3$Bottom = data3$D/data3$C

data4 = rbind(data1, data2, data3)
data4 = data4[,c(1:6,11:13)]
data5 = data4 %>%
  gather("Top","Middle","Bottom",key=whorl,value = NoneAll)
data5 = subset(data5, NoneAll!="NA")
data5$NoneAll = as.numeric(data5$NoneAll)
data5$Year = as.character(data5$Year)

data5 = subset(data5, Year=="2019")
#write.csv(data5, file="ProcessedData_2019_Inbred_Subpop_EI_All.csv", row.names = TRUE)

####ANOVAs: Does the contribution of each whorl differ within a genotype?
attach(data5)
lm_x <- lm(NoneAll ~ whorl*Accession)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("whorl","Accession"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
shapiro.test(data5$NoneAll)
#transformation data to meet assumptions
data5$tukey <- transformTukey(
  data5$NoneAll,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data5$tukey ~ whorl*Accession)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("whorl"), console = TRUE)
HSD.test(lm_x2_aov2, trt = c("Accession"), console = TRUE)
data5$tukey = NULL
detach(data5)

####Figure Generation
data5$Accession = factor(data5$Accession,levels = c("B73","Oh43","A632"))
Fig2C = ggplot(data5, aes(x=whorl, y=NoneAll, color=Accession)) +
  geom_jitter(width = 0.1, height = 0.1,size=2)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Ratio Individual Whorls")+
  theme(axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=17), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 15, colour = "blue"),
        strip.text.y = element_text(size = 15, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig2C

Figure2=plot_grid(Fig2A, Fig2B, Fig2C, labels = c('A', 'B', 'C'), nrow = 1)
Figure2

pdf("Figure2.pdf", width = 8.5, height = 4.5)
plot(Figure2)
dev.off()

##R6 Data Analysis####
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashleyhostetler/Desktop/Hostetler_Erndwein_et_al_2021/R Scripts and Associated Data/")
getwd()

data7 = read.csv(file= "ProcessedData_2019_02102022.csv", header = TRUE, na.strings = "NA")

#Remove plants that only have 1 whorl of data
head(data7)
x = data7 %>%    #identify how many unique 'ID's only appear once, suggesting there is only 1 observation for this plant, plot, rep
  group_by(ID) %>%
  dplyr::filter(n() == 1) 
x = as.data.frame(x) #convert to data frame
x = x$ID
x = as.list(x)
data7 <- data7[ ! data7$ID %in% x, ] #remove rows from list (list was plants that only had 1 observation)

####Span to depth ratio####
head(data7)
range(data7$ao)
mean(data7$ao)
17.5/(2*0.7625) #low value
17.5/(2*2.9670) #high value
17.5/(2*1.814219) #mean value



####Brace roots from whorls closer to the ground are stronger: Figure3, TableS3-S4####
####ANOVAS: Do structural mechanical properties differ between whorls and genotypes?
attach(data7)
#Does K vary among whorls
lm_x <- lm(K ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
shapiro.test(data7$K)
#transformation data to meet assumptions
data7$tukey <- transformTukey(
  data7$K,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data7$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR"), console = TRUE)
HSD.test(lm_x2_aov2, trt = c("Geno"), console = TRUE)
data7$tukey = NULL

#Does UL vary among whorls
lm_x <- lm(UL ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
shapiro.test(data7$UL)
#transformation data to meet assumptions
data7$tukey <- transformTukey(
  data7$UL,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data7$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR"), console = TRUE)
data7$tukey = NULL

#Does BL vary among whorls
lm_x <- lm(BL ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
HSD.test(lm_x_aov, trt = c("WR_CHR"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
shapiro.test(data7$BL)
detach(data7)

####Figure Generation - Structural Mechanical Properties
data7$Geno = factor(data7$Geno,levels = c("B73","Oh43","A632"))
Fig3C = ggplot(data7, aes(x=WR_CHR, y=K, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Structural stiffness - K (N/mm)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig3C

Fig3D = ggplot(data7, aes(x=WR_CHR, y=UL, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Ultimate load - UL (N)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig3D

Fig3E = ggplot(data7, aes(x=WR_CHR, y=BL, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Break load - BL (N)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig3E

Figure3=plot_grid(Fig3C, Fig3D, Fig3E, labels = c('C', 'D', 'E'), nrow = 1)
Figure3

pdf("Figure3.pdf", width = 8.5, height = 4.5)
plot(Figure3)
dev.off()

####Brace roots from whorls closer to the ground are larger: Figure4, TableS3-S4####
####ANOVAs: Does geometry differ between whorls and genotypes?
attach(data7)
#Does MajorD vary among whorls and genotypes
lm_x <- lm(MajorD_AH ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
HSD.test(lm_x_aov, trt = c("WR_CHR"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$MajorD_AH)
hist(data7$MajorD_AH)
shapiro.test(data7$MajorD_AH)

#Does MinorD vary among whorls and genotypes
lm_x <- lm(MinorD_AH ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
HSD.test(lm_x_aov, trt = c("WR_CHR"), console = TRUE)
HSD.test(lm_x_aov, trt = c("Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$MinorD_AH)
hist(data7$MinorD_AH)
shapiro.test(data7$MinorD_AH)

#Does MOI_Solid vary among whorls
lm_x <- lm(MOI_solid_AH ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$MOI_solid_AH)
hist(data7$MOI_solid_AH)
shapiro.test(data7$MOI_solid_AH)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data7$tukey <- transformTukey(
  data7$MOI_solid_AH,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data7$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR"), console = TRUE)
HSD.test(lm_x2_aov2, trt = c("Geno"), console = TRUE)
data7$tukey = NULL
detach(data7)

####Figure Generation - Geometry
data7$Geno = factor(data7$Geno,levels = c("B73","Oh43","A632"))
Fig4A = ggplot(data7, aes(x=WR_CHR, y=MajorD_AH, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Major Diameter (mm)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig4A

Fig4B = ggplot(data7, aes(x=WR_CHR, y=MinorD_AH, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Minor Diameter (mm)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig4B

Fig4C = ggplot(data7, aes(x=WR_CHR, y=MOI_solid_AH, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Moment of inertia (mm4)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig4C

####Figure Generation - Regressions: Can geometry explain structural mechanical properties?
data7$Geno = factor(data7$Geno,levels = c("B73","Oh43","A632"))
Fig4D = ggplot(data7, aes(x=MOI_solid_AH, y=K)) + 
  geom_point(aes(colour = factor(Geno)))+
  geom_smooth(method=lm, color="black")+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  labs(x="Moment of inertia (mm4)",y="Structural stiffness - K (N/mm)")
Fig4D
cor.test (data7$MOI_solid_AH, data7$K, use = "everything", method = c("pearson"))

Fig4E = ggplot(data7, aes(x=MOI_solid_AH, y=UL)) + 
  geom_point(aes(colour = factor(Geno)))+
  geom_smooth(method=lm, color="black")+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  labs(x="Moment of inertia (mm4)",y="Ultimate load - UL (N)")
Fig4E
cor.test (data7$MOI_solid_AH, data7$UL, use = "everything", method = c("pearson"))

Fig4F = ggplot(data7, aes(x=MOI_solid_AH, y=BL)) + 
  geom_point(aes(colour = factor(Geno)))+
  geom_smooth(method=lm, color="black")+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  labs(x="Moment of inertia (mm4)",y="Break load - BL (N)")
Fig4F
cor.test (data7$MOI_solid_AH, data7$BL, use = "everything", method = c("pearson"))

Figure4=plot_grid(Fig4A, Fig4B, Fig4C, Fig4D, Fig4E, Fig4F,  labels = c('A', 'B','C','D','E','F'), nrow = 2)
Figure4

pdf("Figure4.pdf", width = 8.5, height = 6.5)
plot(Figure4)
dev.off()

####The bending modulus of brace roots varies by whorl and genotype: Figure5, TableS3-S4####
####ANOVAs: Does bending modulus differ between whorls and genotypes?
attach(data7)
lm_x <- lm(E_GPa_AH ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$E_GPa_AH)
hist(data7$E_GPa_AH)
shapiro.test(data7$E_GPa_AH)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data7$tukey <- transformTukey(
  data7$E_GPa_AH,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data7$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR"), console = TRUE)
HSD.test(lm_x2_aov2, trt = c("Geno"), console = TRUE)
data7$tukey = NULL
detach(data7)

####Figure Generation - Material Properties
data7$Geno = factor(data7$Geno,levels = c("B73","Oh43","A632"))
Fig5 = ggplot(data7, aes(x=WR_CHR, y=E_GPa_AH, color=Geno, fill=Geno)) +
  geom_point(position=position_jitterdodge())+
  geom_boxplot(alpha=0.1)+
  scale_fill_manual(values=c("#009999", "#663399", "#CC9900"))+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Bending modulus - E (GPa)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "black"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Fig5

pdf("Figure5.pdf", width = 4.25, height = 4.5)
plot(Fig5)
dev.off()

##R1/R2 Data Analysis####
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashleyhostetler/Desktop/Hostetler_Erndwein_et_al_2021/R Scripts and Associated Data/")
getwd()
data7 = read.csv(file= "ProcessedData_2020_02102022.csv", header = TRUE, na.strings = "NA")
#Remove plants that only have 1 whorl of data
head(data7)
x = data7 %>%    #identify how many unique 'ID's only appear once, suggesting there is only 1 observation for this plant, plot, rep
  group_by(ID) %>%
  dplyr::filter(n() == 1) 
x = as.data.frame(x) #convert to data frame
x = x$ID
x = as.list(x)
data7 <- data7[ ! data7$ID %in% x, ] #remove rows from list (list was plants that only had 1 observation)
data7$Geno = factor(data7$Geno,levels = c("B73","Oh43","A632"))

####Span to depth ratio####
head(data7)
range(data7$a)
mean(data7$a)
17.5/(2*1.69) #low value
17.5/(2*3.19) #high value
17.5/(2*2.412935) #mean value

####Brace root structural mechanical properties and geometry are genotype-dependent at early reproductive stages: FigureS4-S6, TableS3-S4 ####
####ANOVAs: Does structural mechanical properties differ between whorls and genotypes?
attach(data7)
#Does K vary among whorls and genotypes
lm_x <- lm(K ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$K)
hist(data7$K)
shapiro.test(data7$K)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data7$tukey <- transformTukey(
  data7$K,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data7$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR","Geno"), console = TRUE)
data7$tukey = NULL

#Does UL vary among whorls and genotypes
lm_x <- lm(UL ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$UL)
hist(data7$UL)
shapiro.test(data7$UL)

#Does BL vary among whorls and genotypes
lm_x <- lm(BL ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$BL)
hist(data7$BL)
shapiro.test(data7$BL)
detach(data7)

####Figure Generation - Structural Mechanical Properties
data7$Geno = factor(data7$Geno,levels = c("B73","Oh43","A632"))
FigS4A = ggplot(data7, aes(x=WR_CHR, y=K, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Structural stiffness - K (N/mm")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
FigS4A

FigS4B = ggplot(data7, aes(x=WR_CHR, y=UL, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Ultimate load - UL (N)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
FigS4B

FigS4C = ggplot(data7, aes(x=WR_CHR, y=BL, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Break load - BL (N)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
FigS4C

FigureS4=plot_grid(FigS4A, FigS4B, FigS4C, labels = c('A', 'B', 'C'), nrow = 1)
FigureS4

pdf("FigureS4.pdf", width = 8.5, height = 4.5)
plot(FigureS4)
dev.off()

####ANOVAs: Does geometry differ between whorls and genotypes?
attach(data7)
#Does MajorD vary among whorls
lm_x <- lm(DCMax ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(1,1))
plot(data7$DCMax)
hist(data7$DCMax)
shapiro.test(data7$DCMax)

lm_x <- lm(DCMin ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$DCMin)
hist(data7$DCMin)
shapiro.test(data7$DCMin)

#Does MOI_Solid vary among whorls
lm_x <- lm(MOI_solid_AH ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$MOI_solid_AH)
hist(data7$MOI_solid_AH)
shapiro.test(data7$MOI_solid_AH)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data7$tukey <- transformTukey(
  data7$MOI_solid_AH,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
hist(data7$tukey)
lm_x2 <- lm(data7$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
data7$tukey = NULL
detach(data7)

####Figure Generation - Geometry
data7$Geno = factor(data7$Geno,levels = c("B73","Oh43","A632"))
FigS5A = ggplot(data7, aes(x=WR_CHR, y=DCMax, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Major Diameter (mm)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
FigS5A

FigS5B = ggplot(data7, aes(x=WR_CHR, y=DCMin, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Minor Diameter (mm)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
FigS5B

FigS5C = ggplot(data7, aes(x=WR_CHR, y=MOI_solid_AH, color=Geno)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Moment of inertia (mm4)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
FigS5C

####Figure Generation - Regressions: Can geometry explain structural mechanical properties?
data7$Geno = factor(data7$Geno,levels = c("B73","Oh43","A632"))
FigS5D = ggplot(data7, aes(x=MOI_solid_AH, y=K)) + 
  geom_point(aes(colour = factor(Geno)))+
  geom_smooth(method=lm, color="black")+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  labs(x="Moment of inertia (mm4)",y="Structural stiffness - K (N/mm)")
FigS5D
cor.test (data7$MOI_solid_AH, data7$K, use = "everything", method = c("pearson"))

FigS5E = ggplot(data7, aes(x=MOI_solid_AH, y=UL)) + 
  geom_point(aes(colour = factor(Geno)))+
  geom_smooth(method=lm, color="black")+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  labs(x="Moment of inertia (mm4)",y="Ultimate load - UL (N)")
FigS5E
cor.test (data7$MOI_solid_AH, data7$UL, use = "everything", method = c("pearson"))

FigS5F = ggplot(data7, aes(x=MOI_solid_AH, y=BL)) + 
  geom_point(aes(colour = factor(Geno)))+
  geom_smooth(method=lm, color="black")+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  labs(x="Moment of inertia (mm4)",y="Break load - BL (N)")
FigS5F
cor.test (data7$MOI_solid_AH, data7$BL, use = "everything", method = c("pearson"))

FigureS5=plot_grid(FigS5A, FigS5B, FigS5C, FigS5D, FigS5E, FigS5F,  labels = c('A', 'B','C','D','E','F'), nrow = 2)
FigureS5

pdf("FigureS5.pdf", width = 8.5, height = 6.5)
plot(FigureS5)
dev.off()

####ANOVAs: Does bending modulus differ between whorls and genotypes?
attach(data7)
lm_x <- lm(E_GPa_AH ~ WR_CHR*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data7$E_GPa_AH)
hist(data7$E_GPa_AH)
shapiro.test(data7$E_GPa_AH)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data7$tukey <- transformTukey(
  data7$E_GPa_AH,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data7$tukey ~ WR_CHR*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("Geno"), console = TRUE)
data7$tukey = NULL
detach(data7)

####Figure Generation - Bending Modulus
data7$Geno = factor(data7$Geno,levels = c("B73","Oh43","A632"))
FigS6 = ggplot(data7, aes(x=WR_CHR, y=E_GPa_AH, color=Geno, fill=Geno)) +
  geom_point(position=position_jitterdodge())+
  geom_boxplot(alpha=0.1, outlier.colour = "black", outlier.alpha = .5)+
  scale_fill_manual(values=c("#009999", "#663399", "#CC9900"))+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Whorl",y="Bending Modulus (Gpa)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "blue"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
FigS6

pdf("FigureS6.pdf", width = 4.25, height = 4.5)
plot(FigS6)
dev.off()



##R1/R2 vs R6 Comparison Data Analysis#####
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashleyhostetler/Desktop/Hostetler_Erndwein_et_al_2021/R Scripts and Associated Data/")
getwd()

#R1/R2 Data
data2020 = read.csv(file= "ProcessedData_2020_02102022.csv", header = TRUE, na.strings = "NA")
head(data2020)
x = data2020 %>%    #identify how many unique 'ID's only appear once, suggesting there is only 1 observation for this plant, plot, rep
  group_by(ID) %>%
  dplyr::filter(n() == 1) 
x = as.data.frame(x) #convert to data frame
x = x$ID
x = as.list(x)
data2020 <- data2020[ ! data2020$ID %in% x, ] #remove rows from list (list was plants that only had 1 observation)

data2020$MajorD_AH = data2020$DCMax
data2020$MinorD_AH = data2020$DCMin
data2020 = data2020[,c(1:8,16:17,13:15)]
data2020$Year = "2020"

#R6 Data
data2019 = read.csv(file= "ProcessedData_2019_02102022.csv", header = TRUE, na.strings = "NA")
####Remove plants that only have 1 whorl of data
head(data2019)
x = data2019 %>%    #identify how many unique 'ID's only appear once, suggesting there is only 1 observation for this plant, plot, rep
  group_by(ID) %>%
  dplyr::filter(n() == 1) 
x = as.data.frame(x) #convert to data frame
x = x$ID
x = as.list(x)
data2019 <- data2019[ ! data2019$ID %in% x, ] #remove rows from list (list was plants that only had 1 observation)
data2019 = data2019[,c(1,2,5,7,4,12,13,14,22,21,17,19,20)]
data2019$Year = "2019"

data=rbind(data2019, data2020)
#write.csv(data, file="2019-2020_Data_Combined_12012021.csv", row.names = TRUE)

####Structural mechanical properties are variable by growth stage within a genotype: Figure 6-7, TableS7-S8####
###ANOVAs : Does structural mechanical properties and I differ between whorls,genotypes, and developmental stage
attach(data)
#Does K vary among whorls, genotypes, and developmental stage
lm_x <- lm(K ~ WR_CHR+Year+Geno+WR_CHR*Year+WR_CHR*Geno+Year*Geno+WR_CHR*Year*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Year","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$K)
hist(data$K)
shapiro.test(data$K)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data$tukey <- transformTukey(
  data$K,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data$tukey ~ WR_CHR+Year+Geno+WR_CHR*Year+WR_CHR*Geno+Year*Geno+WR_CHR*Year*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR","Year","Geno"), console = TRUE)
data$tukey = NULL

#Does UL vary among whorls, genotypes, and developmental stage
lm_x <- lm(UL ~ WR_CHR+Year+Geno+WR_CHR*Year+WR_CHR*Geno+Year*Geno+WR_CHR*Year*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Year","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$UL)
hist(data$UL)
shapiro.test(data$UL)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data$tukey <- transformTukey(
  data$UL,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data$tukey ~ WR_CHR+Year+Geno+WR_CHR*Year+WR_CHR*Geno+Year*Geno+WR_CHR*Year*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR","Year","Geno"), console = TRUE)
data$tukey = NULL

#Does BL vary among whorls, genotypes, and developmental stage
lm_x <- lm(BL ~ WR_CHR+Year+Geno+WR_CHR*Year+WR_CHR*Geno+Year*Geno+WR_CHR*Year*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Year","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$BL)
hist(data$BL)
shapiro.test(data$BL)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data$tukey <- transformTukey(
  data$BL,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data$tukey ~ WR_CHR+Year+Geno+WR_CHR*Year+WR_CHR*Geno+Year*Geno+WR_CHR*Year*Geno)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("WR_CHR","Geno"), console = TRUE)
HSD.test(lm_x2_aov2, trt = c("Year"), console = TRUE)
data$tukey = NULL

#Does MOI vary among whorls, genotypes, and developmental stage
lm_x <- lm(MOI_solid_AH ~ WR_CHR+Year+Geno+WR_CHR*Year+WR_CHR*Geno+Year*Geno+WR_CHR*Year*Geno)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("WR_CHR","Year","Geno"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$MOI_solid_AH)
hist(data$MOI_solid_AH)
shapiro.test(data$MOI_solid_AH)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data$tukey <- transformTukey(
  data$MOI_solid_AH,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
lm_x2 <- lm(data$tukey ~ WR_CHR+Year+Geno+WR_CHR*Year+WR_CHR*Geno+Year*Geno+WR_CHR*Year*Geno)
anova(lm_x2)
data$tukey = NULL
detach(data)

####Figure Generation - Structural Mechanical Properties between stages
data$Geno = factor(data$Geno,levels = c("B73","Oh43","A632"))
data$Year = factor(data$Year,levels = c("2020","2019"))
Fig6A = ggplot(data, aes(x=Year, y=K, color=Geno, shape=Year)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Developmental Stage",y="Structural stiffness - K (N/mm")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "black"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")+
  facet_wrap(~WR_CHR)+
  geom_jitter(width=0.25)
Fig6A

Fig6B = ggplot(data, aes(x=Year, y=UL, color=Geno, shape=Year)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Developmental Stage",y="Ultimate load - UL (N)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "black"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")+
  facet_wrap(~WR_CHR)+
  geom_jitter(width=0.25)
Fig6B

Fig6C = ggplot(data, aes(x=Year, y=BL, color=Geno, shape=Year)) +
  geom_jitter(width = 0.1, height = 0.1,size=1.5)+
  scale_color_manual(values=c("#009999", "#663399", "#CC9900"))+
  labs(x="Developmental Stage",y="Break load - BL (N)")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "black"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")+
  facet_wrap(~WR_CHR)+
  geom_jitter(width=0.25)
Fig6C

Figure6=plot_grid(Fig6A, Fig6B, Fig6C, labels = c('A', 'B', 'C'), nrow = 3)
Figure6

pdf("Figure6.pdf", width = 4.25, height = 7.5)
plot(Figure6)
dev.off()


####Figure Generation - Comparison of bending modulus within maize
Figure7 = ggplot(data=data, aes(x=E_GPa_AH, group=Year, fill=Year)) +
  geom_density(adjust=1.5, alpha=.5)+
  scale_fill_manual(values=c("#009999", "#CC9900"))+
  labs(x="E [GPa]",y="Density")+
  theme(axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        plot.title=element_text(size=11, vjust=3), 
        axis.text=element_text(size=11), 
        axis.title = element_text(size=11), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 11, colour = "black"),
        strip.text.y = element_text(size = 11, colour = "black"),
        legend.title=element_blank())+
  theme(legend.position = "none")
Figure7

pdf("Figure7.pdf", width = 3.54, height = 4)
plot(Figure7)
dev.off()
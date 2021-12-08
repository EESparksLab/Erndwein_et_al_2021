cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashleyhostetler/Desktop/Hostetler_Erndwein_et_al_2021/R/")
getwd()
library(tidyverse)
data = read.csv(file= "2020_R1R2Field_55Geno_Data_AH-MasterFile.csv", header = TRUE, na.strings = "NA")
data = data[,c(1:15)]
str(data)
i <- c(1:4,8)
data[,i] <- apply(data[ ,i], 2,
                  function(x) as.character(x))
i <- c(5:7,9:15)
data[,i] <- apply(data[ ,i], 2, 
                  function(x) as.numeric(x))

#Change WR number to Charachter (e.g. WR1 for TWR, WR2 for MWR, WR3 for BWR)
data$WR_CHR = data$WR
for (i in 1:length(data$WR_CHR)){
  if (data$WR_CHR[i] == 1) {
    data$WR_CHR[i] = "Top"
  } else if (data$WR_CHR[i] == 2) {
    data$WR_CHR[i] = "Middle"
  } else if (data$WR_CHR[i] == 3) {
    data$WR_CHR[i] = "Bottom"
  }
}

datax = subset(data, WR_CHR == "Bottom")
datay = subset(data, WR_CHR == "Middle")
data1 = rbind(datax, datay)

data1$ID = paste(data1$Geno, data1$Plant, sep="_")

#Calculate means for all roots within plant within whorl
colnames(data1)
data2 = data1[,c(17,1,2,16,5:7,9:15)]
colnames(data2)
MEANS = data2[,c(1:4)]
MEANS<-unique(MEANS[,c(1:4)]) #remove duplicate genotype entries
name<-colnames(data2)
name
name<-name[c(-1,-2,-3,-4)] #Remove all column names that are not traits
for(i in 1:length(name)){
  datax = data2[c(1,2,3,4,i+4)]
  names(datax)[5] = paste("trait")
  datay = datax %>%
    group_by(ID, Geno, Plant, WR_CHR) %>%
    summarise(trait_avg = mean(trait, na.rm = TRUE))
  datay = as.data.frame(datay) 
  test = datay[,c(1,2,3,4,5)] #This needs to have PLOT ID and Number 
  names(test)[5] = paste(name[i])
  MEANS = merge(MEANS, test, by.x =c("ID", "Geno", "Plant", "WR_CHR"))
}
data3 = MEANS

data3 = data3[!is.na(data3$BL),]

#Calculations
colnames(data3)
data3$MOI_solid_AH = (3.1415/4)*((data3$a^3)*(data3$b))
data3$E_Mpa_AH = ((data3$K)*(17^3))/(48*data3$MOI_solid_AH)
data3$E_GPa_AH = ((data3$E_Mpa_AH)*0.001)
colnames(data3)
data4 = data3[,c(1:11,15:17)]
data4A = subset(data4, Geno == "B73")
data4B = subset(data4, Geno == "Oh43")
data4C = subset(data4, Geno == "A632")
write.csv(data4, file="ProcessedData_2020_10272021.csv", row.names = TRUE)

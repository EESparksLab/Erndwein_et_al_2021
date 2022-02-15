cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashleyhostetler/Desktop/Hostetler_Erndwein_et_al_2021/R/")
getwd()
library(dplyr)
#Check Whorl Information - 08242021 ####
data0 = read.csv(file= "2019_R6Field_55Geno_Data_AH-MasterFile.csv", header = TRUE, na.strings = "NA")
data1 = read.csv(file= "Inbred_Subpop_2Years_Broot_Ratio.csv", header = TRUE, na.strings = "NA")
colnames(data0)
data0 = data0[,c(2,6:9,13,21:28,31:40)]
data = data0[,c(1:6)]
data$ID = paste(data$Geno, data$Row, data$Plant, sep="_")
colnames(data1)
data1 = subset(data1, Year == "2019")
data1 = data1[,c(3,5,6,13)]
data1$ID = paste(data1$Accession, data1$Plot.ID, data1$Plant.Number, sep="_")
data2 = merge(data, data1, by = "ID")
head(data2)
data2 = na.omit(data2)

#Seperate into 3 data frames depending on how many whorls are present 
data_1 = subset(data2, Brace.Root.Whorls.in.the.Soil == "1")
n_distinct(data_1$WR_number)
data_2 = subset(data2, Brace.Root.Whorls.in.the.Soil == "2")
n_distinct(data_2$WR_number)
data_3 = subset(data2, Brace.Root.Whorls.in.the.Soil == "3")
n_distinct(data_3$WR_number)

#Update Structure
str(data_1)
str(data_2)
str(data_3)
#Change colmns 1-5 to charachters
i <- c(1:5)                            
data_1[,i] <- apply(data_1[ ,i], 2,           
                    function(x) as.character(x))
data_2[,i] <- apply(data_2[ ,i], 2,           
                    function(x) as.character(x))
data_3[,i] <- apply(data_3[ ,i], 2,           
                    function(x) as.character(x))
#Change colmns 6-7 to numeric
i <- c(6:7)                            
data_1[,i] <- apply(data_1[ ,i], 2,          
                    function(x) as.numeric(x))
data_2[,i] <- apply(data_2[ ,i], 2,           
                    function(x) as.numeric(x))
data_3[,i] <- apply(data_3[ ,i], 2,           
                    function(x) as.numeric(x))


#Use Loop to change WR number to Charachter (e.g. WR1 for BWR, WR2 for MWR, WR3 for TWR)
data_1$WR_CHR = data_1$WR_number
for (i in 1:length(data_1$WR_CHR)){
  if (data_1$WR_CHR[i] == 1) {
    data_1$WR_CHR[i] = "Bottom"
  } else if (data_1$WR_CHR[i] == 2) {
    data_1$WR_CHR[i] = "NA"
  } else if (data_1$WR_CHR[i] == 3) {
    data_1$WR_CHR[i] = "NA"
  }
}


data_2$WR_CHR = data_2$WR_number
for (i in 1:length(data_2$WR_CHR)){
  if (data_2$WR_CHR[i] == 1) {
    data_2$WR_CHR[i] = "Middle"
  } else if (data_2$WR_CHR[i] == 2) {
    data_2$WR_CHR[i] = "Bottom"
  } else if (data_2$WR_CHR[i] == 3) {
    data_2$WR_CHR[i] = "NA"
  }
}


data_3$WR_CHR = data_3$WR_number
for (i in 1:length(data_3$WR_CHR)){
  if (data_3$WR_CHR[i] == 1) {
    data_3$WR_CHR[i] = "Top"
  } else if (data_3$WR_CHR[i] == 2) {
    data_3$WR_CHR[i] = "Middle"
  } else if (data_3$WR_CHR[i] == 3) {
    data_3$WR_CHR[i] = "Bottom"
  }
}


data2=rbind(data_1, data_2, data_3)
data2 = data2[,c(1,2,11:12)]
data3 = merge(data2, data0, by = "ScanOrder")

colnames(data3)
data3$PlantPlot = paste(data3$Row, data3$Plant, sep="_")

data3x = subset(data3, WR_CHR == "Bottom")
data3y = subset(data3, WR_CHR == "Middle")
data3 = rbind(data3x, data3y)
data3A = subset(data3, Geno == "B73")
data3B = subset(data3, Geno == "Oh43")
data3C = subset(data3, Geno == "A632")
data3 = rbind(data3A, data3B, data3C)

data4 = data3[grep("[[:digit:]]", data3$MinorD), ]
str(data4)
colnames(data4)
i = c(1:9)                          
data4[,i] <- apply(data4[ ,i], 2,          
                   function(x) as.character(x))
i = c(10:17)                          
data4[,i] <- apply(data4[ ,i], 2,  
                   function(x) as.numeric(x))
i = c(19:21)                    
data4[,i] <- apply(data4[ ,i], 2,
                   function(x) as.numeric(x))
i = c(23:27)                    
data4[,i] <- apply(data4[ ,i], 2,
                   function(x) as.numeric(x))

x = data4 %>%    #identify how many unique 'ID's only appear once, suggesting there is only 1 observation for this plant, plot, rep
  group_by(ID) %>%
  dplyr::filter(n() == 1) 
x = as.data.frame(x) #convert to data frame
x = x$ID
x = as.list(x)
data4 = data4[ ! data4$ID %in% x, ] #remove rows from list (list was plants that only had 1 observation)

#Update K by a factor of 10
data4$K = data4$K/10

#Calculate means for all roots within plant within whorl 10/19/2021
colnames(data4)
data5 = data4[,c(2,3,4,5,6,7,10:17,19:21,23:27)]
colnames(data5)
MEANS = data5[,c(1,2,3,4,5,6)]
MEANS<-unique(MEANS[,c(1,2,3,4,5,6)]) #remove duplicate genotype entries; keep only one row per genotype 
name<-colnames(data5) 
name
name<-name[c(-1,-2,-3,-4,-5,-6)] #Remove all column names that are not traits
for(i in 1:length(name)){
  datax = data5[c(1,2,3,4,5,6,i+6)]
  names(datax)[7] = paste("trait")
  datay = datax %>%
    group_by(ID, Brace.Root.Whorls.in.the.Soil, WR_CHR, Geno, Row, Plant) %>%
    summarise(trait_avg = mean(trait, na.rm = TRUE))
  datay = as.data.frame(datay) 
  test = datay[,c(1,2,3,4,5,6,7)]
  names(test)[7] = paste(name[i])
  MEANS = merge(MEANS, test, by.x =c("ID", "Brace.Root.Whorls.in.the.Soil", "WR_CHR", "Geno", "Row", "Plant"))
}
data6 = MEANS
data6 = data6[!is.na(data6$BL),]

#Calculations 
data6$MOI_solid_AH = (3.1415/4)*((data6$ao^3)*(data6$bo))
data6$MOI_hollow_AH = (3.1415/4)*(((data6$ao^3)*(data6$bo))-((data6$ai2^3)*(data6$bi2)))
data6$E_Mpa_AH = ((data6$K)*(17^3))/(48*data6$MOI_solid_AH)
data6$E_GPa_AH = ((data6$E_Mpa_AH)*0.001)
data6$MinorD_AH = (2*data6$ao)
data6$MajorD_AH = (2*data6$bo)
colnames(data6)
data7 = data6[,c(1:10,15:17,21:28)]
write.csv(data7, file="ProcessedData_2019_02102022.csv", row.names = TRUE)

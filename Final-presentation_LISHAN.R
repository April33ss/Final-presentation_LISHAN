

#DEA-CCR & DEA-BCC
#read the file of input and output of 30 provinces for one year
data16 <- read.csv("C:/Users/inha/Desktop/data/sum/R/2016.csv")
head(data16)  #Simply browse through the data sheets.The names of the indexes are found to be too long.

#Rename the columns
library(dplyr)
data16 <- rename(data16, x1 = Fresh.cold.chain.logistics.fixed.asset.investment.100.million.yuan.,
                 x2 = Number.of.employees.related.to.logistics.10.000.people.,
                 x3 = Cold.storage.capacity.10.000.tons.,
                 y1 = Fresh.cold.chain.freight.volume.10.000.tons.,
                 y2 = Fresh.cold.chain.logistics.industry.value.added.100.million.yuan.)
data16<- data.frame(data16)
data16
summary(data16)

#Classify input and output indicators
X_data<- matrix(c(data16$x1,data16$x2,data16$x3), ncol=3 ) 
head(X_data)
Y_data<- matrix(c(data16$y1,data16$y2), ncol=2)
head(Y_data)

#Calculate efficiency based on input-orientation crs & vrs
library(Benchmarking)
CRS_eff<- dea(X= X_data, Y=Y_data,RTS="crs", ORIENTATION="in") #Calculate technical efficiency
VRS_eff<- dea(X= X_data, Y=Y_data,RTS="vrs", ORIENTATION="in",SLACK=TRUE )#calculate pure technical efficiency

dea.plot.frontier(X_data,Y_data,RTS="vrs",col="blue",txt=TRUE)#Draw a graph of a DEA technology. 

CRS_eff  #data of technical efficiency(TE)
VRS_eff  #data of pure technical efficiency(PTE)

CRS_eff$eff  #TE details 
VRS_eff$eff  #PTE details

summary(CRS_eff)
summary(VRS_eff)

Scale_eff<- CRS_eff$eff/VRS_eff$eff  #calculate the scale efficiency(SE)
Scale_eff

result1<- cbind(c(1:30),data16$DMU,CRS_eff$eff,VRS_eff$eff,Scale_eff,VRS_eff$slack)
colnames(result1)<- c("Number","DMU","CRS_eff","VRS_eff","Scale_eff","slack")#Name columns that have no name
result1

result<-cbind(result1,VRS_eff$sx,VRS_eff$sy)
result

write.csv(result, file = "C:/Users/inha/Desktop/data/sum/R/2016result.csv")

#Make statistical barplots of the calculation results
barplot(CRS_eff$eff,main = " Technical efficiency of each DMU in 2016",
        names.arg= c(1:30),
        xlab = "DMU",
        ylab = "TE value",
        col = '#99d5eb',  
        border = '#729db7') #The TE value of each DMU for the year is represented by a barplot
abline(h=1, lwd=2,lty=1,col='red')#add a line of TE=1 which means that TE is effective.
abline(h=0.9,lwd=2,lty=2,col='#61ac53')# 0.9<TE<1 means TE is weak effective.

barplot(VRS_eff$eff,main = " Pure technical efficiency of each DMU in 2016",
        names.arg= c(1:30),
        xlab = "DMU",
        ylab = "PTE value",
        col = '#f2d98f',  
        border = '#ffe993') #The PTE value of each DMU for the year is represented by a barplot
abline(h=1, lwd=2,lty=1,col='red')#add a line of PTE=1 which means that PTE is effective.

barplot(VRS_eff$eff,main = " Scale efficiency of each DMU in 2016",
        names.arg= c(1:30),
        xlab = "DMU",
        ylab = "SE value",
        col = '#dfedde',  
        border = '#72c9b4') #The SE value of each DMU for the year is represented by a barplot
abline(h=1, lwd=2,lty=1,col='red')#add a line of PTE=1 which means that SE is effective.

#Calculating the efficiency of other years requires only modifying the csv data


#Malmquist index method
#Read the data of each DMU from 2016 to 2019
data <- read.csv("C:/Users/inha/Desktop/data/sum/R/1619.csv")
head(data)

#Rename the columns
library(dplyr)
data <- rename(data, x1 = Fresh.cold.chain.logistics.fixed.asset.investment.100.million.yuan.,
               x2 = Number.of.employees.related.to.logistics.10.000.people.,
               x3 = Cold.storage.capacity.10.000.tons.,
               y1 = Fresh.cold.chain.freight.volume.10.000.tons.,
               y2 = Fresh.cold.chain.logistics.industry.value.added.100.million.yuan.)
data<- data.frame(data)
data
summary(data)

#Classify input and output indicators
X_data<- matrix(c(data$x1,data$x2,data$x3), ncol=3 ) 
head(X_data)
Y_data<- matrix(c(data$y1,data$y2), ncol=2)
head(Y_data)

#Calculate Malmquist results
library(nonparaeff)
malm.data<- data.frame(year= rep(2016:2019,each=30),data)
malm.data

malm.result <- faremalm2(malm.data,noutput = 2, id="DMU", year="year")
malm.result 
#The data obtained are arranged by time and the first letter of the province. It's not easy to use.

#Modify the name of the DMU to keep the order consistent
malm.data<- data.frame(DMU=rep(1:30,time=4), year= rep(2016:2019,each=30),data$y1,data$y2,data$x1,data$x2,data$x3)
malm.data

malm.result <- faremalm2(malm.data,noutput = 2, id="DMU", year="year")
malm.result

write.csv(malm.result, file = "C:/Users/inha/Desktop/data/sum/R/malmresult.csv")


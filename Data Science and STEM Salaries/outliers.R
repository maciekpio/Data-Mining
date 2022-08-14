
setwd("C:/Users/sivay/Documents/MD")
dd <- read.table("NaTreated.csv",header=T, sep=";", dec='.');

library(ggplot2)
names(dd)

#Tratar outliers totalYearlyCompensation
summary(dd[4])
boxplot(dd[4])

boxplot(dd$totalyearlycompensation, plot=FALSE)$out 
outliers <- boxplot(dd$totalyearlycompensation, plot=FALSE)$out
x<-dd 
x<- x[-which(x$totalyearlycompensation %in% outliers),] 
boxplot(x[4])
#boxplot final WITH POSSIBLE outliers

dd <- x

#Tratar outliers yearsOfExperience
summary(dd[6])
boxplot(dd[6])

boxplot(dd$yearsofexperience, plot=FALSE)$out 
outliers <- boxplot(dd$yearsofexperience, plot=FALSE)$out
x<-dd 
x<- x[-which(x$yearsofexperience %in% outliers),] 
boxplot(x[6])
#boxplot final WITHOUT outliers

dd <- x

#Tratar outliers yearsAtCompany
summary(dd[7])
boxplot(dd[7])

boxplot(dd$yearsAtCompany, plot=FALSE)$out 
outliers <- boxplot(dd$yearsatcompany, plot=FALSE)$out
x<-dd 
x<- x[-which(x$yearsatcompany %in% outliers),] 
boxplot(x[7])
#boxplot final WITHOUT outliers

dd <- x

#Tratar outliers baseSalary
summary(dd[9])
boxplot(dd[9])

boxplot(dd$basesalary, plot=FALSE)$out 
outliers <- boxplot(dd$basesalary, plot=FALSE)$out
x<-dd 
x<- x[-which(x$basesalary %in% outliers),] 
boxplot(x[9])
#boxplot final WITHOUT outliers

dd <- x

#Tratar outliers stockGrantValue
summary(dd[10])
boxplot(dd[10])

boxplot(dd$stockgrantvalue, plot=FALSE)$out 
outliers <- boxplot(dd$stockgrantvalue, plot=FALSE)$out
x<-dd 
x<- x[-which(x$stockgrantvalue %in% outliers),] 
boxplot(x[10])
#boxplot final WITH POSSIBLE outliers

dd <- x

#Tratar outliers bonus
summary(dd[11])
boxplot(dd[11])

boxplot(dd$bonus, plot=FALSE)$out 
outliers <- boxplot(dd$bonus, plot=FALSE)$out
x<-dd 
x<- x[-which(x$bonus %in% outliers),] 
boxplot(x[11])
#boxplot final WITHOUT outliers

dd <- x

write.table(dd, file = "TreatedOutliers.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)


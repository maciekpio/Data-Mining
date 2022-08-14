setwd("C:/Users/whisp/OneDrive/Escritorio/md")
dat <- read.csv("3000WithNA.csv", header=TRUE, sep=";")

#to calculate the % of missings
sapply(dat, function(x) sum(is.na(x)))

#mapping the locations into acronyms
dat$location <- unlist(lapply(dat$location, function(x) unlist(strsplit(x, ", "))[[2]]))
#group the acronyms into regions
dat$location[dat$location == "CT" | dat$location == "ME" | dat$location == "MA" | dat$location == "NH" | dat$location == "RI" | dat$location == "VT" | dat$location == "NJ" | dat$location == "NY" | dat$location == "PA"] <- "REGION1"
dat$location[dat$location == "IL" | dat$location == "IN" | dat$location == "MI" | dat$location == "OH" | dat$location == "WI" | dat$location == "IN" | dat$location == "NC" | dat$location == "ND" | dat$location == "IA" | dat$location == "KS" | dat$location == "MN" | dat$location == "MO" | dat$location == "NE"] <- "REGION2"
dat$location[dat$location == "SC" | dat$location == "NC" | dat$location == "DE" | dat$location == "DC" | dat$location == "FL" | dat$location == "GA" | dat$location == "MD" | dat$location == "ND" | dat$location == "VA" | dat$location == "WV" | dat$location == "AL" | dat$location == "KY" | dat$location == "MS" | dat$location == "TN" | dat$location == "AR" | dat$location == "LA" | dat$location == "OK" | dat$location == "TX"] <- "REGION3"
dat$location[dat$location == "AZ" | dat$location == "CO" | dat$location == "ID" | dat$location == "MT" | dat$location == "NV" | dat$location == "NM" | dat$location == "UT" | dat$location == "WY" | dat$location == "AK" | dat$location == "CA" | dat$location == "HI" | dat$location == "OR" | dat$location == "WA"] <- "REGION4"





install.packages("mice")
library(mice)
install.packages("dplyr")
library(dplyr)

#we need mutate the variables into factor or num
dat <- dat %>% mutate(company = as.factor(company)) %>% mutate(level = as.factor(level)) %>% mutate(title = as.factor(title)) %>% mutate(location = as.factor(location)) %>% mutate(tag = as.factor(tag)) %>% mutate(Race = as.factor(Race)) %>% mutate(Education = as.factor(Education)) %>% mutate(gender = as.factor(gender)) %>% mutate(totalyearlycompensation = as.numeric(totalyearlycompensation)) %>% mutate(stockgrantvalue = as.numeric(stockgrantvalue)) %>% mutate(yearsofexperience = as.numeric(yearsofexperience)) %>% mutate(yearsatcompany = as.numeric(yearsatcompany)) %>% mutate(bonus = as.numeric(bonus)) %>% mutate(basesalary = as.numeric(basesalary))
#to check if everything is ok
str(dat)

#inicialize the mice parameters
init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix

#we need to choose what method we want apply for each variable
meth[c("Race", "Education", "gender")]="polyreg"

#mice's execution
imputed = mice(dat, method=meth, predictorMatrix=predM, m=5, nnet.MaxNWts = 3200)
#to get our new dataset
dat <- complete(imputed,1)

#export the dataset
write.table(dat, file = "NaTreated.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

#to check if we have similar results
model1 <- read.csv("3000WithNA.csv", header=TRUE, sep=";")
model2 <- complete(imputed,1)

model1<-with(model1,lm(basesalary ~ gender+Race+Education))
summary(model1)

model2<-with(model2,lm(basesalary ~ gender+Race+Education))
summary(model2)

model2 <- complete(imputed,2)

model2<-with(model2,lm(basesalary ~ gender+Race+Education))
summary(model2)

model2 <- complete(imputed,3)

model2<-with(model2,lm(basesalary ~ gender+Race+Education))
summary(model2)

model2 <- complete(imputed,4)

model2<-with(model2,lm(basesalary ~ gender+Race+Education))
summary(model2)

model2 <- complete(imputed,5)

model2<-with(model2,lm(basesalary ~ gender+Race+Education))
summary(model2)

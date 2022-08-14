install.packages('stringr', dependencies = TRUE)
install.packages('dplyr', dependencies = TRUE)
library('stringr')
library('dplyr')
dd <- read.csv("Levels_Fyi_Salary_Data.csv", row.names = NULL, stringsAsFactors = FALSE)

dd<-select(dd, company, level, title, totalyearlycompensation, location, yearsofexperience, yearsatcompany, tag, basesalary, stockgrantvalue, bonus, gender, Race, Education)
            

#Rename all companies other than top5 to "Others"
dd$company[!(dd$company =="Amazon" | dd$company =="Microsoft" | dd$company =="Google" | 
              dd$company =="Facebook" | dd$company =="Apple")] <- "Others"


#Remove all rows that are from outside USA
dd$location[(str_count(dd$location, ",") >= 2)] <- "outsideUSA"
dd <- dd[!(dd$location =="outsideUSA"),]



#Remove all job positions that aren't in the top10 
dd <- dd[dd$tag == "Full Stack" | dd$tag == "Distributed Systems (Back-End)" | dd$tag == "API Development (Back-end)" | 
         dd$tag == "ML / AI" | dd$tag == "Web Development (Front-End)" | dd$tag == "Product" | dd$tag == "Data" | 
         dd$tag == "DevOps" | dd$tag == "Security" | dd$tag == "Networking" | is.na(dd$tag),]
#We have to include NAs in the past function, otherwise some columns are filled with NA and not deleted, so we delete the NA afterwards:
dd <- dd[!is.na(dd$tag),]


#Standardize the level of experience
dd$level[(dd$level =="Senior Software Engineer")] <- "L5"
dd$level[(dd$level =="Senior")] <- "L5"
dd$level[(dd$level =="IC3")] <- "L2"
dd$level[(dd$level =="IC4")] <- "L4"
dd$level[(dd$level =="63")] <- "L6"
dd$level[(dd$level =="62")] <- "L5"
dd$level[(dd$level =="61")] <- "L4"
dd$level[(dd$level =="E4")] <- "L2"
dd$level[(dd$level =="ICT5")] <- "L6"
dd$level[(dd$level =="ICT4")] <- "L5"

#Remove all levels other than L1 to L6
dd<- dd[dd$level == "L1" | dd$level == "L2" | dd$level == "L3" | dd$level == "L4" | dd$level == "L5" | dd$level == "L6" | is.na(dd$level),]
dd <- dd[!is.na(dd$level),]

#We separate the rows that have some NA in Education, Race or gender on another dataset, then we remove them from the main dataset
ddna <- dd[is.na(dd$Education) | is.na(dd$Race) | is.na(dd$gender) ,]
dd <- dd[!is.na(dd$Education) & !is.na(dd$Race) & !is.na(dd$gender),]


#Randomly delete 1000 rows from the main dataset
dd <- dd[-sample(1:nrow(dd), 1000), ]

#Randomly delete 8000 from the NA dataset
ddna <- ddna[-sample(1:nrow(ddna), 8000), ]

#We combine the main dataset and the NA dataset to have 3000 rows in total with 13% of NA
ddT <- rbind(dd, ddna)

sapply(ddT, function(x) sum(is.na(x)))




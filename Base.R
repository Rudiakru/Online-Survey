###Load packages(pacman)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, janitor, lubridate, psych,
               plotly, purrr, readr, rio, repurrrsive, rmarkdown, shiny, stringr,survey, tidyr, tidyverse)
install.packages("xlsx")
library("xlsx")

###Importing with rio

#rio_csv <- import("") #Makes it a data frame
#weighting <- import("") #Import the weigthing excel


###----------------------------###     Choosing the question     ###----------------------------###

dataset <- na.omit(subset(rio_csv, select = c(268, 220, 221))) #For Question Q2j
dataset <- subset(dataset, subset = (qhidagree20 == 3))

#dataset <- na.omit(subset(dataset, subset = (q2j == 1)))

#dataset <- na.omit(subset(dataset, subset = (qhidagree20 == 1)))
#dataset <- na.omit(subset(dataset, subset = (p2 == 16))) #math

i.dataset <- dataset %>% 
  relocate(p1, .before = qhidagree20) #Take p1 to first column

data1 <- cbind(i.dataset, total = rowSums(i.dataset[2:3])) 

data2 <- data1[,c(1,4)]

data2$total= 1

data3<-data2 %>% 
  group_by(p1) %>% 
  summarise(TotalAnswers = sum(total))

i.dataset5 <- data3 %>% 
  relocate(p1, .before = TotalAnswers) #Take p1 to first column

i.dataset5 <- i.dataset5[order(as.integer(i.dataset5$p1),decreasing = FALSE), ] #Dataset in decreasing order

#-----------------------#Dataset from excel#-----------------------#

weighting <-na.omit(weighting[1:10]) #Without the nas
i.dataset2 <- weighting %>% select(7,10) #Choose p1 and want from the
i.dataset2

#-----------------------#Make a dataset with the right order#-----------------------#

x <- weighting$`country by p1` #Returns list in the right order for the countries

x

condensedData2 <- i.dataset5 %>% #through comparison of x brings Group1 in the right order, although the rest is made na's
  mutate(p1 =  factor(p1, levels = x)) %>%
  arrange(p1) 

#-----------------------#Summing up the rest#-----------------------#

dat1 <- condensedData2[1:20,] #First split condensedData2 by rows in two sets
dat2 <- condensedData2[21:100,]


Other <-  dat2 %>% #In dat2 the "Other" get summed up as one variable
  adorn_totals("row", name= "Other")

#-----------------------#Making the complete data.frame#-----------------------#

dat3 <- Other[81,] #Take the whole row of "Other"

summed_countries_and_other <-rbind(dat1, dat3) #bind summed "Other countries" and summed up 20 countries

#-----------------------#Weigthing function in action#-----------------------#

weightedAnswers <- as.data.frame(mapply('*', data.frame(summed_countries_and_other), data.frame(weighting[10])))
weightedAnswers <- weightedAnswers[, -1] # Take first column out

y <-summed_countries_and_other[1]

weightedAnswers <- as.data.frame(cbind(y, weightedAnswers))  #give the names of Group1 from Na into weightframe
names(weightedAnswers)[1] <- "Countries" #Rename first column

#-----------------------#Weighted total per Anwser#-----------------------#

totalWeightedAnswers.dataframe <-  weightedAnswers %>%  #Sum up ALL answers for each column and make new row with "Total per Answer" for every question
  adorn_totals("row", name = "Total_per_Answer")
totalWeightedAnswersRounded.dataframe<- adorn_rounding(totalWeightedAnswers.dataframe, digits = 0, rounding = "half up")



#demographics <- na.omit(subset(rio_csv, select = c(287)))

#write.xlsx(demographics, file = "demographics.xlsx",
#           sheetName = "p10", append = TRUE)

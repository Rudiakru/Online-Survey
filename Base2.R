###Load packages(pacman)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, janitor, lubridate, psych,
               plotly, purrr, readr, rio, repurrrsive, rmarkdown, shiny, stringr,survey, tidyr, tidyverse)
install.packages("xlsx")
library("xlsx")

###Importing with rio

rio_csv1 <- import("")
rio_csv2 <- import("")
weighting <- import("")
rio_csv <- rbind(rio_csv1,rio_csv2,by='responseid')
rio_csv <- rio_csv[-c(2302),] 

###Q1g_1

dataset <- na.omit(subset(rio_csv, select = c(257, 112:122)))


###----------------------------###     Choosing the question     ###----------------------------###

#dataset <- na.omit(subset(rio_csv, select=c(268, 287)) ) #For Question 
#dataset <- na.omit(subset(dataset, subset = (p10 == 1 | p10 == 2)))   



#dataset <- na.omit(subset(dataset, subset = (q2j == 1)))

#dataset <- na.omit(subset(dataset, subset = (qhidagree20 == 1)))
#dataset <- na.omit(subset(dataset, subset = (p2 == 16))) #math

i.dataset <- dataset %>% 
  relocate(p1, .before = q1g_1_1) #Take p1 to first column

data1 <- cbind(i.dataset, total = rowSums(i.dataset[2:12])) 

data2 <- data1[,c(1,13)]

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

delete.na <- function(df, n=2) {
  df[rowSums(is.na(df)) <= n,]
}

#Funktion wird auf summed_countries_and_other angewendet
summed_countries_and_other<-delete.na(summed_countries_and_other)

names(summed_countries_and_other)[1] <- 'Group.1'
#-----------------------#Weigthing function in action#-----------------------#

df1<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 211) * 0.78908105
df2<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 44) * 1.60452354
df3<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 187) * 0.22373039
df4<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 101) * 0.23756335
df5<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 210) * 0.56830668
df6<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 157) * 0.01410014
df7<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 78) * 4.83180385
df8<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 166) * 1.16706223
df9<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 29) * 0.58679816
df10<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 103) * 2.27165306
df11<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 72) * 1.13045772
df12<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 38) * 0.64045050
df13<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 130) * 0.17591730
df14<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 95) * 1.30703639
df15<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 10) * 0.31866250
df16<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 13) * 0.63602982
df17<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 160) * 0.76977804
df18<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 141) * 0.59490204
df19<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 193) * 0.35232179
df20<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == 185) * 0.27869008
df21 <- summed_countries_and_other %>% filter_all(any_vars(is.na(.)))
df21 <- df21 * 0.83257628


#Hier die ?nderung, benutzen wenn summed_countries_and_other nur ZWEI KOLUMNEN hat!!
df22<-summed_countries_and_other %>% filter(summed_countries_and_other$Group.1 == "Other") * 0.83257628

#Normal
#df22<-summed_countries_and_other[,-1] %>% filter(summed_countries_and_other$Group.1 == 'Other') * 0.83257628
#df22<- cbind('Other', df22) 
#names(df22)[1] <- 'Group.1'

weightedAnswers<-bind_rows(get0("df1"),get0("df2"),get0("df3"),get0("df4"),get0("df5"),
                           get0("df6"),get0("df7"),get0("df8"),get0("df9"),get0("df10"),get0("df11"),get0("df12"),get0("df13"),
                           get0("df14"),get0("df15"),get0("df16"),get0("df17"),get0("df18"),get0("df19"),get0("df20"),get0("df21"),get0("df22")) 




weightedAnswers <- weightedAnswers[, -1] # Take first column out

#Hier ?nderung 2
weightedAnswers <- as.data.frame(weightedAnswers)


weightedAnswers <- cbind(summed_countries_and_other$Group.1,weightedAnswers)  #give the names of Group1 from Na into weightframe
names(weightedAnswers)[1] <- "Countries" #Rename first column

#-----------------------#Weighted total per Anwser#-----------------------#

totalWeightedAnswers.dataframe <-  weightedAnswers %>%  #Sum up ALL answers for each column and make new row with "Total per Answer" for every question
  adorn_totals("row", name = "Total_per_Answer")
totalWeightedAnswersRounded.dataframe<- adorn_rounding(totalWeightedAnswers.dataframe, digits = 0, rounding = "half up")



#demographics <- na.omit(subset(rio_csv, select = c(287)))

#write.xlsx(demographics, file = "demographics.xlsx",
#           sheetName = "p10", append = TRUE)


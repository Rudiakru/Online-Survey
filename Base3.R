### Data for the loop
#install.packages("xlsx")
library("xlsx")

Countries <- levels(as.factor(i.dataset5$p1)) #Make the loop for all the countries p1

#countries <-c("211","44", "187", "101", "210", "157", "78", "166", "29", "103", "72", "38", "130", "95", "10", "195","13","160","141", "193" ) Only for the 20 countries we need

ncol(i.dataset5) #How many columns 

QuestionIndices<-(2:9)#Change here, could automate but not required

#QuestionIndices <-(2:10)                      #Go from column 2 to 46, so from question q1c1 till p10

Questionnames <- names(i.dataset5)[QuestionIndices] #take the columnames from QuestionIndices and put them in Questionames

ListResults <- NULL #Make variable with NULL

results2 <-NULL #for changing columns

i.dataset5[i.dataset5 == 0] <- NA   
#Summing up with three loops

#First Loop

for (Counter in 1:length(QuestionIndices)){ #Go from 1 till the end of Questionindices (having the integer by length)
  
  AnswerOptions <- levels(as.factor(i.dataset5[,QuestionIndices[Counter]])) 
  #Go trough the columns of Questionindices by Counter, by having i.dataset before makes it a list, level as factors, so every different data make as factor
  #Result:chr [1:8] "1" "2" "3" "4" "5" "6" "7" "99" # Get Simple list
  results2[Counter] <-list(AnswerOptions) ##For changing columns
  
  Results <-matrix(ncol=length(AnswerOptions),nrow=length(Countries),dimnames=list(Countries,c(AnswerOptions)))
  #Returns matrices with number of columns through the length of the list, number of rows is length of Countries. Then give the columns and rows the names
  #Result: 
  #logi [1:100, 1:8] NA NA NA NA NA NA ...
  #- attr(*, "dimnames")=List of 2
  #..$ : chr [1:100] "3" "10" "11" "13" ...
  #..$ : chr [1:8] "1" "2" "3" "4" ...
  
  
  ListResults<-unlist(list(ListResults, list(Results)), recursive=FALSE)
  #Result:
  #List of 284
  #$ : int [1:100, 1:8] 1 17 2 18 4 0 1 1 1 7 ...
  #..- attr(*, "dimnames")=List of 2
  #.. ..$ : chr [1:100] "3" "10" "11" "13" ...
  #.. ..$ : chr [1:8] "1" "2" "3" "4" ...
  
  #Second loop
  
  for (Country in Countries){
    
    
    
    Subset.Country <- subset(i.dataset5, p1 == Country)
    
    #Third loop
    
    for (Answer in AnswerOptions){
      
      ListResults[[Counter]][Country,Answer] <- length(which(Subset.Country[,QuestionIndices[Counter]] == Answer))
      
    }
    
    
    
  }
  
  
  
}
###End of the loop and you get ListResults, which are 45 listed lists


names(ListResults)<-Questionnames #Give them names



#Change list of lists into dataframe
ListResults2<-map_dfr(ListResults, ~as.data.frame(t(.x))) #use purrr to change the lists into a dataframe
condensedData <- as.data.frame(t(ListResults2)) #now exchange the rows and columns by turning into table and then again into dataframe

#Change the name of the columns
names(results2) <-Questionnames  #Changes the name of results2

#Makes the data frame with right names, need library purrr which is already at the start of the code
names(condensedData) <- imap(results2, paste, sep = '_') %>% flatten_chr()



#-----------------------#Dataset from excel#-----------------------#

weighting <-na.omit(weighting[1:10]) #Without the nas
i.dataset2 <- weighting %>% select(7,10) #Choose p1 and wght from the
i.dataset3 <- i.dataset2[1:20,]

#-----------------------#Make a dataset with the right order#-----------------------#
library(dplyr)
condensedData <- tibble::rownames_to_column(condensedData, "Group.1") #Make the Group1 from the rowindeces

#library(data.table)
#setDT(condensedData, keep.rownames = "Group.1")[] ##Make Group1 from rownames # das m?sste auch gehen


x <- (weighting$`country by p1`) #Returns list in the right order for the countries

condensedData2 <- condensedData %>% #through comparison of x brings Group1 in the right order, although the rest is made na's
  mutate(Group.1 =  factor(Group.1, levels = x)) %>%
  arrange(Group.1) 


#-----------------------#Summing up the rest#-----------------------#

dat1 <- condensedData2[1:20,] #First split condensedData2 by rows in two sets
dat2 <- condensedData2[21:100,]

Other <- dat2 %>% #In dat2 the "Other" get summed up as one variable
  adorn_totals("row", name= "Other")


#-----------------------#Making the complete data.frame#-----------------------#

dat3 <-Other[84,]
dat3 <- Other[81,] #Take the whole row of "Other"

summed_countries_and_other <-rbind(dat1, dat3) #bind summed "Other countries" and summed up 20 countries

intermediate.dataframe <-  summed_countries_and_other %>%  #Sum up ALL answers for each column and make new row with "Total" for every Answer
  adorn_totals("row", name = "Total_per_Answer")


#-----------------------#Weigthing function in action#-----------------------#
###Fehler


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
df22<-summed_countries_and_other[,-1] %>% filter(summed_countries_and_other$Group.1 == 'Other') * 0.83257628
df22<- cbind('Other', df22) 
names(df22)[1] <- 'Group.1'


#weightedAnswers <- mget(ls(pattern = 'df[0-9]*'))
#weightedAnswers <- weightedAnswers[sapply(weightedAnswers, is.data.frame)]
#dfg_all <- do.call(rbind, weightedAnswers)
#dfg_all <- do.call(rbind, mget(ls(pattern = 'dfg[0-9]*')))

#weigthedAnswers<-bind_rows(get0(df1),df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df13,df14,df15,df16,df17,df18,df19,df20,df21,df22)) 
weightedAnswers<-bind_rows(get0("df1"),get0("df2"),get0("df3"),get0("df4"),get0("df5"),
                           get0("df6"),get0("df7"),get0("df8"),get0("df9"),get0("df10"),get0("df11"),get0("df12"),get0("df13"),
                           get0("df14"),get0("df15"),get0("df16"),get0("df17"),get0("df18"),get0("df19"),get0("df20"),get0("df21"),get0("df22")) 

#weightedAnswers <- as.data.frame(mapply('*', data.frame(summed_countries_and_other), data.frame(i.dataset2[2])))
weightedAnswers <- weightedAnswers[, -1] # Take first column out
weightedAnswers <- cbind(summed_countries_and_other$Group.1,weightedAnswers)  #give the names of Group1 from Na into weightframe
names(weightedAnswers)[1] <- "Countries" #Rename first column


#-----------------------#Weighted total per Anwser#-----------------------#


totalWeightedAnswers.dataframe<- adorn_rounding(weightedAnswers, digits = 0, rounding = "half up")
totalWeightedAnswers.dataframe <-  totalWeightedAnswers.dataframe %>%  #Sum up ALL answers for each column and make new row with "Total per Answer" for every question
  adorn_totals("row", name = "Total_per_Answer")
#write.xlsx(totalWeightedAnswers.dataframe, file = "weightedQ1eii.xlsx",
#          sheetName = "q1d_testRudi2", append = TRUE)

#-----------------------#Weighted results in % #-----------------------#


#totalWeightedAnswers1.dataframe <- totalWeightedAnswers.dataframe[,-1] # Take first column out
#allSharedWeightedAnswers <- (totalWeightedAnswers1.dataframe/2022)*100

#allSharedWeightedAnswers <- cbind(totalWeightedAnswers.dataframe$Countries,allSharedWeightedAnswers)  #give the names of Countries from Na into allSharedWeightedAnswers
#names(allSharedWeightedAnswers)[1] <- "Countries" #Rename first column
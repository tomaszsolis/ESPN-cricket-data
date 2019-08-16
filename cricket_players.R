getwd()
unzip("espn-cricket-players-data.zip")

library(data.table)
library(tidyverse)

cricdata<- data.table(read.csv("cricket_data.csv", stringsAsFactors=FALSE))

head(cricdata)
colnames(cricdata)
summary(cricdata)
str(cricdata)
##############################################################################
#he top 10 countries ranked by ‘Test Runs’ scored;
##############################################################################
#cleaning the data in Bowling Test Runs
cricdata$BOWLING_Tests_Runs[cricdata$BOWLING_Tests_Runs==""]<-0
cricdata$BOWLING_Tests_Runs[cricdata$BOWLING_Tests_Runs=="-"]<-0
cricdata$BOWLING_Tests_Runs<-as.numeric(cricdata$BOWLING_Tests_Runs)

#cleaning the data in Batting Test Runs
cricdata$BATTING_Tests_Runs[cricdata$BATTING_Tests_Runs==""]<-0
cricdata$BATTING_Tests_Runs[cricdata$BATTING_Tests_Runs=="-"]<-0
cricdata$BATTING_Tests_Runs<-as.numeric(cricdata$BATTING_Tests_Runs)

#summarising the test runs
cricdata$Test_Runs_TOTAL <- cricdata$BOWLING_Tests_Runs + cricdata$BATTING_Tests_Runs

#aggregating by country
TestRuns<-aggregate(Test_Runs_TOTAL~COUNTRY, cricdata, sum)
TestRuns<-TestRuns[order(TestRuns$Test_Runs_TOTAL, decreasing = TRUE),]

#top 10
top10TestRuns<-head(TestRuns, 10)
print(top10TestRuns, row.names = FALSE)
##############################################################################
#Top players who have played for England as one of their major teams, 
#who have a ‘Test Batting Average’ greater than or equal to 30, 
#but have batted in 100 or more ‘Batting Test Inns’, ranked by 
#sum of ‘Test Runs’;
##############################################################################
#missing values 
cricdata$BATTING_Tests_Ave[cricdata$BATTING_Tests_Ave==""]<-0
cricdata$BATTING_Tests_Ave[cricdata$BATTING_Tests_Ave=="-"]<-0
cricdata$BATTING_Tests_Ave<-as.numeric(cricdata$BATTING_Tests_Runs)

cricdata$BATTING_Tests_Inns[cricdata$BATTING_Tests_Inns==""]<-0
cricdata$BATTING_Tests_Inns[cricdata$BATTING_Tests_Inns=="-"]<-0
cricdata$BATTING_Tests_Inns<-as.numeric(cricdata$BATTING_Tests_Inns)
#filtering
TopEngland<-cricdata[cricdata$Major.teams %like% "England" & 
                             cricdata$BATTING_Tests_Ave >=30 &
                             cricdata$BATTING_Tests_Inns>=100,]
#reducing the number of columns
TopEngland <- select(TopEngland,"NAME", "Test_Runs_TOTAL")
#results - ordered
TopEngland<-TopEngland[order(TopEngland$Test_Runs_TOTAL, decreasing = TRUE)]

##############################################################################
# a list of countries whose cricketers have won the “Wisden Cricketer of the Year” including;
#The number of times players from these countries have won;
#How this looks as a percentage;
#The stats on whether the cricketer is likely to be a right-hand batter 
#or left-hand batter.
##############################################################################
#filtering
cricdata%>%
        filter(grepl("Wisden Cricketer of the Year", AWARDS, fixed = TRUE))%>%
        select("COUNTRY", "Batting.style")->task3
#awards per country
task3%>% 
        group_by(COUNTRY) %>%
        summarise(noOfAwards = length(COUNTRY))%>%
        mutate(percent = noOfAwards / sum(noOfAwards)*100)->task3a
#Batting style data
task3b<- as.data.frame(table(task3))
task3b_left<-task3b[task3b$Batting.style == "Left-hand bat",c("COUNTRY", "Freq")]
task3b_right<-task3b[task3b$Batting.style == "Right-hand bat",c("COUNTRY", "Freq")]
#adding proper names for batting style columns
colnames(task3b_left)<-c("COUNTRY", "Left-hand bat")
colnames(task3b_right)<-c("COUNTRY", "Right-hand bat")
#merging of the data filtered and manipulated above
merge(task3a, task3b_left, by = "COUNTRY")%>%
        merge(task3b_right, by = "COUNTRY")->task3_result
#ordering to receive final result
task3_result[order(task3_result$percent, decreasing = TRUE),]

##############################################################################
#A list of Top 10 ‘Bowling Tests Wickets’, by player, showing their country, 
#and their bowling style;
##############################################################################
#missing values
cricdata$BOWLING_Tests_Wkts[cricdata$BOWLING_Tests_Wkts==""]<-0
cricdata$BOWLING_Tests_Wkts[is.na(cricdata$BOWLING_Tests_Wkts)]<-0
cricdata$BOWLING_Tests_Wkts[cricdata$BOWLING_Tests_Wkts=="-"]<-0
cricdata$BOWLING_Tests_Wkts <- as.numeric(cricdata$BOWLING_Tests_Wkts)
#reducing the number of columns
task4<- select(cricdata,"NAME", "BOWLING_Tests_Wkts", "Bowling.style")[order(BOWLING_Tests_Wkts, decreasing = TRUE)]
#top 10
task4<-head(task4, 10)

##############################################################################
#A list of Top 5 English ‘Test Bowlers’ who are still alive, ranked by 
#‘Bowling Tests Wickets’, who played for Cambridge University as a major team.
##############################################################################

#data filtering (data were cleared earlier - point 4)
cricdata[cricdata$Died==""]%>%
        filter(grepl("Cambridge University", Major.teams, fixed = TRUE))%>%
        select(c("NAME", "BOWLING_Tests_Wkts"))->task5
#ordering
task5<-task5[order(task5$BOWLING_Tests_Wkts, decreasing = TRUE),]
#top 5
task5<-head(task5, 5)
print(task5, row.names = FALSE)
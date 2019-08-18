getwd()
unzip("espn-cricket-players-data.zip")

library(data.table)
library(tidyverse)

cricdata<- data.table(read.csv("cricket_data.csv", stringsAsFactors=FALSE))
#general overview of the data
dim(cricdata)
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

#aggregating by country and show top 10
aggregate(Test_Runs_TOTAL~COUNTRY,cricdata, sum)%>%
        arrange(desc(Test_Runs_TOTAL))%>%
        head(n=10)

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
#filtering and reducing the number of columns
cricdata%>%
        filter(Major.teams %like% "England",
               BATTING_Tests_Ave >=30, 
               BATTING_Tests_Inns>=100)%>%
        select(NAME, Test_Runs_TOTAL)%>%
        arrange(desc(Test_Runs_TOTAL))%>%
        head(n=10)
    
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
        select("COUNTRY", "Batting.style")->wca
#awards per country
wca%>% 
        group_by(COUNTRY) %>%
        summarise(noOfAwards = length(COUNTRY))%>%
        mutate(percent = noOfAwards / sum(noOfAwards)*100)%>%
        select(COUNTRY, percent, noOfAwards)->wcaPart1
#Batting style data
wcaPart2<- as.data.frame(table(wca))
wcaPart2Left<-wcaPart2[wcaPart2$Batting.style == "Left-hand bat",c("COUNTRY", "Freq")]
wcaPart2Right<-wcaPart2[wcaPart2$Batting.style == "Right-hand bat",c("COUNTRY", "Freq")]
#adding proper names for batting style columns
colnames(wcaPart2Left)<-c("COUNTRY", "Left-hand bat")
colnames(wcaPart2Right)<-c("COUNTRY", "Right-hand bat")
#merging and ordering of the data filtered and manipulated above
merge(wcaPart1, wcaPart2Left, by = "COUNTRY")%>%
        merge(wcaPart2Right, by = "COUNTRY")%>%
        arrange(desc(percent))

##############################################################################
#A list of Top 10 ‘Bowling Tests Wickets’, by player, showing their country, 
#and their bowling style;
##############################################################################
#missing values
cricdata$BOWLING_Tests_Wkts[cricdata$BOWLING_Tests_Wkts==""]<-0
cricdata$BOWLING_Tests_Wkts[is.na(cricdata$BOWLING_Tests_Wkts)]<-0
cricdata$BOWLING_Tests_Wkts[cricdata$BOWLING_Tests_Wkts=="-"]<-0
cricdata$BOWLING_Tests_Wkts <- as.numeric(cricdata$BOWLING_Tests_Wkts)
#reducing the number of columns and selecting top 10
cricdata%>%
        select("NAME", "BOWLING_Tests_Wkts", "Bowling.style")%>%
        arrange(desc(BOWLING_Tests_Wkts))%>%
        head(n=10)
##############################################################################
#A list of Top 5 English ‘Test Bowlers’ who are still alive, ranked by 
#‘Bowling Tests Wickets’, who played for Cambridge University as a major team.
##############################################################################

#data filtering (data were cleared earlier - point 4)
#ordering
#top 5
cricdata%>%
        filter(Died=="",
               Major.teams %like% "Cambridge University")%>%
        select(c("NAME", "BOWLING_Tests_Wkts"))%>%
        arrange(desc(BOWLING_Tests_Wkts))%>%
        head(n=5)
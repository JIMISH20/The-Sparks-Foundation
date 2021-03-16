#Implemented by :- KHOLAPURE JIMISH PRAMOD
#THE SPARKS FOUNDATION (GRIPMAR2021)


#Installing the required Libraries
install.packages('readr')
install.packages("dplyr")
install.packages("tidyverse")

#Loading the Libraries
library(readr)
library(dplyr)
library(ggplot2)

#Loading the Datasets.
matches_data <- read.csv("matches.csv")
deliveries_data <- read.csv("deliveries.csv")

View(matches_data)
View(deliveries_data)

summary(matches_data)
summary(deliveries_data)

#Finding any missing values in the datasets
sum(is.na(matches_data))
sum(is.na(deliveries_data))

#Now lets do the analysis.


#1 Number of matches played per season

No_of_matches_per_season <- select(matches_data, season)
No_of_matches_per_season <- group_by(No_of_matches_per_season, season)
No_of_matches_per_season <- summarize(No_of_matches_per_season, n())
No_of_matches_per_season


#2 Most number of man of the match awards


Most_plyr_of_the_match_winner <- matches_data%>%select(player_of_match)%>%group_by(player_of_match)%>%summarize(total_count=n())%>%arrange(-total_count)
Most_plyr_of_the_match_winner<- Most_plyr_of_the_match_winner[1:5,]
Most_plyr_of_the_match_winner

write.csv(Most_plyr_of_the_match_winner, "MaxPlyrOfTheMatch.csv")

#3 team who won by maximum runs

win_by_max_runs <- matches_data%>%select(winner, win_by_runs)%>%group_by(winner)%>%summarize(total_runs=sum(win_by_runs))%>%arrange(-total_runs)
win_by_max_runs

write.csv(win_by_max_runs, "TeamWinByMaxRuns.csv")


#4 team who won by max wickets

win_by_max_wickets <- matches_data%>%select(winner, win_by_wickets)%>%group_by(winner)%>%summarize(tot_wickets=sum(win_by_wickets))%>%arrange(-tot_wickets)
win_by_max_wickets

write.csv(win_by_max_wickets, "TeamWinByMaxWickets.csv")


#5 batsman with most runs

most_runs_by_batsman <- deliveries_data%>%select(batsman, batsman_runs)%>%group_by(batsman)%>%summarize(tot_runs=sum(batsman_runs))%>%arrange(-tot_runs)
most_runs_by_batsman

write.csv(most_runs_by_batsman, "MostRunsByPlayer.csv")


#6 stadium/venue with max no of matches played

max_venue <-matches_data%>%select(venue)%>%group_by(venue)%>%summarize(no_of_matches_played=n())%>%arrange(-no_of_matches_played)
max_venue

write.csv(max_venue, "MaxVenue.csv")




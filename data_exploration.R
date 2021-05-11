# data exploration sports networks: shots

library(dplyr)
library(StatsBombR)
library(ggplot2)

StatsBombData <- StatsBombFreeEvents()


Comp <- FreeCompetitions()

# all matches in all comps
Matches <- FreeMatches(Comp)
names(Matches)

# filter only barca matches
BarcaMatches <- Matches[Matches$away_team.away_team_name=="Barcelona" | Matches$home_team.home_team_name=="Barcelona",]

# barca lineups
lineups <- StatsBombFreeLineups(MatchesDF = BarcaMatches)
lineups <- lineups[lineups$team_name=="Barcelona",]

'''
# games per player: to do
games_PerPlayer <- lineups %>%
'''

#Matches[Matches$away_team.away_team_name=="Barcelona" | Matches$home_team.home_team_name=="Barcelona",]

BarcaEvents <- StatsBombData[StatsBombData$team.name=="Barcelona",]
names(BarcaEvents)



##### number of xG
# xG = shot.statsbomb_xg
shots <- BarcaEvents[BarcaEvents$type.name=="Shot",]

ShotsPerGame <- 7742/448
ShotsPerGame # 17.28125

stats_PerPlayer <- shots %>%
  group_by(player.name) %>%
  summarise(total_shots = n(), total_goals = sum(shot.outcome.name=="Goal"), xG = sum(shot.statsbomb_xg))

'''
stats_PerPlayer_Per90 <- shots %>%
  group_by(player.name, match_id) %>%
  summarise(total_shots = n(), total_goals = sum(shot.outcome.name=="Goal"), xG = sum(shot.statsbomb_xg))
'''


# top 15 in total goals
top_15 <- top_n(stats_PerPlayer, 15, total_goals)

ggplot(top_15, aes(y=total_goals, x=xG, color=player.name)) +
  geom_point(size=3) +
  labs(title="Top 15 - Goals vs. xG") +
  ylab("Goals") +
  xlab("Expected Goals") +
  geom_abline(slope=1, intercept=0)
  
p <- ggplot(top_15, aes(x=player.name, y=total_shots)) +
  geom_bar(stat="identity") +
  labs(title="Top 15 Shot Takers") +
  ylab("Shots taken") +
  xlab("Player")
p + theme(axis.text.x = element_text(angle = 90, size=8))

p2 <- ggplot(goals_PerPlayer, aes(x=player.name, y=total_shots)) +
  geom_bar(stat="identity") +
  ylab("Total Goals")
p2 + theme(axis.text.x = element_text(angle = 90))

#combined <- cbind(goals_PerPlayer$total_shots, total_xG$xG)
#ggplot(data=cbind(goals_PerPlayer, total_xG))

# number of goals
# number of xA
# number of assists


### table with lineup, goals, match, season, minutes played together
# note: take minute to make it easy
# substitution.replacement.name

elapsed_events <- formatelapsedtime(BarcaEvents)
player_minutes <- get.minutesplayed(elapsed_events)

# also sort by alphabet the list?
"""
lineups_games <- player_minutes %>%
  group_by(match_id) %>%
  summarise(lineup_1=list(player.id[1:11]), time_1=min(TimeOff),
            lineup_2=list(player.id[1:11]!=player.id[order(TimeOff[2])]),
            time_2=TimeOff[order(TimeOff[2])]-min(TimeOff))
"""
lineups_games <- player_minutes %>%
  group_by(match_id) %>%
  summarise(lineup_1=list(player.id[1:11]), time_1=min(TimeOff),
            sub_1=min(TimeOff), lineup_2=list(player.id[1:12][-order(TimeOff)[1]]),
            time_2=TimeOff[order(TimeOff)[2]]-min(TimeOff),
            sub_2=TimeOff[order(TimeOff)[2]],
            lineup_3=list(player.id[1:13][-order(TimeOff)[1:2]]),
            time_3=TimeOff[order(TimeOff)[3]]-TimeOff[order(TimeOff)[2]],
            sub_3=TimeOff[order(TimeOff)[3]],
            lineup_4=list(player.id[1:14][-order(TimeOff)[1:3]]),
            time_4=TimeOff[order(TimeOff)[4]]-TimeOff[order(TimeOff)[3]])

# test
lineups_games$lineup_1[[1]]
lineups_games$lineup_2[[1]]
lineups_games$lineup_3[[1]]
lineups_games$lineup_4[[1]]

lineups_games[,"goals_1"] <- 0
lineups_games[,"goals_2"] <- 0
lineups_games[,"goals_3"] <- 0
lineups_games[,"goals_4"] <- 0


# add goals
all_goals <- shots[shots$shot.outcome.name=="Goal",]

goals_by_match <- all_goals %>%
  group_by(match_id, minute) %>%
  summarise(goals=n())

for (i in 1:nrow(goals_by_match)){
  id = goals_by_match[[i, 1]]
  min = goals_by_match[[i, 2]]
  
  #current_lineup = lineups_games[lineups_games$match_id==id,]
  t1 = lineups_games[lineups_games$match_id==id,]$sub_1
  t2 = lineups_games[lineups_games$match_id==id,]$sub_2
  t3 = lineups_games[lineups_games$match_id==id,]$sub_3
  
  if (min<t1){
    lineups_games[lineups_games$match_id==id,]$goals_1 = lineups_games[lineups_games$match_id==id,]$goals_1 + 1
  }
  else if (t1<min & min<t2){
    lineups_games[lineups_games$match_id==id,]$goals_2 = lineups_games[lineups_games$match_id==id,]$goals_2 + 1
  }
  else if (t2<min & min<t3){
    lineups_games[lineups_games$match_id==id,]$goals_3 = lineups_games[lineups_games$match_id==id,]$goals_3 + 1
  }
  else{
    lineups_games[lineups_games$match_id==id,]$goals_4 = lineups_games[lineups_games$match_id==id,]$goals_4 + 1
  }

}

library(data.table)
#long_lineups_games <- melt(lineups_games,  id.vars="match_id", measure.vars=c("lineup_1", "time_1", "goals_1"))

unlist(lineups_games[1,2])
lineups_unlisted <- cbind(lineups_games[!sapply(lineups_games, is.list)], 
                          (t(apply(lineups_games[sapply(lineups_games, is.list)], 1, unlist))))

write.csv(lineups_unlisted, "lineups_unlisted", row.names = FALSE)

## format change
## keep sorted to facilitate joins
lineups_games$lineup_1<-lapply(lineups_games$lineup_1,sort)
lineups_games$lineup_2<-lapply(lineups_games$lineup_2,sort)
lineups_games$lineup_3<-lapply(lineups_games$lineup_3,sort)
lineups_games$lineup_4<-lapply(lineups_games$lineup_4,sort)
## stack the 4 lineups
lu1<-select(lineups_games,c('match_id','lineup_1','time_1','goals_1')) %>% rename(lineup=lineup_1,time=time_1,goals=goals_1)
lu2<-select(lineups_games,c('match_id','lineup_2','time_2','goals_2')) %>% rename(lineup=lineup_2,time=time_2,goals=goals_2)
lu3<-select(lineups_games,c('match_id','lineup_3','time_3','goals_3')) %>% rename(lineup=lineup_3,time=time_3,goals=goals_3)
lu4<-select(lineups_games,c('match_id','lineup_4','time_4','goals_4')) %>% rename(lineup=lineup_4,time=time_4,goals=goals_4)
lineups_formated <- rbind(lu1,lu2,lu3,lu4)
## add match if necessary
lineups_formated_agg<-lineups_formated %>% group_by(lineup) %>% summarise(time=sum(time),goals=sum(goals))
lineups_formated_agg$id_lineup<-rownames(lineups_formated_agg)
save(lineups_formated_agg, file = "../data/processed/lineups_formated_agg.RData")


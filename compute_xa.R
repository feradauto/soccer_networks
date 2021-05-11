
library(dplyr)
library(StatsBombR)
library(ggplot2)
## load data
StatsBombData <- StatsBombFreeEvents()
Comp <- FreeCompetitions()
# all matches in all comps
Matches <- FreeMatches(Comp)

# filter only barca matches
BarcaMatches <- Matches[Matches$away_team.away_team_name=="Barcelona" | Matches$home_team.home_team_name=="Barcelona",]

# barca lineups
lineups <- StatsBombFreeLineups(MatchesDF = BarcaMatches)
lineups <- lineups[lineups$team_name=="Barcelona",]

BarcaEvents <- StatsBombData[StatsBombData$team.name=="Barcelona",]

##### get number of xG and join with key pass to get xA
passes <- BarcaEvents[BarcaEvents$type.name=="Pass",]
shots <- BarcaEvents[BarcaEvents$type.name=="Shot",]
shots_selected<-select(shots,c('match_id','player.id','shot.key_pass_id','shot.statsbomb_xg')) %>% 
  rename( player.id.shot=player.id )
passes_selected<-select(passes,c('id','match_id','player.id','pass.recipient.id')) %>% 
  rename(shot.key_pass_id=id, player.id.pass=player.id, player.id.shot=pass.recipient.id)
pass_shot<-merge(passes_selected, shots_selected, by = c("shot.key_pass_id","match_id","player.id.shot")) 
pass_shot<-pass_shot%>% group_by(match_id,player.id.shot,player.id.pass) %>% summarise(xA=sum(shot.statsbomb_xg))
pass_shot<-mutate(pass_shot, pair = mapply(c, player.id.pass, player.id.shot, SIMPLIFY = F))
pass_shot$pair<-lapply(pass_shot$pair,sort)


## time played by each lineup
elapsed_events <- formatelapsedtime(BarcaEvents)
player_minutes <- get.minutesplayed(elapsed_events)

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


library(data.table)
## keep sorted to facilitate joins
lineups_games$lineup_1<-lapply(lineups_games$lineup_1,sort)
lineups_games$lineup_2<-lapply(lineups_games$lineup_2,sort)
lineups_games$lineup_3<-lapply(lineups_games$lineup_3,sort)
lineups_games$lineup_4<-lapply(lineups_games$lineup_4,sort)
## stack the 4 lineups
lu1<-select(lineups_games,c('match_id','lineup_1','time_1')) %>% rename(lineup=lineup_1,time=time_1)
lu2<-select(lineups_games,c('match_id','lineup_2','time_2')) %>% rename(lineup=lineup_2,time=time_2)
lu3<-select(lineups_games,c('match_id','lineup_3','time_3')) %>% rename(lineup=lineup_3,time=time_3)
lu4<-select(lineups_games,c('match_id','lineup_4','time_4')) %>% rename(lineup=lineup_4,time=time_4)
lineups_time <- rbind(lu1,lu2,lu3,lu4)
lineups_time_agg<-lineups_time %>% group_by(lineup,match_id) %>% summarise(time=sum(time))
lineups_time_agg$id_lineup<-rownames(lineups_time_agg)

## unlist and then aggregate to get all possible combinations of 2 players among a lineup
unlist_func <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[!temp1], temp2)
}

lineups_time_agg_unlisted<-unlist_func(lineups_time_agg)
lu_melt<- melt(as.data.table(lineups_time_agg_unlisted),  id.vars=c("match_id","id_lineup","time"), measure.vars=c("lineup.1","lineup.2",
                                                                                                                         "lineup.3","lineup.4","lineup.5","lineup.6","lineup.7","lineup.8","lineup.9","lineup.10","lineup.11"))
lu_melt_combn<-lu_melt %>% group_by(match_id,id_lineup,time) %>% summarise(combn = list(combn(value, 2, toString))) %>%
  unnest(combn)
lu_melt_combn_agg<-lu_melt_combn %>% group_by(match_id,combn)  %>% summarise(time = sum(time)) 
#lu_melt_combn_agg<-lu_melt_combn %>% group_by(combn)  %>% summarise(time = sum(time)) 

lu_melt_combn_agg<-separate(lu_melt_combn_agg, col = combn, into = c("p1","p2"), sep = ", ")
lu_melt_combn_agg<-transform(lu_melt_combn_agg, p1 = as.numeric(p1), p2 = as.numeric(p2))

lu_melt_combn_agg<-mutate(lu_melt_combn_agg, pair = mapply(c, p1, p2, SIMPLIFY = F)) %>% select(match_id,pair,time)
## keep sorted for future join
lu_melt_combn_agg$pair<-lapply(lu_melt_combn_agg$pair,sort)


## join xA with time played per match and pair of players (order matters for xA)
time_xa<-merge(lu_melt_combn_agg, pass_shot, by = c("match_id","pair"))
save(time_xa, file = "../data/processed/time_xa.RData")


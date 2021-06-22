
library(dplyr)
library(StatsBombR)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggrepel)
load("../data/processed/lineups_formated_agg.RData")
load("../data/processed/time_xa2015.RData")
load("../data/processed/nickname_id.RData")

StatsBombData <- StatsBombFreeEvents()
Comp <- FreeCompetitions()
# all matches in all comps
Matches <- FreeMatches(Comp)

# filter only barca matches
BarcaMatches <- Matches[Matches$away_team.away_team_name=="Barcelona" | Matches$home_team.home_team_name=="Barcelona",]
BarcaMatches <- BarcaMatches[BarcaMatches$season.season_name=="2014/2015",]
BarcaMatches <- BarcaMatches[BarcaMatches$competition.competition_name=="La Liga",]
# barca lineups
lineups <- StatsBombFreeLineups(MatchesDF = BarcaMatches)
lineups <- lineups[lineups$team_name=="Barcelona",]

BarcaEvents <- StatsBombData[StatsBombData$team.name=="Barcelona",]
BarcaEvents <- BarcaEvents[BarcaEvents$match_id %in% BarcaMatches$match_id,]
## unlist and then aggregate to get all possible combinations of 2 players among a lineup
unlist_func <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[!temp1], temp2)
}

lineups_formated_agg_unlisted<-unlist_func(lineups_formated_agg)
library(reshape2)
library(data.table)
lu_melt_formated<- melt(as.data.table(lineups_formated_agg_unlisted),  id.vars=c("id_lineup","time"), measure.vars=c("lineup.1","lineup.2",
                                                                                                                     "lineup.3","lineup.4","lineup.5","lineup.6","lineup.7","lineup.8","lineup.9","lineup.10","lineup.11"))
lu_melt_form_combn<-lu_melt_formated %>% group_by(id_lineup,time) %>% summarise(combn = list(combn(value, 2, toString))) %>%
  unnest(combn)

lu_melt_form_combn<-separate(lu_melt_form_combn, col = combn, into = c("p1","p2"), sep = ", ")
lu_melt_form_combn<-transform(lu_melt_form_combn, p1 = as.numeric(p1), p2 = as.numeric(p2))
lu_melt_form_combn<-mutate(lu_melt_form_combn, pair = mapply(c, p1, p2, SIMPLIFY = F)) %>% select(id_lineup,pair)
## keep sorted for future join
lu_melt_form_combn$pair<-lapply(lu_melt_form_combn$pair,sort)

time_xa_agg<-time_xa %>% group_by(pair,player.id.shot,player.id.pass) %>% summarise(xA = sum(xA),time_pair=sum(time))
time_xa_agg$xA_90<-90*time_xa_agg$xA/time_xa_agg$time_pair

time_xa_agg<-time_xa_agg %>% ungroup()%>%select(pair,player.id.shot,player.id.pass,xA,time_pair,xA_90)

players<-unique(BarcaEvents%>%select(player.id,player.name))
players_pos<-BarcaEvents%>%select(id,player.id,player.name,position.name)%>% group_by(player.id,player.name,position.name) %>% summarise(count = n_distinct(id))
players_pos<-players_pos[order(players_pos$count,decreasing = TRUE),]
players_pos<-players_pos[!duplicated(players_pos$player.name),]
players_pos$label <- ifelse(grepl("back", players_pos$position.name, ignore.case = T), "Defender",
                    ifelse(grepl("midfield", players_pos$position.name, ignore.case = T), "Midfielder",
                           ifelse(grepl("keeper", players_pos$position.name, ignore.case = T), "Goalkeeper",
                                  ifelse(grepl("wing", players_pos$position.name, ignore.case = T), "Forward", 
                                         ifelse(grepl("forward", players_pos$position.name, ignore.case = T), "Forward", "Default")))))
players_pos<-players_pos%>% rename(player.id.pass=player.id,player.name.pass=player.name)%>%select(player.id.pass,position.name,label)
players_pos<-players_pos[!is.na(players_pos$player.id.pass),]

playerspass<-players%>% rename(player.id.shot=player.id,player.name.shot=player.name)
playersshot<-players%>% rename(player.id.shot=player.id,player.name.shot=player.name)
playerspass<-players%>% rename(player.id.pass=player.id,player.name.pass=player.name)
time_xa_agg<-merge(time_xa_agg, playerspass, by = c("player.id.pass"))
time_xa_agg<-merge(time_xa_agg, playersshot, by = c("player.id.shot"))
time_xa_agg<-time_xa_agg   %>% filter(time_pair>90)
time_xa_agg<-time_xa_agg   %>% filter(xA_90>0)
time_xa_agg<-time_xa_agg %>% select(player.id.shot,player.id.pass,xA,time_pair,xA_90,player.name.pass,player.name.shot)
#time_xa_agg<-merge(time_xa_agg, players_pos, by = c("player.id.pass","player.name.pass"))

players<-players[!is.na(players$player.id),]
links<-time_xa_agg %>% rename(from=player.id.pass,to=player.id.shot,player.pass=player.name.pass,player.shot=player.name.shot,weight=xA_90)
players<-players%>% rename(id=player.id,nom=player.name)

nickname<-nickname_id
players<-merge(nickname, players, by = c("id"))

players<-transform(players, id = as.character(id))
links<-transform(links, to = as.character(to),from = as.character(from))
grafo<-tbl_graph(players,links, directed=T)
players_mod<-mutate(players, indegree = degree(grafo, mode="in"))
players_mod<-mutate(players_mod, outdegree = degree(grafo, mode="out"))
players_mod<-mutate(players_mod, instrength = strength(grafo, mode="in"))
players_mod<-mutate(players_mod, outstrength = strength(grafo, mode="out"))

players_posn<-players_pos%>% rename(id=player.id.pass,nom=player.name.pass)
players_mod_pos<-merge(players_mod, players_posn, by = c("id","nom"))
#players_mod_pos$Colour[players_mod_pos$label=="Forwarder"]="red"
#players_mod_pos$Colour[players_mod_pos$label=="Midfielder"]="blue"
#players_mod_pos$Colour[players_mod_pos$label=="Defender"]="green"
#players_mod_pos$Colour[players_mod_pos$label=="Goalkeeper"]="black"
# Plot all points at once, using newly generated colours
#plot(players_mod_pos$instrength,players_mod_pos$outstrength, col=players_mod_pos$Colour)
nickname_pass<-nickname_id%>% rename(player.id.pass=id,nickname.pass=nickname)
nickname_shot<-nickname_id%>% rename(player.id.shot=id,nickname.shot=nickname)

time_xa_agg<-merge(time_xa_agg, nickname_pass, by = c("player.id.pass"))
time_xa_agg<-merge(time_xa_agg, nickname_shot, by = c("player.id.shot"))


players_mod_pos<-players_mod_pos%>%rename(Position=label)

save(players_mod_pos, file = "../data/processed/players_mod_pos.RData")

ggplot(players_mod_pos, aes(y=outstrength, x=instrength, color=Position, label=nickname)) +geom_point(size=2) +geom_abline(slope=1, intercept=0) +
  labs(title="In strength vs Out strength") +
  ylab("Out strength") +
  xlab("In strength") + geom_text( size=6) +
  theme_bw()

ggplot(players_mod_pos, aes(y=outstrength, x=instrength, color=Position, label=nickname)) +geom_point(size=2) +geom_abline(slope=1, intercept=0) +
  labs(title="In strength vs Out strength") +
  ylab("Out strength") +
  xlab("In strength") + geom_text_repel(size=6) +
  theme_bw()

write.csv(players_mod,"../data/processed/degrees2015.csv",row.names = FALSE)
write.csv(time_xa_agg,"../data/processed/xA_2015.csv",row.names = FALSE)
write.csv(players_pos,"../data/processed/players_2015.csv",row.names = FALSE)

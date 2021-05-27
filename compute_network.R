
library(dplyr)
library(StatsBombR)
library(ggplot2)

load("../data/processed/lineups_formated_agg.RData")
load("../data/processed/time_xa.RData")
## unlist and then aggregate to get all possible combinations of 2 players among a lineup
unlist_func <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[!temp1], temp2)
}

lineups_formated_agg_unlisted<-unlist_func(lineups_formated_agg)
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
playerspass<-players%>% rename(player.id.shot=player.id,player.name.shot=player.name)
playersshot<-players%>% rename(player.id.shot=player.id,player.name.shot=player.name)
playerspass<-players%>% rename(player.id.pass=player.id,player.name.pass=player.name)
time_xa_agg<-merge(time_xa_agg, playerspass, by = c("player.id.pass"))
time_xa_agg<-merge(time_xa_agg, playersshot, by = c("player.id.shot"))
time_xa_agg<-time_xa_agg   %>% filter(time_pair>45)
write.csv(time_xa_agg,"xA_2019.csv",row.names = FALSE)
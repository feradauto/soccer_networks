
library(dplyr)
library(StatsBombR)
library(ggplot2)

load("../data/processed/lineups_formated_agg.RData")
load("../data/processed/time_xa.RData")
load("../data/processed/lineup_season.RData")
## unlist and then aggregate to get all possible combinations of 2 players among a lineup
unlist_func1 <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[!temp1], temp2)
}
unlist_func <- function(data) {
  temp1 <- plyr::ldply(data[[1]], rbind)
  temp <- cbind(data[,2:4], temp1)
  colnames(temp) <- c("time","goals","id_lineup",
                      "lineup.1","lineup.2", "lineup.3","lineup.4",
                      "lineup.5","lineup.6","lineup.7","lineup.8",
                      "lineup.9","lineup.10","lineup.11")
  return(temp)
} 


lineups_formated_agg<-lineups_formated_agg   %>% filter(time>0)
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
#time_xa_agg$xA_90<-90*time_xa_agg$xA/time_xa_agg$time_pair

lineup_pairs<-merge(lineups_formated_agg[c("id_lineup","lineup")], lu_melt_form_combn, by = c("id_lineup"))

unlist_func_pairs <- function(data) {
  temp1 <- "pair"
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[, -which(names(data) %in% c("pair"))], temp2)
}

lineup_pairs<-unlist_func_pairs(lineup_pairs)
time_xa_agg<-cbind(time_xa_agg[, -which(names(time_xa_agg) %in% c("pair"))],lapply(time_xa_agg["pair"], function(x) 
  data.frame(do.call(rbind, x), check.names=FALSE)))  %>% rename(pair.1='1',pair.2='2')

lineup_pairs_xa<-merge(lineup_pairs, time_xa_agg, by = c("pair.1","pair.2"))
lineup_pairs_xa$xA_90<-90*lineup_pairs_xa$xA/lineup_pairs_xa$time_pair
xA_goals<-lineup_pairs_xa %>% group_by(id_lineup,lineup) %>% summarise(xA_90 = sum(xA_90))
lineups_formated_agg<-lineups_formated_agg   %>% filter(time>1)
lineups_formated_agg$goals_90<-90*lineups_formated_agg$goals/lineups_formated_agg$time
xA_goals<-merge(xA_goals, lineups_formated_agg, by = c("id_lineup","lineup"))


unlist_season <- function(data) {
  temp1 <- plyr::ldply(data[[1]], rbind)
  temp <- cbind(data[,2:3], temp1)
  colnames(temp) <- c("season.season_name","time",
                      "lineup.1","lineup.2", "lineup.3","lineup.4",
                      "lineup.5","lineup.6","lineup.7","lineup.8",
                      "lineup.9","lineup.10","lineup.11")
  return(temp)
} 

idl<-lineups_formated_agg_unlisted%>%select(id_lineup,lineup.1,lineup.2,lineup.3,lineup.4,lineup.5,lineup.6,lineup.7,lineup.8,lineup.9,lineup.10,lineup.11)
lineup_season<-unlist_season(lineup_season)
line_id<-merge(idl, lineup_season, by = c("lineup.1","lineup.2", "lineup.3","lineup.4","lineup.5","lineup.6","lineup.7","lineup.8","lineup.9","lineup.10","lineup.11"))
line_id<-line_id[order(line_id$time),]
line_id <- line_id %>% 
  group_by(id_lineup) %>% 
  filter(time == max(time)) %>%
  distinct
line_id <- line_id %>% select(id_lineup,season.season_name)

xA_goals<-merge(xA_goals, line_id, by = c("id_lineup"))

xA_goals$type[xA_goals$season.season_name=="2004/2005"]="League"
xA_goals$type[xA_goals$season.season_name=="2005/2006"]="League"
xA_goals$type[xA_goals$season.season_name=="2006/2007"]="No League"
xA_goals$type[xA_goals$season.season_name=="2007/2008"]="No League"
xA_goals$type[xA_goals$season.season_name=="2008/2009"]="Trible"
xA_goals$type[xA_goals$season.season_name=="2009/2010"]="League"
xA_goals$type[xA_goals$season.season_name=="2010/2011"]="Treble"
xA_goals$type[xA_goals$season.season_name=="2011/2012"]="No League"
xA_goals$type[xA_goals$season.season_name=="2012/2013"]="League"
xA_goals$type[xA_goals$season.season_name=="2013/2014"]="No League"
xA_goals$type[xA_goals$season.season_name=="2014/2015"]="Treble"
xA_goals$type[xA_goals$season.season_name=="2015/2016"]="League"
xA_goals$type[xA_goals$season.season_name=="2016/2017"]="No League"
xA_goals$type[xA_goals$season.season_name=="2017/2018"]="League"
xA_goals$type[xA_goals$season.season_name=="2018/2019"]="League"
xA_goals$type[xA_goals$season.season_name=="2019/2020"]="No League"



plot_colorByDensity = function(x1,x2,
                               ylim=c(min(x2),max(x2)),
                               xlim=c(min(x1),max(x1)),
                               xlab="",ylab="",main="") {
  
  df <- data.frame(x1,x2)
  x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
  df$dens <- col2rgb(x)[1,] + 1L
  cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
  df$col <- cols[df$dens]
  plot(x2~x1, data=df[order(df$dens),], 
       ylim=ylim,xlim=xlim,pch=20,col=col,
       cex=2,xlab=xlab,ylab=ylab,
       main=main)
}
xA_goalsm<-xA_goals   %>% filter(time>45)
cor(xA_goalsm$xA_90,xA_goalsm$goals_90)
fit <- lm(goals_90 ~ xA_90, data=xA_goalsm)
summary(fit)
plot_colorByDensity(xA_goalsm$xA_90,xA_goalsm$goals_90,main = "Score vs goals",ylab="Goals 90 min",xlab="Lineup score")

ggplot(xA_goalsm%>% filter(time<600), aes(y=goals_90, x=xA_90,color=time)) +
  geom_point(size=3) +
  labs(title="Lineup score vs goals") +
  ylab("Goals 90 min") +
  xlab("Lineup score")

ggplot(xA_goalsm%>% filter(time<600), aes(y=goals_90, x=xA_90,color=season.season_name)) +
  geom_point(size=3) +
  labs(title="Lineup score vs goals") +
  ylab("Goals 90 min") +
  xlab("Lineup score")

ggplot(xA_goalsm, aes(y=goals_90, x=xA_90)) +
  geom_point(size=3) +
  labs(title="Lineup score vs Goals per 90 min") +
  ylab("Goals 90 min") +
  xlab("Lineup score") +
  theme_bw()+ abline(0.3894, 1.3274,color='black')


ggplot(xA_goalsm, aes(y=goals_90, x=xA_90)) +
  labs(title="Lineup score vs goals") +
  ylab("Goals 90 min") +
  xlab("Lineup score") + 
  geom_point() + theme_bw()+
  stat_smooth(method = "lm", col = "red")


require(pscl)
m1 <- zeroinfl(goals ~ time + xA_90 |goals-1,
               data = xA_goalsm, dist = "negbin")
summary(m1)

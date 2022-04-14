install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)

Players_stats <- read_csv("Stats.csv")
View(Players_stats)

Players_stats_2 <- read_csv("Stats3.csv")
View(Players_stats_2)

Stats1_clean <- Players_stats[!duplicated(Players_stats$`Player Name`), ]

Stats2_clean <- Players_stats_2[!duplicated(Players_stats_2$Player), ]
View(Stats2_clean)

#print(paste("Removed", nrow(Stats_2) - nrow(Stats2_clean),"duplicated rows"))

Stats1_clean[238,1] <- "Marc Albrighton"
View(Stats1_clean)

Team_Defense_2017_18 <- read_csv("Team Defense 2017-18.csv")

PL_Defense_stats <- Team_Defense_2017_18 %>% filter(League=="Premier League")

colnames(PL_Defense_stats) <- c("League","Goals.Against","Team","GA.Home","GA.Away","GA.First.Half","GA.Second.Half","GA.First.15.mins","GA.Last.10.mins","GA.Home.Matches.Scored.In","GA.Away.Matches.Scored.In","GA.Pld","GA.Per.Match","Season","KEY")

View(PL_Defense_stats)

barplot(height = PL_Defense_stats$Goals.Against,names.arg = PL_Defense_stats$Team,main = "Amount Of Goals conceded by each team",ylab = "Goals Conceded",las=2)

barplot(height = PL_Defense_stats$GA.Per.Match,names.arg = PL_Defense_stats$Team,main = "Goals Against Per Match",ylab = "GA Per Match",las=2)

barplot(height = PL_Defense_stats$GA.Last.10.mins,names.arg = PL_Defense_stats$Team,main = "Goals Conceded in the last 10 minutes",ylab = "Goals Conceded",las=2)

barplot(height = PL_Defense_stats$GA.First.Half,names.arg = PL_Defense_stats$Team,main = "Goals Conceded in the first half",ylab = "Goals Conceded",las=2)

barplot(height = PL_Defense_stats$GA.Home,names.arg = PL_Defense_stats$Team,main = "Goals Conceded at Home",ylab = "Goals Conceded",las=2)

barplot(height = PL_Defense_stats$GA.Away,names.arg = PL_Defense_stats$Team,main = "Goals Conceded away from Home",ylab = "Goals Conceded",las=2)


goalkeeper_df <- Stats2_clean %>% 
  filter(Position=="Goalkeeper") %>% 
  group_by(Team) %>% 
  drop_na(CS) %>% 
  summarize(total_CS = sum(CS))

View(goalkeeper_df)

barplot(height = goalkeeper_df$total_CS,names.arg = goalkeeper_df$Team,main = "Clean sheets Kept",ylab = "Clean Sheets Kept",las=2)


discipline_df <- Stats2_clean %>% 
  filter(Position=="Defender") %>% 
  group_by(Team) %>%
  drop_na(Yellow.Cards) %>%
  summarise(total_yellows = sum(Yellow.Cards))

View(discipline_df)

#plot(discipline_df$total_yellows,type = "o",col = "yellow", xlab = "Players", ylab = "Number of cards")
#lines(discipline_df$total_reds,type = "o",col = "red")
#legend(7.35,8, legend=c("Yellow","Red"), col=c("yellow","red"),lty = 1:2,cex = 0.8,box.lty = 0 , title="Cards")


str(Stats2_clean)

discipline_df2 <- Stats2_clean %>% 
  filter(Position=="Defender") %>% 
  group_by(Team) %>%
  drop_na(Sent.Off) %>%
  summarise(total_reds = sum(Sent.Off))

barplot(height = discipline_df$total_yellows,names.arg = discipline_df$Team,main = "Yellow Cards Received",ylab = "Cards",las=2,col = "yellow",ylim = c(0,35))

barplot(height = discipline_df2$total_reds,col = "red",names.arg = discipline_df2$Team,main = "Red Cards Received",ylab = "Cards",las=2,ylim = c(0,5))

goalkeeper_df <- Stats1_clean %>% 
  filter(Position=="Goalkeeper")  

gks_scored <- mutate(goalkeeper_df, ovr_score = 0 - (1*Errors.Leading.To.Goal) + (2.5*Assists) - (3*Red.Cards) + (3*Penalties.Saved) - (0.9*Fouls) + (0.2*Appearances) - (1*Own.Goals) + (0.8*Blocked.Shots) - (1*Goals.Conceded) + (2.5*Accurate.Long.Balls) + (2*Saves)) 

top_10_gks <- head(gks_scored[order(-gks_scored$ovr_score),],n=10)

ggplot(data=top_10_gks,aes(x=Player.Name,y=ovr_score))+geom_point(aes(color=Player.Name))

#text(ovr_score ~Player.Name, labels=Player.Name,data=top_10_gks, cex=0.9, font=2)

gk_plt <- top_10_gks %>%
  ggplot(aes(x=Player.Name, y=ovr_score))+ geom_point(aes(color=Player.Name))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title="Top 10 Goalkeepers of 2017/18") +
  geom_text(aes(label=Player.Name),size=3) 

gk_plt + theme(axis.text.x = element_blank(),
               axis.ticks.x = element_blank()) +
  ylab("Overall Score") + xlab("Player Name")


defender_df <- Stats1_clean %>% 
  filter(Position=="Defender")  

def_scored <- mutate(defender_df, ovr_score = 0 + (1.5*Goals) - (2*Errors.Leading.To.Goal) + (0.9*Shots.On.Target) + (1*Assists) + (0.5*Tackles) - (2*Red.Cards) - (0.9*Fouls) + (0.8*Goals.With.Header) - (0.8*Own.Goals) + (0.8*Blocked.Shots) + (0.5*Passes.Per.Match)) 

top_10_def <- head(def_scored[order(-def_scored$ovr_score),],n=10)

def_plt <- top_10_def %>%
  ggplot(aes(x=Player.Name, y=ovr_score))+ geom_point(aes(color=Player.Name))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title="Top 10 Defenders of 2017/18") +
  geom_text(aes(label=Player.Name),size=3) 

def_plt + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank()) +
  ylab("Overall Score") + xlab("Player Name")


mid_df <- Stats1_clean %>% 
  filter(Position=="Midfielder")  

mid_scored <- mutate(mid_df, ovr_score = 0 + (0.5*Shooting.Accuracy.prc) + (0.8*Goals) + (0.9*Shots.On.Target) + (0.9*Assists) + (0.9*Tackle.Success.prc) - (1*Red.Cards) + (0.8*Penalties.Scored) - (0.9*Fouls) + (0.5*Goals.With.Header) - (0.8*Own.Goals) + (0.5*Goals.From.Freekick) + (1.0*Passes.Per.Match))


top_10_mid <- head(mid_scored[order(-mid_scored$ovr_score),],n=10)

mid_plt <- top_10_mid %>%
  ggplot(aes(x=Player.Name, y=ovr_score))+ geom_point(aes(color=Player.Name))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title="Top 10 Midfielders of 2017/18") +
  geom_text(aes(label=Player.Name),size=3) 

mid_plt + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank()) +
  ylab("Overall Score") + xlab("Player Name")



att_df <- Stats1_clean %>% 
  filter(Position=="Forward")  

att_df[is.na(att_df)] <- 0

att_scored <- mutate(att_df, ovr_score = 0 + (0.8*Shooting.Accuracy.prc) + (1*Goals) + (0.9*Shots.On.Target) + (0.5*Assists) + (0.5*Tackle.Success.prc) - (1*Red.Cards) + (0.8*Penalties.Scored) - (0.9*Fouls) + (0.8*Goals.With.Header) - (0.8*Own.Goals) + (0.8*Goals.From.Freekick) + (0.5*Passes.Per.Match) + (0.8*Cross.Accuracy.prc))

top_10_att <- head(att_scored[order(-att_scored$ovr_score),],n=10)


str(top_10_att)

att_plt <- top_10_att %>%
  ggplot(aes(x=Player.Name, y=ovr_score))+ geom_point(aes(color=Player.Name))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title="Top 10 Forwards of 2017/18") +
  geom_text(aes(label=Player.Name),size=3) 


att_plt + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank()) +
  ylab("Overall Score") + xlab("Player Name")

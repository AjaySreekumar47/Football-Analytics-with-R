colnames(Stats1_clean)

make.names(names(Stats2_clean))


colnames(Stats1_clean) <- c("Player.Name","Accurate.Long.Balls","Aerial.Battles.Lost","Aerial.Battles.Won","Appearances","Assists","Attack","Big.Chances.Created","Big.Chances.Missed","Blocked.Shots","Catches","Clean.Sheets","Clearances","Clearances.Off.Line","Club","Cross.Accuracy.prc","Crosses","Defence","Discipline","Duels.Lost","Duels.Won","Errors.Leading.To.Goal","Fouls","Goal.Kicks","Goalkeeping","Goals","Goals.Conceded","Goals.From.Freekick","Goals.Per.Match","Goals.With.Header","Goals.With.Left.Foot","Goals.With.Right.Foot","Headed.Clearance","High.Claims","Hit.Woodwork","Interceptions","Last.Man.Tackles","Losses","Offsides","Own.Goals","Passes","Passes.Per.Match","Penalties.Saved","Penalties.Scored","Position","Punches","Recoveries","Red.Cards","Saves","Shooting.Accuracy.prc","Shots","Shots.On.Target","Successful.Challenges","Sweeper.Clearances", "Tackle.Success.prc","Tackles","Team.Play","Through.Balls","Throw.Outs","Wins","Yellow.Cards")

colnames(Stats2_clean) <- c("League","Team","Season","KEY","Player","Position","Appearences","Goals","Started","Started.As.A.Sub","Came.On","Taken.Off","Own.Goals","Type.Of.Goal","First.Half","Second.Half","First.Scorer","Last.Scorer","Home","Away","Right.Foot","Left.Foot","Header","Other.Method","Open.Play","Cross","Free.Kick", "Direct.Free.Kick","Throw.In", "Penalty","Corner","Other.Type.Of.Play","Minutes.played","Min.goal","Assists", "Prc.Assists","Leading.Recipient","Recipient.Goals","CS","Prc.Clean.Sheets","Hat.Tricks","Yellow.Cards","Yellow.First.Half","Yellow.Second.Half","Yellow.Home","Yellow.Away","Yellow.Minutes.Played","Minutes.per.Yellow.Cards","Sent.Off","Straight.Red.Card","Two.Yellow.Cards","Red.First.Half","Red.Second.Half","Red.Home","Red.Away","Red.Minutes.Played","Minutes.per.Red.Cards")

Stats2_clean$Yellow.Cards <- as.integer(Stats2_clean$Yellow.Cards)










gks_scored <- gks_scored %>% 
  filter(ovr_score!=0.0) %>% 
  arrange(ovr_score)

bottom_10_gks <- head(gks_scored[order(gks_scored$ovr_score),],n=10)

ggplot(data=bottom_10_gks,aes(x=Player.Name,y=ovr_score))+geom_point(aes(color=Player.Name))

#text(ovr_score ~Player.Name, labels=Player.Name,data=top_10_gks, cex=0.9, font=2)

bot_gk_plt <- bottom_10_gks %>%
  ggplot(aes(x=Player.Name, y=ovr_score))+ geom_point(aes(color=Player.Name))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title="Bottom 10 Goalkeepers of 2017/18") +
  geom_text(aes(label=Player.Name),size=3) 

bot_gk_plt + theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank()) +
  ylab("Overall Score") + xlab("Player Name")




def_scored <- def_scored %>% 
  filter(ovr_score!=0.0) %>% 
  arrange(ovr_score)

bot_10_def <- head(def_scored[order(def_scored$ovr_score),],n=10)

bot_def_plt <- bot_10_def %>%
  ggplot(aes(x=Player.Name, y=ovr_score))+ geom_point(aes(color=Player.Name))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title="Bottom 10 Defenders of 2017/18") +
  geom_text(aes(label=Player.Name),size=3) 

bot_def_plt + theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  ylab("Overall Score") + xlab("Player Name")



mid_scored <- mid_scored %>% 
  filter(ovr_score!=0.0) %>% 
  arrange(ovr_score)

bot_10_mid <- head(mid_scored[order(mid_scored$ovr_score),],n=10)

bot_mid_plt <- bot_10_mid %>%
  ggplot(aes(x=Player.Name, y=ovr_score))+ geom_point(aes(color=Player.Name))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title="Bottom 10 Midfielders of 2017/18") +
  geom_text(aes(label=Player.Name),size=3) 

bot_mid_plt + theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  ylab("Overall Score") + xlab("Player Name")


att_scored <- att_scored %>% 
  filter(ovr_score!=0.0) %>% 
  arrange(ovr_score)

bot_10_att <- head(att_scored[order(att_scored$ovr_score),],n=10)

bot_att_plt <- bot_10_att %>%
  ggplot(aes(x=Player.Name, y=ovr_score))+ geom_point(aes(color=Player.Name))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title="Bottom 10 Forwards of 2017/18") +
  geom_text(aes(label=Player.Name),size=3) 

bot_att_plt + theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  ylab("Overall Score") + xlab("Player Name")
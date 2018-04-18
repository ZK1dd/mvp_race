library(tidyverse)
library(dplyr)

teamBox = read.csv('2016-17_teamBoxScore.csv')
playerBox17 = read.csv('2016-17_playerBoxScore.csv')
playerBox18 = read.csv('2017-18_playerBoxScore.csv')

mvpPredictions = playerBox18 %>%
  group_by(playDispNm) %>%
  mutate(blkPerGame = mean(playBLK),
         trbPerGame = mean(playTRB),
         stlPerGame = mean(playSTL),
         toPerGame = mean(playTO),
         astPerGame = mean(playAST),
         ptsPerGame = mean(playPTS),
         teamWins = sum(teamRslt == 'Win'),
         plusMinus = mean(playPTS[teamRslt== 'Win']) - mean(playPTS[teamRslt=='Loss']),
         mvpScore = (blkPerGame * 1.0) + (stlPerGame * 2.3) + (trbPerGame * 1.0)+ (astPerGame * 1.2)
         + (ptsPerGame * 2.0)+ (toPerGame * -1.2) + (teamWins * 0.3)) %>%
  select(playDispNm, blkPerGame, trbPerGame, stlPerGame, toPerGame, astPerGame, ptsPerGame, 
         teamWins, plusMinus, mvpScore) %>%
  slice(1) %>%
  arrange(-mvpScore)

mvpPredictions[1:20,]

ggplot(data = mvpPredictions[1:15,], aes(x=reorder(playDispNm, mvpScore), y=mvpScore)) +
  geom_bar(stat="identity", fill = "#7b9eb8") + 
  coord_flip()

##additional graphs
#what are plus minus rankings?
ggplot(data = mvpPredictions[1:15,], aes(x=reorder(playDispNm, plusMinus), y=plusMinus)) +
  geom_bar(stat="identity", fill = "#7b9eb8") + 
  coord_flip()
par(mfrow=c(2,3))

#each element of MVP score for James Harden, including win indicator

ggplot(data = playerBox18[playerBox18$playDispNm=="James Harden",],
       aes(x=gmDate, y=playPTS, color=teamRslt)) + 
  geom_point(size=2)+
  labs(title="James Harden PTS")

ggplot(data = playerBox18[playerBox18$playDispNm=="James Harden",],
       aes(x=gmDate, y=playBLK, color=teamRslt)) + 
  geom_point(size=2)+
  labs(title="James Harden BLK")

ggplot(data = playerBox18[playerBox18$playDispNm=="James Harden",],
       aes(x=gmDate, y=playTRB, color=teamRslt)) + 
  geom_point(size=2)+
  labs(title="James Harden TRB")

ggplot(data = playerBox18[playerBox18$playDispNm=="James Harden",],
       aes(x=gmDate, y=playSTL, color=teamRslt)) + 
  geom_point(size=2)+
  labs(title="James Harden STL")

ggplot(data = playerBox18[playerBox18$playDispNm=="James Harden",],
       aes(x=gmDate, y=playTO, color=teamRslt)) + 
  geom_point(size=2)+
  labs(title="James Harden TO")

ggplot(data = playerBox18[playerBox18$playDispNm=="James Harden",],
       aes(x=gmDate, y=playAST, color=teamRslt)) + 
  geom_point(size=2)+
  labs(title="James Harden AST")

#does higher MVP score contribution in a game indicate team win?

playerBox18$mvpcontribution <- (playerBox18$playBLK * 1.0) + 
  (playerBox18$playSTL * 2.3) + 
  (playerBox18$playTRB * 1.0) + 
  (playerBox18$playAST * 1.2) + 
  (playerBox18$playPTS * 2.0) + 
  (playerBox18$playTO * -1.2) + 
  ifelse(playerBox18$teamRslt=="Loss",0,0.3)
ggplot(data = playerBox18[playerBox18$playDispNm=="James Harden",],
       aes(x=gmDate, y=mvpcontribution, color=teamRslt)) + 
  geom_point(size=2)+
  labs(title="James Harden MVP score contribution")

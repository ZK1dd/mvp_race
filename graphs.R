library(tidyverse)

teamBox = read.csv('nba-enhanced-stats/2016-17_teamBoxScore.csv')
playerBox17 = read.csv('nba-enhanced-stats/2016-17_playerBoxScore.csv')
playerBox18 = read.csv('nba-enhanced-stats/2017-18_playerBoxScore.csv')

mvpPredictions = playerBox %>%
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



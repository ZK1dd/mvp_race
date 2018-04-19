library(caret)
library(dplyr)

mvpRanks = read.csv('All_player_data.csv')

topPlayers = mvpRanks %>%
  filter((PS.G >= 20 & OBPM >= 0 & DBPM >= 0) | MVP == 1,  season_end < 2012) %>%
  group_by(season_end) %>%
  top_n(5) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM) 
#there are 6 values in 2007 for some reason
topPlayers <- topPlayers[-91,]

#add predictions to df and classify mvp
mvp_glm <- glm(MVP ~ GS + PS.G + TRB + AST + STL + BLK + TOV + OBPM + DBPM, data = topPlayers, family = binomial)
topPlayers$mvp_glm <- predict(mvp_glm, topPlayers[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM')], type="response")
for (i in seq(1,nrow(topPlayers),5)){
  for (j in 0:4){
  topPlayers[i+j,14] <- ifelse(topPlayers[i+j,13]==max(topPlayers[i:(i+4),13]),
                                 1,0)
  }
}

topPlayersTest = mvpRanks %>%
  filter((PS.G >= 20 & OBPM >= 0 & DBPM >= 0) | MVP == 1,  season_end > 2012) %>%
  group_by(season_end) %>%
  top_n(5) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM) 

# logistic regression
mvp_glm.test = predict(mvp_glm, topPlayersTest[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM')], type="response")
topPlayersTest$mvp_glm_test <- mvp_glm.test

topPlayersTest
mvp_glm.test

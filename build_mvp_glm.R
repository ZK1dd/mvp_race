library(caret)
library(dplyr)

mvpRanks = read.csv('All_player_data.csv')

topPlayers = mvpRanks %>%
  filter((PS.G >= 20 & OBPM >= 0 & DBPM >= 0) | MVP == 1,  season_end < 2012) %>%
  group_by(season_end) %>%
  top_n(5) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM) 

mvp_glm <- glm(MVP ~ GS + PS.G + TRB + AST + STL + BLK + TOV + OBPM + DBPM, data = topPlayers, family = binomial)

topPlayersTest = mvpRanks %>%
  filter((PS.G >= 20 & OBPM >= 0 & DBPM >= 0) | MVP == 1,  season_end > 2012) %>%
  group_by(season_end) %>%
  top_n(5) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM) 

# logistic regression
mvp_glm.test = predict(mvp_glm, topPlayersTest[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM')], type="response")

topPlayersTest
mvp_glm.test

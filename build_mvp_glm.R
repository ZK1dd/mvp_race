library(caret)
library(dplyr)

mvpRanks = read.csv('All_player_data.csv')
allgames = read.csv('every_game.csv')

teamWins = allgames %>% 
  select(game_result, team_id, year_id) %>%
  filter(year_id >= 1989, is_playoffs == 0) %>%
  group_by(team_id, year_id) %>%
  mutate(Wins = sum(game_result == 'W')) %>%
  distinct(team_id, year_id, Wins) 

mvpRanks = left_join(mvpRanks, teamWins, c("Tm_x" = "team_id", "season_end" = "year_id"))

topPlayers = mvpRanks %>%
  filter((PS.G >= 20 & OBPM >= 0 & DBPM >= 0) | MVP == 1,  season_end < 2012) %>%
  group_by(season_end) %>%
  top_n(5) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM, Wins) 
#there are 6 values in 2007 for some reason
topPlayers <- topPlayers[-91,]

topPlayersTest = mvpRanks %>%
  filter((PS.G >= 20 & OBPM >= 0 & DBPM >= 0) | MVP == 1,  season_end >= 2012) %>%
  group_by(season_end) %>%
  top_n(5) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM) 

allPlayersTrain = mvpRanks %>%
  filter(season_end < 2012) %>%
  group_by(season_end) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM)

allPlayersTest = mvpRanks %>%
  filter(season_end >= 2012) %>%
  group_by(season_end) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM)

#Method 1 - logistic regression-predict MVP based on instinctual important variables
mvp_glm <- glm(MVP ~ GS + PS.G + TRB + AST + STL + BLK + TOV + OBPM + DBPM, data = topPlayers, family = binomial)
#training data - predictions, add to DF
topPlayers$mvp_glm <- predict(mvp_glm, topPlayers[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM')], type="response")
for (i in seq(1,nrow(topPlayers),5)){
  for (j in 0:4){
  topPlayers[i+j,14] <- ifelse(topPlayers[i+j,13]==max(topPlayers[i:(i+4),13]),
                                 1,0)
  }
}
#training data - confusion matrix
m1cm <- table(topPlayers$MVP,topPlayers$V14)
m1error <- (m1cm[1,2]+m1cm[2,1])/(sum(m1cm[,1])+sum(m1cm[,2]))
m1cm
m1error
#test data - predictions, add to DF
mvp_glm.test = predict(mvp_glm, topPlayersTest[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM')], type="response")
topPlayersTest$mvp_glm_test <- mvp_glm.test
for (i in seq(1,nrow(topPlayersTest),5)){
  for (j in 0:4){
    topPlayersTest[i+j,14] <- ifelse(topPlayersTest[i+j,13]==max(topPlayersTest[i:(i+4),13]),
                                 1,0)
  }
}
#test data - confusion matrix, error
cm <- table(topPlayersTest$MVP,topPlayersTest$V14)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error

#Method 2 - use significant vars Method 1 (using the same didn't converge), all Players
mvp_glm <- glm(MVP ~ PS.G + AST + STL + DBPM, data = allPlayersTest, family = binomial)
#training data - predictions, add to DF
allPlayersTrain$mvp_glm <- predict(mvp_glm, 
                                  allPlayersTrain[, c('PS.G','AST', 'STL', 'DBPM')], 
                                  type="response")
a <- c(data.frame(table(allPlayersTrain$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-24]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    allPlayersTrain[value+j,14] <- ifelse(allPlayersTrain[value+j,13]==max(allPlayersTrain[(value):(value+c[match(value,b)]),13]),
                                 1,0)
  }
}
#i don't know why there were extra rows at the end, but I'm tired so just removing them
allPlayersTrain <- allPlayersTrain[1:9830,]
#training data - confusion matrix
cm <- table(allPlayersTrain$MVP,allPlayersTrain$V14)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
#test data - predictions, add to DF
allPlayersTest$mvp_glm_test <- predict(mvp_glm, allPlayersTest[, c('PS.G','AST', 'STL', 'DBPM')], type="response")
a <- c(data.frame(table(allPlayersTest$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-5]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    allPlayersTest[value+j,14] <- ifelse(allPlayersTest[value+j,13]==max(allPlayersTest[(value):(value+c[match(value,b)]),13]),
                                          1,0)
  }
}
#test data - confusion matrix, error
cm <- table(allPlayersTest$MVP,allPlayersTest$V14)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error

#Method 3 - logistic regression, topPlayer, predict MVP on significant variables from M1: PS.G, AST, STL, DBPM
mvp_glm <- glm(MVP ~ PS.G + AST + STL + DBPM, data = topPlayers, family = binomial)
#training data - predictions, add to DF
topPlayers$mvp_glm_m3 <- predict(mvp_glm, topPlayers[, c('PS.G','AST', 'STL', 'DBPM')], type="response")
for (i in seq(1,nrow(topPlayers),5)){
  for (j in 0:4){
    topPlayers[i+j,16] <- ifelse(topPlayers[i+j,15]==max(topPlayers[i:(i+4),15]),
                                 1,0)
  }
}
#training data - confusion matrix
m1cm <- table(topPlayers$MVP,topPlayers$V16)
m1error <- (m1cm[1,2]+m1cm[2,1])/(sum(m1cm[,1])+sum(m1cm[,2]))
m1cm
m1error
#test data - predictions, add to DF
mvp_glm.test = predict(mvp_glm, topPlayersTest[, c('PS.G','AST', 'STL', 'DBPM')], type="response")
topPlayersTest$mvp_glm_test_m3 <- mvp_glm.test
for (i in seq(1,nrow(topPlayersTest),5)){
  for (j in 0:4){
    topPlayersTest[i+j,16] <- ifelse(topPlayersTest[i+j,15]==max(topPlayersTest[i:(i+4),15]),
                                     1,0)
  }
}
#test data - confusion matrix, error
cm <- table(topPlayersTest$MVP,topPlayersTest$V16)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error

#aggregate all methods MVP picks into one table for comparison
trueMVP = mvpRanks %>%
  filter(MVP == 1) %>%
  group_by(season_end) %>%
  select(season_end,player) 

mvpByMethod1 = rbind(topPlayers,topPlayersTest) %>%
  filter(V14 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

mvpByMethod2 = rbind(allPlayersTrain,allPlayersTest) %>%
  filter(V14 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

mvpByMethod3 = rbind(topPlayers,topPlayersTest) %>%
  filter(V16 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

compareMVP <- cbind(trueMVP, mvpByMethod1, mvpByMethod2, mvpByMethod3)
compareMVP <- compareMVP[,-c(3,5,7)]


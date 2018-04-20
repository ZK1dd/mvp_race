library(caret)
library(dplyr)

mvpRanks = read.csv('All_player_data.csv')
allgames = read.csv('every_game.csv')

teamWins = allgames %>% 
  select(game_result, team_id, year_id, is_playoffs) %>%
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


topPlayersTest = mvpRanks %>%
  filter((PS.G >= 20 & OBPM >= 0 & DBPM >= 0) | MVP == 1,  season_end >= 2012) %>%
  group_by(season_end) %>%
  top_n(5) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM, Wins) 


allPlayersTrain = mvpRanks %>%
  filter(season_end < 2012) %>%
  group_by(season_end) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM, Wins)

allPlayersTest = mvpRanks %>%
  filter(season_end >= 2012) %>%
  group_by(season_end) %>%
  select(player, MVP, GS, PS.G, TRB, AST, STL, BLK, TOV, OBPM, DBPM, Wins)

playerstats18 <- read.csv("playerstats18.csv")
playerstats18 <- playerstats18[1:20,-1]

#Method 1 - logistic regression-predict MVP based on instinctual important variables
mvp_glm <- glm(MVP ~ GS + PS.G + TRB + AST + STL + BLK + TOV + OBPM + DBPM, data = topPlayers, family = binomial)
summary(mvp_glm)
#training data - predictions, add to DF
topPlayers$mvp_glm <- predict(mvp_glm, topPlayers[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM')], type="response")
a <- c(data.frame(table(topPlayers$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-24]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayers[value+j,15] <- ifelse(topPlayers[value+j,14]==
                                       max(topPlayers[(value):(value+c[match(value,b)]),14]),
                                     1,0)
  }
}
#training data - confusion matrix
m1cm <- table(topPlayers$MVP,topPlayers$V15)
m1error <- (m1cm[1,2]+m1cm[2,1])/(sum(m1cm[,1])+sum(m1cm[,2]))
m1cm
m1error
#test data - predictions, add to DF
mvp_glm.test = predict(mvp_glm, topPlayersTest[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM')], type="response")
topPlayersTest$mvp_glm_test <- mvp_glm.test
a <- c(data.frame(table(topPlayersTest$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-5]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayersTest[value+j,15] <- ifelse(topPlayersTest[value+j,14]==
                                       max(topPlayersTest[(value):(value+c[match(value,b)]),14]),
                                     1,0)
  }
}
#test data - confusion matrix, error
cm <- table(topPlayersTest$MVP,topPlayersTest$V15)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
#predict18
playerstats18$m1 <- predict(mvp_glm, playerstats18[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM')], type="response")

#Method 2 - use significant vars Method 1 (using the same didn't converge), all Players
mvp_glm <- glm(MVP ~ PS.G + AST + STL + DBPM, data = allPlayersTrain, family = binomial)
summary(mvp_glm)
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
    allPlayersTrain[value+j,15] <- ifelse(allPlayersTrain[value+j,14]==max(allPlayersTrain[(value):(value+c[match(value,b)]),14]),
                                 1,0)
  }
}
#i don't know why there were extra rows at the end, but I'm tired so just removing them
allPlayersTrain <- allPlayersTrain[1:9830,]
#training data - confusion matrix
cm <- table(allPlayersTrain$MVP,allPlayersTrain$V15)
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
    allPlayersTest[value+j,15] <- ifelse(allPlayersTest[value+j,14]==max(allPlayersTest[(value):(value+c[match(value,b)]),14]),
                                          1,0)
  }
}
#test data - confusion matrix, error
cm <- table(allPlayersTest$MVP,allPlayersTest$V15)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
#2018 predict
playerstats18$m2 <- predict(mvp_glm, playerstats18[, c('PS.G','AST', 'STL', 'DBPM')], type="response")

#Method 3 - logistic regression, topPlayer, predict MVP on significant variables from M1: PS.G, AST, STL, DBPM
mvp_glm <- glm(MVP ~ PS.G + AST + STL + DBPM, data = topPlayers, family = binomial)
summary(mvp_glm)
#training data - predictions, add to DF
topPlayers$mvp_glm_m3 <- predict(mvp_glm, topPlayers[, c('PS.G','AST', 'STL', 'DBPM')], type="response")
a <- c(data.frame(table(topPlayers$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-24]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayers[value+j,17] <- ifelse(topPlayers[value+j,16]==max(topPlayers[(value):(value+c[match(value,b)]),16]),
                                         1,0)
  }
}
#training data - confusion matrix
m1cm <- table(topPlayers$MVP,topPlayers$V17)
m1error <- (m1cm[1,2]+m1cm[2,1])/(sum(m1cm[,1])+sum(m1cm[,2]))
m1cm
m1error
#test data - predictions, add to DF
mvp_glm.test = predict(mvp_glm, topPlayersTest[, c('PS.G','AST', 'STL', 'DBPM')], type="response")
topPlayersTest$mvp_glm_test_m3 <- mvp_glm.test
a <- c(data.frame(table(topPlayersTest$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-5]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayersTest[value+j,17] <- ifelse(topPlayersTest[value+j,16]==max(topPlayersTest[(value):(value+c[match(value,b)]),16]),
                                     1,0)
  }
}
#test data - confusion matrix, error
cm <- table(topPlayersTest$MVP,topPlayersTest$V17)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
#2018 predict
playerstats18$m3 <- predict(mvp_glm, playerstats18[, c('PS.G','AST', 'STL', 'DBPM')], type="response")

#Method 4 - logistic regression-predict MVP based on instinctual important variables, added in team wins
mvp_glm <- glm(MVP ~ GS + PS.G + TRB + AST + STL + BLK + TOV + OBPM + DBPM + Wins, data = topPlayers, family = binomial)
summary(mvp_glm)
#training data - predictions, add to DF
topPlayers$mvp_glm_3 <- predict(mvp_glm, topPlayers[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM', 'Wins')], type="response")
a <- c(data.frame(table(topPlayers$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-24]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayers[value+j,19] <- ifelse(topPlayers[value+j,18]==
                                       max(topPlayers[(value):(value+c[match(value,b)]),18]),
                                         1,0)
  }
}

#training data - confusion matrix
m1cm <- table(topPlayers$MVP,topPlayers$V19)
m1error <- (m1cm[1,2]+m1cm[2,1])/(sum(m1cm[,1])+sum(m1cm[,2]))
m1cm
m1error
#test data - predictions, add to DF
mvp_glm.test = predict(mvp_glm, topPlayersTest[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM','Wins')], type="response")
topPlayersTest$mvp_glm_test3 <- mvp_glm.test
a <- c(data.frame(table(topPlayersTest$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-5]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayersTest[value+j,19] <- ifelse(topPlayersTest[value+j,18]==max(topPlayersTest[(value):(value+c[match(value,b)]),18]),
                                         1,0)
  }
}
#test data - confusion matrix, error
cm <- table(topPlayersTest$MVP,topPlayersTest$V19)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
#2018 predict
playerstats18$m4 <- predict(mvp_glm, playerstats18[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 
                                                       'BLK', 'TOV', 'OBPM', 'DBPM', 'Wins')], 
                            type="response")

#Method 5 - use significant vars Method 4 (using the same didn't converge), all Players, add in team wins
mvp_glm <- glm(MVP ~ PS.G + AST + Wins, data = allPlayersTrain, family = binomial)
summary(mvp_glm)
#training data - predictions, add to DF
allPlayersTrain$mvp_glm5 <- predict(mvp_glm, 
                                   allPlayersTrain[, c('PS.G','AST','Wins')], 
                                   type="response")
#replace NA with 0 so it can find max
allPlayersTrain$mvp_glm5[is.na(allPlayersTrain$mvp_glm5)]<- 0

a <- c(data.frame(table(allPlayersTrain$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-24]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    allPlayersTrain[value+j,17] <- ifelse(allPlayersTrain[value+j,16]==max(allPlayersTrain[(value):(value+c[match(value,b)]),16]),
                                          1,0)
  }
}

#training data - confusion matrix
cm <- table(allPlayersTrain$MVP,allPlayersTrain$V17)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
#test data - predictions, add to DF
allPlayersTest$mvp_glm_test5 <- predict(mvp_glm, allPlayersTest[, c('PS.G','AST', 'Wins')], type="response")
allPlayersTest$mvp_glm_test5[is.na(allPlayersTest$mvp_glm_test5)]<- 0

a <- c(data.frame(table(allPlayersTest$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-5]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    allPlayersTest[value+j,17] <- ifelse(allPlayersTest[value+j,16]==max(allPlayersTest[(value):(value+c[match(value,b)]),16]),
                                         1,0)
  }
}
#test data - confusion matrix, error
cm <- table(allPlayersTest$MVP,allPlayersTest$V17)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
#2018 predictions
playerstats18$m5 <- predict(mvp_glm, playerstats18[, c('PS.G','AST', 'Wins')], 
                            type="response")

#Method 6 - use significant vars Method 4 (using the same didn't converge), top Players
mvp_glm <- glm(MVP ~ PS.G + AST + Wins, data = topPlayers, family = binomial)
summary(mvp_glm)
#training data - predictions, add to DF
topPlayers$mvp_glm6 <- predict(mvp_glm, 
                                    topPlayers[, c('PS.G','AST','Wins')], 
                                    type="response")
#replace NA with 0 so it can find max
topPlayers$mvp_glm6[is.na(topPlayers$mvp_glm6)]<- 0

a <- c(data.frame(table(topPlayers$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-24]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayers[value+j,21] <- ifelse(topPlayers[value+j,20]==max(topPlayers[(value):(value+c[match(value,b)]),20]),
                                          1,0)
  }
}

#training data - confusion matrix
cm <- table(topPlayers$MVP,topPlayers$V21)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
#test data - predictions, add to DF
topPlayersTest$mvp_glm_test6 <- predict(mvp_glm, topPlayersTest[, c('PS.G','AST', 'Wins')], type="response")
topPlayersTest$mvp_glm_test6[is.na(topPlayersTest$mvp_glm_test6)]<- 0

a <- c(data.frame(table(topPlayersTest$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-5]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayersTest[value+j,21] <- ifelse(topPlayersTest[value+j,20]==max(topPlayersTest[(value):(value+c[match(value,b)]),20]),
                                         1,0)
  }
}
#test data - confusion matrix, error
cm <- table(topPlayersTest$MVP,topPlayersTest$V21)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
#2018 predictions
playerstats18$m6 <- predict(mvp_glm, playerstats18[, c('PS.G','AST', 'Wins')], 
                            type="response")

#Method 7 - random Forest
library(randomForest)
mvp_ranForest <- randomForest(MVP ~ GS + PS.G + TRB + AST + STL + BLK + TOV + 
                                OBPM + DBPM + Wins, data = topPlayers)
summary(mvp_ranForest)
#training data - predictions, add to DF
topPlayers$mvp_rf <- predict(mvp_ranForest, topPlayers[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM', 'Wins')], type="response")
a <- c(data.frame(table(topPlayers$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-24]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayers[value+j,23] <- ifelse(topPlayers[value+j,22]==
                                       max(topPlayers[(value):(value+c[match(value,b)]),22]),
                                     1,0)
  }
}

#training data - confusion matrix
m1cm <- table(topPlayers$MVP,topPlayers$V23)
m1error <- (m1cm[1,2]+m1cm[2,1])/(sum(m1cm[,1])+sum(m1cm[,2]))
m1cm
m1error
#test data - predictions, add to DF
mvp_glm.test = predict(mvp_ranForest, topPlayersTest[, c('GS', 'PS.G', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'OBPM', 'DBPM','Wins')], type="response")
topPlayersTest$rftest <- mvp_glm.test
a <- c(data.frame(table(topPlayersTest$season_end))[,2])
b <- c(1,a)
for (i in 2:length(b)) b[i] <- b[i-1]+b[i]
b <- b[-5]
c <- a-1
for (value in b){
  for (j in 0:c[match(value,b)]){
    topPlayersTest[value+j,23] <- ifelse(topPlayersTest[value+j,22]==max(topPlayersTest[(value):(value+c[match(value,b)]),22]),
                                         1,0)
  }
}
#test data - confusion matrix, error
cm <- table(topPlayersTest$MVP,topPlayersTest$V23)
error <- (cm[1,2]+cm[2,1])/(sum(cm[,1])+sum(cm[,2]))
cm
error
playerstats18$m7 <- predict(mvp_glm, playerstats18[, c('GS', 'PS.G', 'TRB', 'AST', 
                                                       'STL', 'BLK', 'TOV', 'OBPM', 'DBPM','Wins')], 
                            type="response")


#aggregate all methods MVP picks into one table for comparison
trueMVP = mvpRanks %>%
  filter(MVP == 1) %>%
  group_by(season_end) %>%
  select(season_end,player) 

mvpByMethod1 = rbind(topPlayers,topPlayersTest) %>%
  filter(V15 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

mvpByMethod2 = rbind(allPlayersTrain,allPlayersTest) %>%
  filter(V15 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

mvpByMethod3 = rbind(topPlayers,topPlayersTest) %>%
  filter(V17 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

mvpByMethod4 = rbind(topPlayers,topPlayersTest) %>%
  filter(V19 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

mvpByMethod5 = rbind(allPlayersTrain,allPlayersTest) %>%
  filter(V17 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

mvpByMethod6 = rbind(topPlayers,topPlayersTest) %>%
  filter(V21 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

mvpByMethod7 = rbind(topPlayers,topPlayersTest) %>%
  filter(V23 == 1) %>%
  group_by(season_end) %>%
  select(season_end,player)

compareMVP <- cbind(trueMVP, mvpByMethod1, mvpByMethod2, mvpByMethod3,
                    mvpByMethod4, mvpByMethod5, mvpByMethod6, mvpByMethod7)
compareMVP <- compareMVP[,-c(3,5,7,9,11,13,15)]
write.csv(compareMVP,"compareMVP.csv")

max(playerstats18$m1)
max(playerstats18$m2)
max(playerstats18$m3)
max(playerstats18$m4)
max(playerstats18$m5)
max(playerstats18$m6)
max(playerstats18$m7)


data <- read.csv("All_player_data.csv")
summary(data)

data <- data[,c(4:59)]
data <- data.frame(data)
lapply(data, unique)
data <-data[,c(-52,-47)]
mvp_lm <- glm(MVP ~.,data = data,family = binomial)
summary(mvp_lm)


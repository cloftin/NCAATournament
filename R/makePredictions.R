#'@export
makePredictions <- function() {

  results <- read.csv(file = "data/HistNCAATournamentResults.csv", header = T, stringsAsFactors = F)
  results$GID <- c(1:nrow(results))

  lmmodel <- create_model(fromSample = T, yearToTest = NULL)

  test <- lmmodel[[2]]
  lmmodel <- lmmodel[[1]]
  # test <- get_kp_data(results %>% filter(Year == 2017))[[1]]
  test$pred <- predict(lmmodel,newdata=test,type='response')
  test$bpred <- ifelse(test$pred > 0.5, 1, 0)
  test <- test[order(test$GID),]

  gids <- test %>% group_by(GID) %>% summarise(gamec = sum(bpred), count = n()) %>%
    filter(gamec != 1) %>% as.data.frame() %>% select(GID)
  gids <- as.vector(gids$GID)
  w <- which(test$GID %in% gids)

  for(i in 1:length(gids)) {
    if(test$pred[w[(2*i)-1]] > test$pred[w[2*i]]) {
      test$bpred[w[2*i - 1]] <- 1
      test$bpred[w[2*i]] <- 0
    } else {
      test$bpred[w[2*i - 1]] <- 0
      test$bpred[w[2*i]] <- 1
    }
  }

  test$Correct <- ifelse(test$Result == test$bpred, 1, 0)
  plyr::count(test$Correct)

  games <- data.frame()
  for(i in 1:length(unique(test$GID))) {
    t <- test %>% filter(GID == unique(test$GID)[i])
    t <- t[order(-t$bpred),]
    temp <- as.data.frame(matrix(ncol = 0, nrow = 1))
    temp$GID <- t$GID[1]
    temp$Year <- t$Year[1]
    temp$Round <- t$Round[1]
    temp$PWinner <- t$Team[1]
    temp$PLoser <- t$Team[2]
    temp$Winner <- t %>% filter(Result == 1) %>% select(Team) %>% unlist()
    temp$Loser <- t %>% filter(Result == 0) %>% select(Team) %>% unlist()
    temp$Correct <- ifelse(temp$PWinner == temp$Winner, 1, 0)

    games <- rbind(games, temp)
  }

}

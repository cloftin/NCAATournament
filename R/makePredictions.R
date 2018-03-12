library(NCAATournament)
library(caTools)

dat <- get_data()
set.seed(101)

dat$Result <- as.factor(dat$Result)

# sample <- sample.split(dat$Result, SplitRatio = 0.70)
train <- dat %>% filter(Year < 2017)
test <- dat %>% filter(Year == 2017)

lmmodel <- stats::glm(formula = "Score ~ Seed + oSeed + Tempo + OffEff + RankOE + DefEff + RankDE + oTempo + oOffEff + oRankOE + oDefEff",
                     data = train, family = binomial(link = "logit"))

test$pred <- predict(lmmodel,newdata=test,type='response')
test$bpred <- ifelse(test$pred > 0.5, 1, 0)
test <- test[order(test$GID),]

gids <- test %>% group_by(GID) %>% summarise(gamec = sum(bpred)) %>%
  filter(gamec > 1) %>% as.data.frame() %>% select(GID)
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

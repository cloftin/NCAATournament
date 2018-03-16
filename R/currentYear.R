#'@export
currentYear <- function() {

  useAdj = F
  model <- create_model(fromSample = F, useAdj = useAdj)[[1]]

  ##Play-in Games
  playin <- data.frame(GID = c(1:4), Year = rep(2018, 4), Round = rep("Play-In", 4),
                       HomeSeed = c(16,11,16,11), AwaySeed = c(16,11,16,11),
                       Region = c("East", "East", "West", "Midwest"))
  playin$HomeTeam <- c("Radford", "UCLA", "Texas Southern", "Syracuse")
  playin$AwayTeam <- c("Long Island", "St. Bonaventure", "North Carolina Central", "Arizona State")

  playin$Region <- as.character(playin$Region)

  playin <- get_kp_data(playin, yearFilter = 2018, resultKnown = F, useAdj = useAdj)[[1]]

  playin$pred <- predict(model, newdata = playin, type = "response")

  gids <- unique(playin$GID)
  w <- c(1:length(gids))
  for(i in 1:length(gids)) {
    if(playin$pred[(2*i)-1] > playin$pred[2*i]) {
      playin$bpred[(2*i - 1)] <- 1
      playin$bpred[2*i] <- 0
    } else {
      playin$bpred[(2*i - 1)] <- 0
      playin$bpred[(2*i)] <- 1
    }
  }

  winners <- playin %>% filter(bpred == 1) %>% select(Year, Team, Region, Seed)

  #### First Round Games
  firstround <- data.frame(GID = c(1:32), Year = rep(2018, 32), Round = rep("First Round", 32),
                           HomeSeed = rep(c(1,8,4,5,2,7,3,6), 4), AwaySeed = rep(c(16,9,13,12,15,10,14,11), 4),
                           Region = rep(c("East", "Midwest", "West", "South"), each = 8))
  firstround$Round <- as.character(firstround$Round)
  firstround$Region <- as.character(firstround$Region)
  firstround$HomeTeam <- c("Villanova", "Virginia Tech","Wichita State", "West Virginia",
                           "Purdue", "Arkansas", "Texas Tech", "Florida",
                           "Kansas", "Seton Hall",  "Auburn", "Clemson",
                           "Duke", "Rhode Island", "Michigan State", "TCU",
                           "Xavier", "Missouri",  "Gonzaga", "Ohio State",
                           "North Carolina", "Texas A&M", "Michigan", "Houston",
                           "Virginia", "Creighton", "Arizona", "Kentucky",
                           "Cincinnati", "Nevada", "Tennessee", "Miami (Fla.)")
  firstround$AwayTeam <- c("Radford", "Alabama", "Marshall", "Murray St.",
                           "Cal State Fullerton", "Butler", "Stephen F. Austin", "St. Bonaventure",
                           "Penn", "North Carolina State", "College of Charleston", "New Mexico State",
                           "Iona", "Oklahoma", "Bucknell", "ASU/Syr",
                           "NCC/TxSo", "Florida State", "UNC Greensboro", "South Dakota State",
                           "Lipscomb", "Providence", "Montana", "San Diego State",
                           "Maryland-Baltimore County", "Kansas State", "Buffalo", "Davidson",
                           "Georgia State", "Texas", "Wright State", "Loyola Chicago")

  firstround <- as.data.table(firstround)
  for(i in 1:nrow(winners)) {
    if(winners$Seed[i] <= 8) {
      firstround[Region == winners$Region[i] & HomeSeed == winners$Seed[i], HomeTeam := winners$Team[i]]
    } else {
      firstround[Region == winners$Region[i] & AwaySeed == winners$Seed[i], AwayTeam := winners$Team[i]]
    }
  }

  firstround <- get_kp_data(as.data.frame(firstround), yearFilter = 2018, resultKnown = F, useAdj = useAdj)[[1]]

  firstround$pred <- predict(model, newdata = firstround, type = "response")

  gids <- unique(firstround$GID)
  w <- c(1:length(gids))
  for(i in 1:length(gids)) {
    if(firstround$pred[(2*i)-1] > firstround$pred[2*i]) {
      firstround$bpred[(2*i - 1)] <- 1
      firstround$bpred[2*i] <- 0
    } else {
      firstround$bpred[(2*i - 1)] <- 0
      firstround$bpred[(2*i)] <- 1
    }
  }

  winners <- firstround %>% filter(bpred == 1) %>% select(Year, Team, Region, Seed)

  ## Second Round Games
  secondround <- data.frame(GID = c(1:16), Year = rep(2018, 16), Round = rep("Second Round", 16))
  t <- data.frame()
  for(i in 1:(nrow(winners)/2)) {
    temp <- data.frame(Region = winners$Region[(2*i-1)], HomeSeed = winners$Seed[(2*i-1)],
                    HomeTeam = winners$Team[(2*i-1)], AwaySeed = winners$Seed[(2*i)],
                    AwayTeam = winners$Team[(2*i)])
    t <- rbind(t, temp)
  }
  secondround <- cbind(secondround, t)

  secondround <- get_kp_data(as.data.frame(secondround), yearFilter = 2018, resultKnown = F, useAdj = useAdj)[[1]]

  secondround$pred <- predict(model, newdata = secondround, type = "response")

  gids <- unique(secondround$GID)
  w <- c(1:length(gids))
  for(i in 1:length(gids)) {
    if(secondround$pred[(2*i)-1] > secondround$pred[2*i]) {
      secondround$bpred[(2*i - 1)] <- 1
      secondround$bpred[2*i] <- 0
    } else {
      secondround$bpred[(2*i - 1)] <- 0
      secondround$bpred[(2*i)] <- 1
    }
  }

  winners <- secondround %>% filter(bpred == 1) %>% select(Year, Team, Region, Seed)

  sweet <- data.frame(GID = c(1:8), Year = rep(2018, 8), Round = rep("Sweet Sixteen", 8))
  t <- data.frame()
  for(i in 1:(nrow(winners)/2)) {
    temp <- data.frame(Region = winners$Region[(2*i-1)], HomeSeed = winners$Seed[(2*i-1)],
                       HomeTeam = winners$Team[(2*i-1)], AwaySeed = winners$Seed[(2*i)],
                       AwayTeam = winners$Team[(2*i)])
    t <- rbind(t, temp)
  }
  sweet <- cbind(sweet, t)

  sweet <- get_kp_data(as.data.frame(sweet), yearFilter = 2018, resultKnown = F, useAdj = useAdj)[[1]]

  sweet$pred <- predict(model, newdata = sweet, type = "response")

  gids <- unique(sweet$GID)
  w <- c(1:length(gids))
  for(i in 1:length(gids)) {
    if(sweet$pred[(2*i)-1] > sweet$pred[2*i]) {
      sweet$bpred[(2*i - 1)] <- 1
      sweet$bpred[2*i] <- 0
    } else {
      sweet$bpred[(2*i - 1)] <- 0
      sweet$bpred[(2*i)] <- 1
    }
  }

  winners <- sweet %>% filter(bpred == 1) %>% select(Year, Team, Region, Seed)

  ## Elite Eight
  elite <- data.frame(GID = c(1:4), Year = rep(2018, 4), Round = rep("Elite Eight", 4))
  t <- data.frame()
  for(i in 1:(nrow(winners)/2)) {
    temp <- data.frame(Region = winners$Region[(2*i-1)], HomeSeed = winners$Seed[(2*i-1)],
                       HomeTeam = winners$Team[(2*i-1)], AwaySeed = winners$Seed[(2*i)],
                       AwayTeam = winners$Team[(2*i)])
    t <- rbind(t, temp)
  }
  elite <- cbind(elite, t)

  elite <- get_kp_data(as.data.frame(elite), yearFilter = 2018, resultKnown = F, useAdj = useAdj)[[1]]

  elite$pred <- predict(model, newdata = elite, type = "response")

  gids <- unique(elite$GID)
  w <- c(1:length(gids))
  for(i in 1:length(gids)) {
    if(elite$pred[(2*i)-1] > elite$pred[2*i]) {
      elite$bpred[(2*i - 1)] <- 1
      elite$bpred[2*i] <- 0
    } else {
      elite$bpred[(2*i - 1)] <- 0
      elite$bpred[(2*i)] <- 1
    }
  }

  winners <- elite %>% filter(bpred == 1) %>% select(Year, Team, Region, Seed)

  ##Final Four
  final <- data.frame(GID = c(1:2), Year = rep(2018, 2), Round = rep("Final Four", 2))
  t <- data.frame()
  for(i in 1:(nrow(winners)/2)) {
    temp <- data.frame(Region = winners$Region[(2*i-1)], HomeSeed = winners$Seed[(2*i-1)],
                       HomeTeam = winners$Team[(2*i-1)], AwaySeed = winners$Seed[(2*i)],
                       AwayTeam = winners$Team[(2*i)])
    t <- rbind(t, temp)
  }
  final <- cbind(final, t)

  final <- get_kp_data(as.data.frame(final), yearFilter = 2018, resultKnown = F, useAdj = useAdj)[[1]]

  final$pred <- predict(model, newdata = final, type = "response")

  gids <- unique(final$GID)
  w <- c(1:length(gids))
  for(i in 1:length(gids)) {
    if(final$pred[(2*i)-1] > final$pred[2*i]) {
      final$bpred[(2*i - 1)] <- 1
      final$bpred[2*i] <- 0
    } else {
      final$bpred[(2*i - 1)] <- 0
      final$bpred[(2*i)] <- 1
    }
  }

  winners <- final %>% filter(bpred == 1) %>% select(Year, Team, Region, Seed)
  winners

  ##Championship
  champ <- data.frame(GID = c(1), Year = rep(2018, 1), Round = rep("National Championship", 1))
  t <- data.frame()
  for(i in 1:(nrow(winners)/2)) {
    temp <- data.frame(Region = winners$Region[(2*i-1)], HomeSeed = winners$Seed[(2*i-1)],
                       HomeTeam = winners$Team[(2*i-1)], AwaySeed = winners$Seed[(2*i)],
                       AwayTeam = winners$Team[(2*i)])
    t <- rbind(t, temp)
  }
  champ <- cbind(champ, t)

  champ <- get_kp_data(as.data.frame(champ), yearFilter = 2018, resultKnown = F, useAdj = useAdj)[[1]]

  champ$pred <- predict(model, newdata = champ, type = "response")

  gids <- unique(champ$GID)
  w <- c(1:length(gids))
  for(i in 1:length(gids)) {
    if(champ$pred[(2*i)-1] > champ$pred[2*i]) {
      champ$bpred[(2*i - 1)] <- 1
      champ$bpred[2*i] <- 0
    } else {
      champ$bpred[(2*i - 1)] <- 0
      champ$bpred[(2*i)] <- 1
    }
  }

  winners <- champ %>% filter(bpred == 1) %>% select(Year, Team, Region, Seed)
  winners
}

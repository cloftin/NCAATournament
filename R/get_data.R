#'@export
get_data <- function() {
  kp <- plyr::ldply(list.files("data/kenpom_data/", pattern = ".csv"), function(fname) {
    t <- read.csv(file = paste0("data/kenpom_data/", fname), header = T, stringsAsFactors = F)
    t$year <- as.integer(substr(fname, 1, 4))
    return(t)
  })

  kp <- cleanKP(kp)

  results <- read.csv(file = "data/HistNCAATournamentResults.csv", header = T, stringsAsFactors = F)
  results$GID <- c(1:nrow(results))
  # kp %>% filter(year == results$Year[1], TeamName %in% c(results$HomeTeam[1], results$AwayTeam[1]))

  preddata <- data.frame()
  problems <- data.frame()
  for(i in 1:nrow(results)) {
    print(i/nrow(results))
    home <- kp %>% filter(year == results$Year[i], TeamName == results$HomeTeam[i])
    away <- kp %>% filter(year == results$Year[i], TeamName == results$AwayTeam[i])
    if(nrow(home) == 1 && nrow(away) == 1) {
      ##Home Team
      t <- as.data.frame(matrix(ncol = 0, nrow = 1))
      t$GID <- results$GID[i]
      t$Year <- results$Year[i]
      t$Round <- results$Round[i]
      t$Result <- ifelse(results$HomeScore[i] > results$AwayScore[i], 1, 0)
      t$Team <- results$HomeTeam[i]
      t$Seed <- results$HomeSeed[i]
      t$Score <- results$HomeScore[i]
      t$Tempo <- home$Tempo
      t$OffEff <- home$OE
      t$RankOE <- home$RankOE
      t$DefEff <- home$DE
      t$RankDE <- home$RankDE
      t$oTeam <- results$AwayTeam[i]
      t$oSeed <- results$AwaySeed[i]
      t$oScore <- results$AwayScore[i]
      t$oTempo <- away$Tempo
      t$oOffEff <- away$OE
      t$oRankOE <- away$RankOE
      t$oDefEff <- away$DE

      preddata <- rbind(preddata, t)

      t <- as.data.frame(matrix(ncol = 0, nrow = 1))
      t$GID <- results$GID[i]
      t$Year <- results$Year[i]
      t$Round <- results$Round[i]
      t$Result <- ifelse(results$AwayScore[i] > results$HomeScore[i], 1, 0)
      t$Team <- results$AwayTeam[i]
      t$Seed <- results$AwaySeed[i]
      t$Score <- results$AwayScore[i]
      t$Tempo <- away$Tempo
      t$OffEff <- away$OE
      t$RankOE <- away$RankOE
      t$DefEff <- away$DE
      t$RankDE <- away$RankDE
      t$oTeam <- results$HomeTeam[i]
      t$oSeed <- results$HomeSeed[i]
      t$oScore <- results$HomeScore[i]
      t$oTempo <- home$Tempo
      t$oOffEff <- home$OE
      t$oRankOE <- home$RankOE
      t$oDefEff <- home$DE

      preddata <- rbind(preddata, t)
    } else {
      t <- results[i,]
      problems <- rbind(problems, t)
    }
  }
  return(preddata)
}

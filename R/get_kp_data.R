#'@export
get_kp_data <- function(dat, yearExclude = NULL, yearFilter = NULL, resultKnown = T, useAdj = F) {
  kp <- plyr::ldply(list.files("data/kenpom_data/", pattern = ".csv"), function(fname) {
    t <- read.csv(file = paste0("data/kenpom_data/", fname), header = T, stringsAsFactors = F)
    t$year <- as.integer(substr(fname, 1, 4))
    return(t)
  })
  kp <- as.data.table(kp)
  kp[is.na(Season), Season:=year]
  kp[is.na(AdjEM), AdjEM:=(AdjOE-AdjDE)]
  kp <- kp[order(kp$year, -kp$AdjEM),]
  kp$RankAdjEM <- as.numeric(kp$RankAdjEM)
  kp[is.na(RankAdjEM), RankAdjEM:=rank(-AdjEM), by = list(year)]

  kp <- as.data.frame(kp)

  if(!is.null(yearExclude)) {
    kp <- kp %>% filter(year != yearExclude)
  }
  if(!is.null(yearFilter)) {
    kp <- kp %>% filter(year == yearFilter)
  }
  kp <- cleanKP(kp)

  if(useAdj) {
    kp$Tempo <- kp$AdjTempo
    kp$RankTempo <- kp$RankAdjTempo
    kp$OE <- kp$AdjOE
    kp$RankOE <- kp$RankAdjOE
    kp$DE <- kp$AdjDE
    kp$RankDE <- kp$RankAdjDE
    kp$EM <- kp$AdjEM
    kp$RankEM <- kp$RankAdjTempo
  }
  # kp %>% filter(year == results$Year[1], TeamName %in% c(results$HomeTeam[1], results$AwayTeam[1]))

  preddata <- data.frame()
  problems <- data.frame()
  for(i in 1:nrow(dat)) {
    print(i/nrow(dat))
    home <- kp %>% filter(year == dat$Year[i], TeamName == dat$HomeTeam[i])
    away <- kp %>% filter(year == dat$Year[i], TeamName == dat$AwayTeam[i])
    if(nrow(home) == 1 && nrow(away) == 1) {
      ##Home Team
      t <- as.data.frame(matrix(ncol = 0, nrow = 1))
      t$GID <- dat$GID[i]
      t$Year <- dat$Year[i]
      t$Round <- dat$Round[i]
      if("Region" %in% colnames(dat)) {
        t$Region <- dat$Region[i]
      }
      if(resultKnown) {
        t$Result <- ifelse(dat$HomeScore[i] > dat$AwayScore[i], 1, 0)
      }
      t$Team <- dat$HomeTeam[i]
      t$Seed <- dat$HomeSeed[i]
      if(resultKnown) {
        t$Score <- dat$HomeScore[i]
      }
      t$Tempo <- home$Tempo
      t$OffEff <- home$OE
      t$RankOE <- home$RankOE
      t$DefEff <- home$DE
      t$RankDE <- home$RankDE
      t$oTeam <- dat$AwayTeam[i]
      t$oSeed <- dat$AwaySeed[i]
      if(resultKnown) {
        t$oScore <- dat$AwayScore[i]
      }
      t$oTempo <- away$Tempo
      t$oOffEff <- away$OE
      t$oRankOE <- away$RankOE
      t$oDefEff <- away$DE
      t$oRankDE <- away$RankDE

      preddata <- rbind(preddata, t)

      t <- as.data.frame(matrix(ncol = 0, nrow = 1))
      t$GID <- dat$GID[i]
      t$Year <- dat$Year[i]
      t$Round <- dat$Round[i]
      if("Region" %in% colnames(dat)) {
        t$Region <- dat$Region[i]
      }
      if(resultKnown) {
        t$Result <- ifelse(dat$AwayScore[i] > dat$HomeScore[i], 1, 0)
      }
      t$Team <- dat$AwayTeam[i]
      t$Seed <- dat$AwaySeed[i]
      if(resultKnown) {
        t$Score <- dat$AwayScore[i]
      }
      t$Tempo <- away$Tempo
      t$OffEff <- away$OE
      t$RankOE <- away$RankOE
      t$DefEff <- away$DE
      t$RankDE <- away$RankDE
      t$oTeam <- dat$HomeTeam[i]
      t$oSeed <- dat$HomeSeed[i]
      if(resultKnown) {
        t$oScore <- dat$HomeScore[i]
      }
      t$oTempo <- home$Tempo
      t$oOffEff <- home$OE
      t$oRankOE <- home$RankOE
      t$oDefEff <- home$DE
      t$oRankDE <- home$RankDE

      preddata <- rbind(preddata, t)
    } else {
      t <- dat[i,]
      problems <- rbind(problems, t)
    }
  }
  return(list(preddata, problems))
}

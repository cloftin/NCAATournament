#'@export
create_model <- function(fromSample = T, yearToTest = NULL, useAdj = F) {

  if(fromSample && !is.null(yearToTest)) {
    stop("Cannot make model for both specific year and from random sample.")
  }

  results <- read.csv(file = "data/HistNCAATournamentResults.csv", header = T, stringsAsFactors = F)
  results$GID <- c(1:nrow(results))

  dat <- if(!is.null(yearToTest) && yearToTest == 2018) {
    get_kp_data(results, yearExclude = yearToTest, resultKnown = F, useAdj = useAdj)
  } else if(!is.null(yearToTest)) {
    get_kp_data(results, yearExclude = yearToTest, resultKnown = T, useAdj = useAdj)
  } else {
    get_kp_data(results, yearExclude = 2018, useAdj = useAdj)
  }

  dat <- dat[[1]]
  set.seed(101)

  dat$Result <- as.factor(dat$Result)

  test <- NA
  train <- if(fromSample) {
    sample <- sample.split(dat$Result, SplitRatio = 0.70)
    test <- subset(dat, sample == F)
    subset(dat, sample == T)
  } else {
    if(!is.null(yearToTest)) {
      dat %>% filter(Year != yearToTest)
    } else {
      dat
    }
  }


  lmmodel <- stats::glm(formula = "Result ~ Tempo + OffEff + RankOE + DefEff + RankDE + oTempo + oOffEff + oRankOE + oDefEff",
                        data = train, family = binomial(link = "logit"))
  return(list(lmmodel, test))
}

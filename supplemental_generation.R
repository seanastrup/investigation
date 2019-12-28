setwd("~/Projects/merge")
library(tidyverse)
library(rlang)

GenerateCalculations <- function(df, condition, experiment) { 
  groupByVars <- rlang::syms(condition)
  
  summarizedDf <- df %>% 
    na.omit() %>%
    dplyr::group_by(ParticipantNumber, !!! groupByVars) %>% 
    dplyr::mutate(
      Hit = ifelse(Emotion == 0 & Response == 0, 1, 0), 
      FalseAlarm = ifelse(Emotion == 1 & Response == 0, 1, 0),
      Miss = ifelse(Emotion == 0 & Response == 1, 1, 0),
      CorrectRejection = ifelse(Emotion == 1 & Response == 1, 1, 0),
      FAOpportunity = ifelse(Emotion == 1, 1, 0),
      HitOpportunity = ifelse(Emotion == 0, 1, 0)
    ) %>% 
    dplyr::summarise(
      totalHits = sum(Hit), 
      totalFalseAlarms = sum(FalseAlarm), 
      totalMisses = sum(Miss), 
      totalCorrectRejections = sum(CorrectRejection),
      totalFAOpportunities = sum(FAOpportunity),
      totalHitOpportunities = sum(HitOpportunity)
    ) %>%
    dplyr::mutate(
      HitRate = ifelse(
        totalHits / (totalHits + totalMisses) == 1, 
        1 - 1 / (2 * totalHitOpportunities),
        ifelse(
          totalHits / (totalHits + totalMisses) == 0,
          1 / (2 * totalHitOpportunities),
          totalHits / (totalHits + totalMisses)
        )
      ),
      FalseAlarmRate = ifelse(
        totalFalseAlarms / totalFAOpportunities == 1,
        1 - 1 / (2 * totalFAOpportunities),
        ifelse(
          totalFalseAlarms / totalFAOpportunities == 0,
          1 / (2 * totalFAOpportunities),
          totalFalseAlarms / totalFAOpportunities
        )
      ),
      HitRateReplaced = ifelse(
        totalHits / (totalHits + totalMisses) == 0 | totalHits / (totalHits + totalMisses) == 1, 
        1, 
        0
      ),
      FalseAlarmReplaced = ifelse(
        totalFalseAlarms / totalFAOpportunities == 0 | totalFalseAlarms / totalFAOpportunities == 1,
        1,
        0
      ),
      HRZscore = qnorm(HitRate),
      FAZscore = qnorm(FalseAlarmRate),
      dPrime = HRZscore - FAZscore,
      Criterion = -0.5 * (HRZscore + FAZscore),
      Experiment = experiment
    ) 
  
  return(summarizedDf)
}

supplementalFear <- GenerateCalculations(
  fear, 
  c("GroupSize", "Intensity"), 
  "fear"
)

supplementalNoise <- GenerateCalculations(
  noise,
  c("GroupSize", "Noise"),
  "noise"
)

supplementalIdentity <- GenerateCalculations(
  identity,
  c("Intensity", "Condition"),
  "identity"
)

supplementalSizeIntCollapse <- GenerateCalculations(
  size,
  c("GroupSize"),
  "group_size_inensity_collapsed"
)

supplementalSizeGroupCollapse <- GenerateCalculations(
  size,
  c("Intensity"),
  "group_size_group_collapsed"
)

supplementalGender <- GenerateCalculations(
  gender,
  c("GroupSize", "Gender", "Intensity"),
  "gender"
)

supplementalCrowdVsSingle <- GenerateCalculations(
  crowdVsSingle,
  c("GroupSize", "Intensity"),
  "crowdVsSingle"
)

allSupplemental <- rbind(
  supplementalCrowdVsSingle,
  supplementalFear, 
  supplementalNoise,
  supplementalSizeIntCollapse,
  supplementalSizeGroupCollapse,
  supplementalIdentity,
  supplementalGender
)

write.csv(allSupplemental, "test_delete.csv", row.names = FALSE)



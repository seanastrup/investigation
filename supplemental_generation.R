setwd("~/Projects/merge")
library(tidyverse)
library(rlang)

# source merged dataframes must exist prior to supplemental generation

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

#Experiment supplementals generation
#################### 

supplementalFear <- GenerateCalculations(
  fear, 
  c("GroupSize", "Intensity"), 
  "fear"
) %>% 
  dplyr::filter(ParticipantNumber != 31)

supplementalNoise <- GenerateCalculations(
  noise,
  c("GroupSize", "Noise"),
  "noise"
)

supplementalIdentity <- GenerateCalculations(
  identity,
  c("Intensity", "Condition"),
  "identity"
) %>% 
  dplyr::filter(ParticipantNumber < 31)

supplementalSizeIntCollapse <- GenerateCalculations(
  size,
  c("GroupSize"),
  "group_size_intensity_collapsed"
) %>% 
  dplyr::filter(ParticipantNumber != 31)

supplementalSizeGroupCollapse <- GenerateCalculations(
  size,
  c("Intensity"),
  "group_size_group_collapsed"
) %>% 
  dplyr::filter(ParticipantNumber != 31)

supplementalGender <- GenerateCalculations(
  gender,
  c("GroupSize", "Gender", "Intensity"),
  "gender"
)

supplementalCrowdTypeGender <- GenerateCalculations(
  gender,
  c("GroupType", "IntensityType"),
  "gender_crowd_type"
)

supplementalCrowdVsSingle <- GenerateCalculations(
  crowdVsSingle,
  c("GroupSize", "Intensity"),
  "crowdVsSingle"
)

allSupplemental <- 
  rbind(
    supplementalCrowdVsSingle,
    supplementalFear, 
    supplementalNoise,
    supplementalSizeIntCollapse,
    supplementalSizeGroupCollapse,
    supplementalIdentity,
    supplementalGender,
    supplementalCrowdTypeGender
  ) %>% 
  dplyr::rename(
    IdentityCondition = Condition,
    TotalHits = totalHits,
    TotalCorrectRejections = totalCorrectRejections,
    TotalFalseAlarms = totalFalseAlarms, 
    TotalMisses = totalMisses
  ) %>%
  dplyr::select(
    Experiment, ParticipantNumber, GroupSize, Intensity, Noise, 
    IdentityCondition, Gender, dPrime, Criterion, TotalHits, TotalFalseAlarms, 
    TotalMisses, TotalCorrectRejections, HitRate, FalseAlarmRate, 
    HitRateReplaced, FalseAlarmReplaced, GroupType, IntensityType
  )

allSupplemental$ParticipantNumber <- as.factor(allSupplemental$ParticipantNumber)
allSupplemental$Experiment <- as.factor(allSupplemental$Experiment)
allSupplemental$GroupSize <- as.factor(allSupplemental$GroupSize)
allSupplemental$Intensity <- as.factor(allSupplemental$Intensity)
allSupplemental$Noise <- as.factor(allSupplemental$Noise)
allSupplemental$IdentityCondition <- as.factor(allSupplemental$IdentityCondition)
allSupplemental$Gender <- as.factor(allSupplemental$Gender)
allSupplemental$GroupType <- as.factor(allSupplemental$GroupType)
allSupplemental$IntensityType <- as.factor(allSupplemental$IntensityType)

######################################
# Write all supplementals to csv
write.csv(allSupplemental, "test_delete.csv", row.names = FALSE)


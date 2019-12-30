setwd("~/Projects/merge")
library(tidyverse)
library(rlang)

# Controls
############################
CONTROLS <- list(
  fear = list(
    investigation = "fear", 
    fields = 10,
    conditions = c("1.1", "1.10", "1.20", "1.30", "1.40", "1.50",         # Single conditions
                   "12.1", "12.10", "12.20", "12.30", "12.40", "12.50"),  # group conditions
    zackCompiledFields = c(1, 9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)
  ),
  crowd_vs_single = list(
    investigation = "crowd_vs_single",
    fields = 14
  ),
  gender = list(
    investigation = "gender",
    fields = 12
  ),
  identity = list(
    investigation = "identity", 
    fields = 12
  ),
  noise = list(
    investigation = "noise",
    fields = 11, 
    conditions = c("1.0", "1.15", "1.30", "1.45", "1.60", "1.75", "1.90", 
                   "12.0", "12.15", "12.30", "12.45", "12.60", "12.75", "12.90"),
    zackCompiledFields = c(1, 9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119, 
                           129, 139)
  ),
  size = list(
    investigation = "size",
    fields = 10
  )
)


# Create data
############################

MergeFiles <- function(investigation, fields) {
  setwd(sprintf("~/Projects/merge/data/%s", investigation))
  fileList <- 
    list.files(path = sprintf("~/Projects/merge/data/%s", investigation))
  
  mergeList <-
    lapply(fileList, function(file) {
      participantNumber <- 
        as.numeric(gsub("\\..*", "", gsub(".*_", "", file)))
      
      print(
        sprintf(
          "Merging participant %s for the %s investigation", 
          participantNumber, 
          investigation
        )
      )
      
      df <- readxl::read_excel(file, range = cell_cols(1:fields))
      
      if (investigation == "crowd_vs_single") {
        df$ParticipantNumber <- df$Observer
      } else {
        df$ParticipantNumber <- participantNumber  
      }
      
      return(df)
    })
  
  mergedFiles <- do.call(rbind.data.frame, mergeList)
  setwd("~/Projects/merge")
  
  return(mergedFiles)
}

# merge all individual's data
fear <- MergeFiles(CONTROLS$fear$investigation, CONTROLS$fear$fields)
crowdVsSingle_repair <- MergeFiles(
  CONTROLS$crowd_vs_single$investigation, 
  CONTROLS$crowd_vs_single$fields
)
gender_repair <- MergeFiles(CONTROLS$gender$investigation, CONTROLS$gender$fields)
identity <- MergeFiles(CONTROLS$identity$investigation, CONTROLS$identity$fields)
noise <- MergeFiles(CONTROLS$noise$investigation, CONTROLS$noise$fields)
size <- MergeFiles(CONTROLS$size$investigation, CONTROLS$size$fields)

# clean experiment 1 data because it sucks
crowdVsSingle <- crowdVsSingle_repair %>% 
  dplyr::rename(
    GroupSize = Group,
    Emotion_DELETE = Emotion,
    Response = response
  ) %>% 
  dplyr::filter(Emotion_DELETE != 1) %>% 
  dplyr::mutate(
    GroupSize = ifelse(
      GroupSize == 1, 
      12,
      ifelse(
        GroupSize == 0,
        1,
        GroupSize
      )
    ),
    Intensity = ifelse(
      Emotion_DELETE == 2 | Emotion_DELETE == 7,
      10,
      ifelse(
        Emotion_DELETE == 3 | Emotion_DELETE == 8,
        20,
        ifelse(
          Emotion_DELETE == 4 | Emotion_DELETE == 9,
          30,
          ifelse(
            Emotion_DELETE == 5 | Emotion_DELETE == 10,
            40,
            ifelse(
              Emotion_DELETE == 6 | Emotion_DELETE == 11,
              50,
              NA
            )
          )
        )
      )
    ),
    Emotion = ifelse(
      Emotion_DELETE %in% c(2:6),
      1,
      ifelse(
        Emotion_DELETE %in% c(7:11),
        0,
        NA
      )
    )
  ) %>% 
  dplyr::select(-Emotion_DELETE)

# Also clean gender data because it sucks
gender <- gender_repair %>% 
  dplyr::mutate(
    GroupType = ifelse(
      Gender == 1 & GroupSize == 1, 
      1, 
      ifelse(
        Gender == 2 & GroupSize == 1,
        2,
        ifelse(
          Gender == 1 & GroupSize == 12,
          3,
          ifelse(
            Gender == 2 & GroupSize == 12,
            4,
            ifelse(
              Gender == 3 & GroupSize == 12, 
              5, 
              NA
            )
          )
        )
      )
    ), 
    IntensityType = ifelse(
      Intensity %in% c(1, 10, 20),
      "Low",
      ifelse(
        Intensity %in% c(30, 40, 50),
        "High",
        NA
      )
    )
  )

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

# Create supplementals
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
    IdentityCondition, Gender, GroupType, IntensityType,dPrime, Criterion, 
    TotalHits, TotalFalseAlarms, TotalMisses, TotalCorrectRejections, HitRate, 
    FalseAlarmRate, HitRateReplaced, FalseAlarmReplaced
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

#############################
# Confidence Intervals
GetConfidenceIntervals <- function(df, condition) {
  groupByVars <- rlang::syms(condition)
  
  confidenceIntervalDf <- df %>% 
    dplyr::group_by(!!! groupByVars) %>% 
    dplyr::summarise(
      mean_dPrime = mean(dPrime),
      mean_Criterion = mean(Criterion),
      stdDprime = sd(dPrime),
      stdCriterion = sd(Criterion),
      numObs = n()
    ) %>% 
    dplyr::mutate(
      intensityPercentage = ifelse(
        Intensity == 1, 2,
        ifelse(
          Intensity == 10, 20,
          ifelse(
            Intensity == 20, 40, 
            ifelse(
              Intensity == 30, 60,
              ifelse(
                Intensity == 40, 80,
                ifelse(
                  Intensity == 50, 100,
                  NA
                )
              )
            )
          )
        )
      ),
      errorDprime = qnorm(0.975) * stdDprime / sqrt(numObs),
      errorCriterion = qnorm(0.975) * stdCriterion / sqrt(numObs),
      dPrimeLow95 = mean_dPrime - errorDprime,
      dPrimeHigh95 = mean_dPrime + errorDprime,
      CriterionLow95 = mean_Criterion - errorCriterion,
      CriterionHigh95 = mean_Criterion + errorCriterion,
      SEMdPrime = stdDprime / sqrt(numObs),
      SEMCriterion = stdCriterion / sqrt(numObs)
    ) %>%
    dplyr::select(
      !!! groupByVars, intensityPercentage, mean_dPrime, dPrimeLow95,
      dPrimeHigh95, mean_Criterion, CriterionLow95, CriterionHigh95, SEMdPrime,
      SEMCriterion
    )
  
  return(confidenceIntervalDf)
}

GetOverallMeansConfidenceIntervals <- function(df, condition, overallMeanGrouping) {
  groupByVars <- rlang::syms(condition)
  overallGroups <- rlang::syms(overallMeanGrouping)
  
  confidenceIntervalDf <- df %>% 
    dplyr::group_by(!!! groupByVars) %>% 
    dplyr::summarise(
      mean_dPrime = mean(dPrime),
      mean_Criterion = mean(Criterion),
      stdDprime = sd(dPrime),
      stdCriterion = sd(Criterion),
      numObs = n()
    ) %>% 
    dplyr::mutate(
      intensityPercentage = ifelse(
        Intensity == 1, 2,
        ifelse(
          Intensity == 10, 20,
          ifelse(
            Intensity == 20, 40, 
            ifelse(
              Intensity == 30, 60,
              ifelse(
                Intensity == 40, 80,
                ifelse(
                  Intensity == 50, 100,
                  NA
                )
              )
            )
          )
        )
      )
    ) 
  
  overallMean <- supplementalCrowdVsSingle %>% 
    dplyr::group_by(!!! overallGroups) %>% 
    
    dplyr::summarize(
      overall_mean_dPrime = mean(dPrime), 
      overall_mean_Criterion = mean(Criterion)
    )
  
  data <- dplyr::inner_join(confidenceIntervalDf, overallMean) %>% 
    dplyr::mutate(
      errorDprime = qnorm(0.975) * stdDprime / sqrt(numObs),
      errorCriterion = qnorm(0.975) * stdCriterion / sqrt(numObs),
      V2_dPrimeLow95 = overall_mean_dPrime - errorDprime,
      V2_dPrimeHigh95 = overall_mean_dPrime + errorDprime,
      V2_CriterionLow95 = overall_mean_Criterion - errorCriterion,
      V2_CriterionHigh95 = overall_mean_Criterion + errorCriterion
    )
  
  return(data)
}

confidenceIntervalCrowdVsSingle <- GetConfidenceIntervals(
  supplementalCrowdVsSingle, 
  c("GroupSize", "Intensity")
)

confidenceIntervalFear <- GetConfidenceIntervals(
  supplementalFear,
  c("GroupSize", "Intensity")
)


overallConfidenceIntervalCrowdVsSingle <- GetOverallMeansConfidenceIntervals(
  supplementalCrowdVsSingle, 
  c("GroupSize", "Intensity"), c("GroupSize")
)

#############################
# Write all supplementals to csv
write.csv(
  allSupplemental, 
  "supplementals/investigation_1_data_supplemental.csv", 
  row.names = FALSE
)
write.csv(
  confidenceIntervalCrowdVsSingle, 
  "supplementals/plot_info/crowd_vs_single_plot_info.csv", 
  row.names = FALSE
)




ggplot(
  data = overallConfidenceIntervalCrowdVsSingle, 
  aes(
    x = intensityPercentage, 
    y = mean_Criterion, 
    group = GroupSize, 
    color = GroupSize
  )
) + 
  ggplot2::geom_line() + 
  ggplot2::geom_errorbar(
    aes(
      ymin = V2_CriterionLow95, 
      ymax = V2_CriterionHigh95
    ), 
    width = 0.2
  )





























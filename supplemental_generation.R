setwd("~/Projects/merge")
library(tidyverse)
library(rlang)
library(readxl)

########## Controls #################
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



########### Create data #####################

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
  dplyr::rename(ResponseTime = RT) %>%
  dplyr::select(-Emotion_DELETE)

# Also clean gender data because it sucks
gender <- gender_repair %>% 
  dplyr::mutate(
    GroupType = ifelse( # someone should make a vectorized switch() for R
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
  
  reactionTimeDf <- df %>% 
    dplyr::mutate(ResponseTime = as.numeric(ResponseTime)) %>%
    dplyr::group_by(ParticipantNumber, !!! groupByVars) %>% 
    dplyr::filter(
      !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
    ) %>%
    dplyr::summarize(meanReponseTime = mean(ResponseTime))
  
  summarizedDf <- df %>% 
    dplyr::select(-ResponseTime) %>%
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
  
  finalDf <- dplyr::inner_join(
    reactionTimeDf,
    summarizedDf
  )
  
  return(finalDf)
}
########### Create supplementals ##################### 

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
    IdentityCondition, Gender, GroupType, IntensityType, dPrime, Criterion, 
    TotalHits, TotalFalseAlarms, TotalMisses, TotalCorrectRejections, HitRate, 
    FalseAlarmRate, HitRateReplaced, FalseAlarmReplaced, meanReponseTime
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

########### Confidence Intervals ##################
GetConfidenceIntervals <- function(df, condition, experiment) {
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
      # intensityPercentage = ifelse(
      #   Intensity == 1, 2,
      #   ifelse(
      #     Intensity == 10, 20,
      #     ifelse(
      #       Intensity == 20, 40,
      #       ifelse(
      #         Intensity == 30, 60,
      #         ifelse(
      #           Intensity == 40, 80,
      #           ifelse(
      #             Intensity == 50, 100,
      #             NA
      #           )
      #         )
      #       )
      #     )
      #   )
      # ),
      errorDprime = qnorm(0.975) * stdDprime / sqrt(numObs),
      errorCriterion = qnorm(0.975) * stdCriterion / sqrt(numObs),
      dPrimeLow95 = mean_dPrime - errorDprime,
      dPrimeHigh95 = mean_dPrime + errorDprime,
      CriterionLow95 = mean_Criterion - errorCriterion,
      CriterionHigh95 = mean_Criterion + errorCriterion,
      experiment = experiment
    ) %>%
    dplyr::select(
      experiment, !!! groupByVars, 
      # intensityPercentage,
      mean_dPrime, dPrimeLow95,
      dPrimeHigh95, mean_Criterion, CriterionLow95, CriterionHigh95
    )
  
  return(confidenceIntervalDf)
}

confidenceIntervalCrowdVsSingle <- GetConfidenceIntervals(
  supplementalCrowdVsSingle,
  c("GroupSize", "Intensity"),
  "crowdVsSingle"
)

confidenceIntervalFear <- GetConfidenceIntervals(
  supplementalFear,
  c("GroupSize", "Intensity"),
  "fear"
)

confidenceIntervalNoise <- GetConfidenceIntervals(
  supplementalNoise,
  c("GroupSize", "Noise"),
  "noise"
)

confidenceIntervalGroupSizeIntensityCollapsed <- GetConfidenceIntervals(
  supplementalSizeIntCollapse,
  c("GroupSize"),
  "crowd_size_intensity_collapsed"
)

confidenceIntervalGroupSizeGroupCollapsed <- GetConfidenceIntervals(
  supplementalSizeGroupCollapse,
  c("Intensity"),
  "crowd_size_group_collapsed"
)

confidenceIntervalIdentity <- GetConfidenceIntervals(
  supplementalIdentity,
  c("Intensity", "Condition"),
  "identity"
)

confidenceIntervalGender <- GetConfidenceIntervals(
  supplementalGender,
  c("GroupSize", "Gender", "Intensity"),
  "gender"
)

########### RT ############
GetCrowdVsSingleAnova <- function(crowdVsSingleDF, groupingCondition = c("Intensity"), outlierByParticipant = FALSE) {
  groupingVar <- rlang::syms(groupingCondition)
  if (outlierByParticipant) {
    crowdVsSingleResponseTime <- crowdVsSingleDF %>%
      dplyr::mutate(ResponseTime = as.numeric(ResponseTime)) %>%
      na.omit() %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(
        ParticipantNumber, Emotion, GroupSize, Intensity
      ) %>%
      dplyr::summarize(meanResponseTime = mean(ResponseTime))  
    
    extraPltData <- crowdVsSingleDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
      na.omit() %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  } else {
    crowdVsSingleResponseTime <- crowdVsSingleDF %>%
      dplyr::mutate(ResponseTime = as.numeric(ResponseTime)) %>%
      na.omit() %>%
      dplyr::group_by(
        ParticipantNumber, Emotion, GroupSize, Intensity
      ) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanResponseTime = mean(ResponseTime))
    
    extraPltData <- crowdVsSingleDF %>% 
      dplyr::mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
      na.omit() %>%
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  }
  
  names(extraPltData) <- c("ParticipantNumber", "Variable", "MeanRT")
  extraPltData$Variable <- as.factor(extraPltData$Variable)
  
  plt <- ggplot2::ggplot(data = extraPltData, aes(x = Variable, y = MeanRT)) + 
    ggplot2::geom_boxplot(aes(group = Variable)) + 
    ggplot2::ggtitle(sprintf("Crowd Vs Single MeanRT & %s", groupingCondition)) + 
    ggplot2::xlab(groupingCondition)
  
  crowdVsSingleResponseTime$ParticipantNumber <- as.factor(
    crowdVsSingleResponseTime$ParticipantNumber
  )
  crowdVsSingleResponseTime$Emotion <- as.factor(
    crowdVsSingleResponseTime$Emotion
  )
  crowdVsSingleResponseTime$GroupSize <- as.factor(
    crowdVsSingleResponseTime$GroupSize
  )
  crowdVsSingleResponseTime$Intensity <- as.factor(
    crowdVsSingleResponseTime$Intensity
  )
    
  anova <-
    summary(
      aov(
        meanResponseTime ~  GroupSize * Emotion * Intensity + Error(
          ParticipantNumber / (GroupSize * Emotion * Intensity)
        ),
        data = crowdVsSingleResponseTime
      )
    )
  
  outList <- list(
    anova = anova,
    plot = plt
  )
  
  return(outList)
}

GetFearAnova <- function(fearDF, groupingCondition = c("Intensity"), outlierByParticipant = FALSE) {
  groupingVar <- rlang::syms(groupingCondition)
  
  if (outlierByParticipant) {
    fearRT<- fearDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
    
    extraPltData <- fearDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  } else {
    fearRT<- fearDF %>%
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
    
    extraPltData <- fearDF %>% 
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  }
  
  names(extraPltData) <- c("ParticipantNumber", "Variable", "MeanRT")
  extraPltData$Variable <- as.factor(extraPltData$Variable)
  
  plt <- ggplot2::ggplot(data = extraPltData, aes(x = Variable, y = MeanRT)) + 
    ggplot2::geom_boxplot(aes(group = Variable)) + 
    ggplot2::ggtitle(sprintf("Fear MeanRT & %s", groupingCondition)) + 
    ggplot2::xlab(groupingCondition) 
  
  fearRT$ParticipantNumber <- as.factor(fearRT$ParticipantNumber)
  fearRT$GroupSize <- as.factor(fearRT$GroupSize)
  fearRT$Emotion <- as.factor(fearRT$Emotion)
  fearRT$Intensity <- as.factor(fearRT$Intensity)
  
  anova <-summary(
    aov(
      meanRT ~ Emotion * Intensity * GroupSize + Error(
        ParticipantNumber / (Emotion * Intensity * GroupSize)
      ),
      data = fearRT
    )
  )
  
  outList <- list(
    anova = anova,
    plot = plt
  )

  return(outList)
}

GetNoiseAnova <- function(noiseDF, groupingCondition = c("Intensity"), outlierByParticipant = FALSE) {
  groupingVar <- rlang::syms(groupingCondition)
  
  if (outlierByParticipant) {
    noiseRT <- noiseDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(
        ParticipantNumber, GroupSize, Emotion, Noise
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))  
    
    extraPltData <- noiseDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  } else {
    noiseRT <- noiseDF %>%
      dplyr::group_by(
        ParticipantNumber, GroupSize, Emotion, Noise
      ) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))
    
    extraPltData <- noiseDF %>% 
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  }
  
  names(extraPltData) <- c("ParticipantNumber", "Variable", "MeanRT")
  extraPltData$Variable <- as.factor(extraPltData$Variable)
  
  plt <- ggplot2::ggplot(data = extraPltData, aes(x = Variable, y = MeanRT)) + 
    ggplot2::geom_boxplot(aes(group = Variable)) + 
    ggplot2::ggtitle(sprintf("Noise MeanRT & %s", groupingCondition)) + 
    ggplot2::xlab(groupingCondition)
  
  noiseRT$ParticipantNumber <- as.factor(noiseRT$ParticipantNumber)
  noiseRT$GroupSize <- as.factor(noiseRT$GroupSize)
  noiseRT$Emotion <- as.factor(noiseRT$Emotion)
  noiseRT$Noise <- as.factor(noiseRT$Noise)
  
  
  anova <- 
    summary(
      aov(
        meanRT ~  GroupSize * Emotion * Noise + Error(
          ParticipantNumber / (GroupSize * Emotion * Noise)
        ), 
        data = noiseRT
      )
    )  
  
  outList <- list(
    anova = anova,
    plot = plt
  )
  
  return(outList)
}

GetSizeAnova <- function(sizeDF, groupingCondition = c("Intensity"), outlierByParticipant = FALSE) {
  groupingVar <- rlang::syms(groupingCondition)
  
  if (outlierByParticipant) {
    sizeRT <- sizeDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(
        ParticipantNumber, GroupSize, Emotion, Intensity
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))  
    
    extraPltData <- sizeDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  } else {
    sizeRT <- sizeDF %>%
      dplyr::group_by(
        ParticipantNumber, GroupSize, Emotion, Intensity
      ) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))
    
    extraPltData <- sizeDF %>% 
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  }
  
  names(extraPltData) <- c("ParticipantNumber", "Variable", "MeanRT")
  extraPltData$Variable <- as.factor(extraPltData$Variable)
  
  plt <- ggplot2::ggplot(data = extraPltData, aes(x = Variable, y = MeanRT)) + 
    ggplot2::geom_boxplot(aes(group = Variable)) + 
    ggplot2::ggtitle(sprintf("Size MeanRT & %s", groupingCondition)) + 
    ggplot2::xlab(groupingCondition)
  
  sizeRT$ParticipantNumber <- as.factor(sizeRT$ParticipantNumber)
  sizeRT$GroupSize <- as.factor(sizeRT$GroupSize)
  sizeRT$Emotion <- as.factor(sizeRT$Emotion)
  sizeRT$Intensity <- as.factor(sizeRT$Intensity)
  
  anova <- 
    summary(
      aov(
        meanRT ~  GroupSize * Emotion * Intensity + Error(
          ParticipantNumber / (GroupSize * Emotion * Intensity)
        ), 
        data = sizeRT
      )
    )  
  
  outList <- list(
    anova = anova,
    plot = plt
  )
  
  return(outList)
}

GetIdentityAnova <- function(identityDF, groupingCondition = c("Intensity"), outlierByParticipant = FALSE) {
  groupingVar <- rlang::syms(groupingCondition)
  
  if (outlierByParticipant) {
    identityRT <- identityDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(
        ParticipantNumber, Condition, Emotion, Intensity
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))  
    
    extraPltData <- identityDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  } else {
    identityRT <- identityDF %>%
      dplyr::group_by(
        ParticipantNumber, Condition, Emotion, Intensity
      ) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))
    
    extraPltData <- identityDF %>% 
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  }
  
  names(extraPltData) <- c("ParticipantNumber", "Variable", "MeanRT")
  extraPltData$Variable <- as.factor(extraPltData$Variable)
  
  plt <- ggplot2::ggplot(data = extraPltData, aes(x = Variable, y = MeanRT)) + 
    ggplot2::geom_boxplot(aes(group = Variable)) + 
    ggplot2::ggtitle(sprintf("Identity MeanRT & %s", groupingCondition)) + 
    ggplot2::xlab(groupingCondition)
  
  identityRT$ParticipantNumber <- as.factor(identityRT$ParticipantNumber)
  identityRT$Condition <- as.factor(identityRT$Condition)
  identityRT$Emotion <- as.factor(identityRT$Emotion)
  identityRT$Intensity <- as.factor(identityRT$Intensity)
  
  anova <- 
    summary(
      aov(
        meanRT ~  Emotion * Condition * Intensity + Error(
          ParticipantNumber / (Emotion * Condition * Intensity)
        ), 
        data = identityRT
      )
    )  
  
  outList <- list(
    anova = anova,
    plot = plt
  )
  
  return(outList)
}

GetGenderAnova_GenderGroupType <- function(genderDF, groupingCondition = c("Intensity"), outlierByParticipant = FALSE) {
  groupingVar <- rlang::syms(groupingCondition)
  
  if (outlierByParticipant) {
    genderRT <- genderDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, GroupType, Emotion, Intensity) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))
    
    extraPltData <- genderDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  } else {
    genderRT <- genderDF %>%
      dplyr::group_by(ParticipantNumber, GroupType, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))  
    
    extraPltData <- genderDF %>% 
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  }
  
  names(extraPltData) <- c("ParticipantNumber", "Variable", "MeanRT")
  extraPltData$Variable <- as.factor(extraPltData$Variable)
  
  plt <- ggplot2::ggplot(data = extraPltData, aes(x = Variable, y = MeanRT)) + 
    ggplot2::geom_boxplot(aes(group = Variable)) + 
    ggplot2::ggtitle(sprintf("Gender MeanRT & %s", groupingCondition)) + 
    ggplot2::xlab(groupingCondition)
  
  genderRT$ParticipantNumber <- as.factor(genderRT$ParticipantNumber)
  genderRT$GroupType <- as.factor(genderRT$GroupType)
  genderRT$Emotion <- as.factor(genderRT$Emotion)
  genderRT$Intensity <- as.factor(genderRT$Intensity)
  
  anova <-
    summary(
      aov(
        meanRT ~ GroupType * Emotion * Intensity + Error(
          ParticipantNumber / (GroupType * Emotion * Intensity)
        ), 
        data = genderRT
      )
    )  
  
  outList <- list(
    anova = anova, 
    plot = plt
  )
  
  return(outList)
}

GetGenderAnova_Gender <- function(genderDF, groupingCondition = c("Intensity"), outlierByParticipant = FALSE) {
  groupingVar <- rlang::syms(groupingCondition)
  
  if (outlierByParticipant) {
    genderRT <- genderDF %>%
      dplyr::filter(Gender != 3) %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, Gender, GroupSize, Emotion, Intensity) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))
    
    extraPltData <- genderDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  } else {
    genderRT <- genderDF %>%
      dplyr::filter(Gender != 3) %>%
      dplyr::group_by(ParticipantNumber, Gender, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))  
    
    extraPltData <- genderDF %>% 
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::group_by(ParticipantNumber, !!! groupingVar) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))
  }
  
  names(extraPltData) <- c("ParticipantNumber", "Variable", "MeanRT")
  extraPltData$Variable <- as.factor(extraPltData$Variable)
  
  plt <- ggplot2::ggplot(data = extraPltData, aes(x = Variable, y = MeanRT)) + 
    ggplot2::geom_boxplot(aes(group = Variable)) + 
    ggplot2::ggtitle(sprintf("Gender MeanRT & %s", groupingCondition)) + 
    ggplot2::xlab(groupingCondition)
  
  genderRT$ParticipantNumber <- as.factor(genderRT$ParticipantNumber)
  genderRT$Gender <- as.factor(genderRT$Gender)
  genderRT$GroupSize <- as.factor(genderRT$GroupSize)
  genderRT$Emotion <- as.factor(genderRT$Emotion)
  genderRT$Intensity <- as.factor(genderRT$Intensity)
  
  anova <-
    summary(
      aov(
        meanRT ~ Gender * GroupSize * Emotion * Intensity + Error(
          ParticipantNumber / (Gender * GroupSize * Emotion * Intensity)
        ), 
        data = genderRT
      )
    )  
  
  outList <- list(
    anova = anova,
    plot = plt
  )
  
  return(outList)
}

crowdVsSingleRTAnova <- GetCrowdVsSingleAnova(crowdVsSingle)$anova
crowdVsSingleRTEmotionPlot <- GetCrowdVsSingleAnova(
  crowdVsSingle, 
  c("Emotion")
)$plot
crowdVsSingleRTIntensityPlot <- GetCrowdVsSingleAnova(
  crowdVsSingle, 
  c("Intensity")
)$plot
crowdVsSingleRTGroupSizePlot <- GetCrowdVsSingleAnova(
  crowdVsSingle, 
  c("GroupSize")
)$plot

fearRTAnova <- GetFearAnova(fear)$anova
fearRTEmotionPlot <- GetFearAnova(fear, c("Emotion"))$plot
fearRTIntenstiyPlot <- GetFearAnova(fear, c("Intensity"))$plot
fearRTGroupSizePlot <- GetFearAnova(fear, c("GroupSize"))$plot

noiseRTAnova <- GetNoiseAnova(noise)$anova
noiseRTEmotionPlot <- GetNoiseAnova(noise, c("Emotion"))$plot
noiseRTNoisePlot <- GetNoiseAnova(noise, c("Noise"))$plot
noiseRTGroupSizePlot <- GetNoiseAnova(noise, c("GroupSize"))$plot

groupSizeRTAnova <- GetSizeAnova(size)$anova
groupSizeRTEmotionPlot <- GetSizeAnova(size, c("Emotion"))$plot
groupSizeRTIntensityPlot <- GetSizeAnova(size, c("Intensity"))$plot
groupSizeRTGroupSizePlot <- GetSizeAnova(size, c("GroupSize"))$plot

identityRTAnova <- GetIdentityAnova(identity)$anova
identityRTEmotionPlot <- GetIdentityAnova(identity, c("Emotion"))$plot
identityRTIntensityPlot <- GetIdentityAnova(identity, c("Intensity"))$plot
identityRTConditionPlot <- GetIdentityAnova(identity, c("Condition"))$plot

genderRTAnova <- GetGenderAnova_Gender(gender)$anova
genderRTEmotionPlot <- GetGenderAnova_Gender(gender, c("Emotion"))$plot
genderRTIntensityPlot <- GetGenderAnova_Gender(gender, c("Intensity"))$plot
genderRTGroupSizePlot <- GetGenderAnova_Gender(gender, c("GroupSize"))$plot
genderRTGenderPlot <- GetGenderAnova_Gender(gender, c("Gender"))$plot


########### RT - Criterion Correlations #################

GetRTCriterionCorrelation <- function(df, experiment) {

  corrData <- df %>%
    dplyr::filter(Experiment == experiment) %>% 
    dplyr::group_by(ParticipantNumber) %>% 
    dplyr::summarise(
      Criterion = mean(Criterion), 
      ResponseTime = mean(meanReponseTime)
    ) 
  
  rSquared <- summary(lm(Criterion ~ ResponseTime, data = corrData))$r.squared
  
  plot <- ggplot2::ggplot(data = corrData, aes(x = Criterion, y = ResponseTime)) + 
    ggplot2::geom_point() + 
    ggplot2::geom_smooth(method = "lm") + 
    ggplot2::ylim(0, 2) + 
    ggplot2::ggtitle(sprintf("%s RT & Criterion Correlation", experiment)) + 
    ggplot2::labs(subtitle = sprintf("R Squared: %f", rSquared))
 
  outList <- list(
    rSquared = rSquared, 
    plot = plot
  )
  
  return(outList)
}

crowdVsSingleRTCriterionCorrelation <- GetRTCriterionCorrelation(
  allSupplemental,
  "crowdVsSingle"
)$rSquared
crowdVsSingleRTCriterionCorrelation_Plot <- GetRTCriterionCorrelation(
  allSupplemental,
  "crowdVsSingle"
)$plot

fearRTCriterionCorrelation <- GetRTCriterionCorrelation(
  allSupplemental,
  "fear"
)$rSquared
fearRTCriterionCorrelation_Plot <- GetRTCriterionCorrelation(
  allSupplemental,
  "fear"
)$plot

noiseRTCriterionCorrelation <- GetRTCriterionCorrelation(
  allSupplemental,
  "noise"
)$rSquared
noiseRTCriterionCorrelation_Plot <- GetRTCriterionCorrelation(
  allSupplemental,
  "noise"
)$plot

groupSizeRTCriterionCorrelation <- GetRTCriterionCorrelation(
  allSupplemental,
  "group_size_intensity_collapsed"
)$rSquared

groupSizeRTCriterionCorrelation_Plot <- GetRTCriterionCorrelation(
  allSupplemental,
  "group_size_intensity_collapsed"
)$plot

identityRTCriterionCorrelation <- GetRTCriterionCorrelation(
  allSupplemental,
  "identity"
)$rSquared
identityRTCriterionCorrelation_Plot <- GetRTCriterionCorrelation(
  allSupplemental,
  "identity"
)$plot

genderRTCriterionCorrelation <- GetRTCriterionCorrelation(
  allSupplemental,
  "gender"
)$rSquared
genderRTCriterionCorrelation_Plot <- GetRTCriterionCorrelation(
  allSupplemental,
  "gender"
)$plot

########### Write all supplementals to csv #################
# write.csv(
#   allSupplemental,
#   "supplementals/investigation_1_data_supplemental.csv",
#   row.names = FALSE
# )
# write.csv(
#   crowdVsSingle_repair,
#   "supplementals/experiment_raw_data/crowd_vs_single_raw.csv",
#   row.names = FALSE
# )
# write.csv(
#   fear,
#   "supplementals/experiment_raw_data/fear_raw.csv",
#   row.names = FALSE
# )
# write.csv(
#   noise,
#   "supplementals/experiment_raw_data/noise_raw.csv",
#   row.names = FALSE
# )
# write.csv(
#   size,
#   "supplementals/experiment_raw_data/size_raw.csv",
#   row.names = FALSE
# )
# write.csv(
#   identity,
#   "supplementals/experiment_raw_data/identity_raw.csv",
#   row.names = FALSE
# )
# write.csv(
#   gender_repair,
#   "supplementals/experiment_raw_data/gender_raw.csv",
#   row.names = FALSE
# )
# write.csv(
#   confidenceIntervalCrowdVsSingle,
#   "supplementals/plot_info/crowd_vs_single_plot_info.csv",
#   row.names = FALSE
# )
# write.csv(
#   confidenceIntervalFear,
#   "supplementals/plot_info/fear_plot_info.csv",
#   row.names = FALSE
# )
# write.csv(
#   confidenceIntervalNoise,
#   "supplementals/plot_info/noise_plot_info.csv",
#   row.names = FALSE
# )
# write.csv(
#   confidenceIntervalGroupSizeIntensityCollapsed,
#   "supplementals/plot_info/crowd_size_intensity_collapsed_plot_info.csv",
#   row.names = FALSE
# )
# write.csv(
#   confidenceIntervalGroupSizeGroupCollapsed,
#   "supplementals/plot_info/crowd_size_group_collapsed_plot_info.csv",
#   row.names = FALSE
# )
# 
# write.csv(
#   confidenceIntervalIdentity,
#   "supplementals/plot_info/identity_plot_info.csv",
#   row.names = FALSE
# )
# 
# write.csv(
#   confidenceIntervalGender,
#   "supplementals/plot_info/gender_plot_info.csv",
#   row.names = FALSE
# )























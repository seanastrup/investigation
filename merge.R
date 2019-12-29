suppressPackageStartupMessages({
  library(parallel)
  library(googledrive)
  library(readxl)
  library(tidyverse)
  library(ggthemes)
  library(data.table)
})

numCores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(numCores, outfile = "")
parallel::clusterEvalQ(cl, {library(readxl)})

invs <- c("fear", "crowd_vs_single", "gender", "identity", "noise", "size")

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

DownloadFiles <- function(investigation) {
  library(googledrive)
  setwd(sprintf("~/Projects/merge/data/%s", investigation))
  
  targetFiles <- 
    googledrive::drive_ls(
      path = sprintf("anger_bias_investigation_1/ab_%s/analyzed", investigation)
    )
  
  targetFileIds <- unique(targetFiles$id)
  
  # too fast? rate limits exceed sometimes  
  # parallel::parLapply(cl, targetFileIds, function(fileId){
  #   googledrive::drive_download(googledrive::as_id(fileId), overwrite = TRUE)
  # })
  
  lapply(targetFileIds, function(fileId){
    googledrive::drive_download(googledrive::as_id(fileId), overwrite = TRUE)
  })
  
}

DownloadZackThings <- function() {
  library(googledrive)
  setwd("~/Projects/merge/data/zacks")
  
  targetFiles <- googledrive::drive_ls(
    path = googledrive::as_id("1dmhy0c1X8Mv681VR9tyj4cpql1foRx9x")
  )
  
  targetFileIds <- unique(targetFiles$id)
    
  # too fast? rate limits exceed sometimes  
  # parallel::parLapply(cl, targetFileIds, function(fileId){
  #   googledrive::drive_download(googledrive::as_id(fileId), overwrite = TRUE)
  # })
  
  lapply(targetFileIds, function(fileId){
    googledrive::drive_download(googledrive::as_id(fileId), overwrite = TRUE)
  })
}

PopulateData <- function() {
  lapply(invs, function(investigation) {
    DownloadFiles(investigation)
  })
}

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
  
  return(mergedFiles)
}

# uncomment to redownload dadta from gDrive
# PopulateData()
# DownloadZackThings()

######################################
# Get datasets in order for analysies below

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

# Get Zack's compiled data
GetFearCompiled <- function() {
  fearConditions <- CONTROLS$fear$conditions
  fearNames <- c("ParticipantNumber", fearConditions)
  # this is gross, don't do this function weird in-pipe crap
  compiledFear <- 
    readxl::read_excel(path = "data/zacks/CompilationFear.xlsx") %>%
    dplyr::select(CONTROLS$fear$zackCompiledFields) %>% 
    na.omit() %>% 
    dplyr::filter(!dplyr::row_number() == 1)
  names(compiledFear) <- fearNames
  compiledFearGathered <- tidyr::gather(
    compiledFear, 
    Condition, Criterion, 
    fearConditions
  )
  compiledFearGathered$ParticipantNumber <- as.numeric(compiledFearGathered$ParticipantNumber)
  compiledFearGathered$Criterion <- as.numeric(compiledFearGathered$Criterion)
  
  return(compiledFearGathered)
}

GetNoiseCompiled <- function() {
  noiseConditions <- CONTROLS$noise$conditions
  noiseNames <- c("ParticipantNumber", noiseConditions)
  # this is gross, don't do this function weird in-pipe crap
  compiledNoise <- 
    readxl::read_excel(path = "data/zacks/CompilationNoise.xlsx") %>%
    dplyr::select(CONTROLS$noise$zackCompiledFields) %>% 
    na.omit() %>% 
    dplyr::filter(!dplyr::row_number() == 1)
  names(compiledNoise) <- noiseNames
  compiledNoiseGathered <- tidyr::gather(
    compiledNoise, 
    Condition, Criterion, 
    noiseConditions
  )
  compiledNoiseGathered$ParticipantNumber <- as.numeric(compiledNoiseGathered$ParticipantNumber)
  compiledNoiseGathered$Criterion <- as.numeric(compiledNoiseGathered$Criterion)
  
  return(compiledNoiseGathered)
}

# some individuals have NA/blank values to to user/machine error in collection
# remove all rows that are not complete
fear <-  fear[complete.cases(fear), ]
gender <-  gender[complete.cases(gender), ]
identity <-  identity[complete.cases(identity), ]
noise <-  noise[complete.cases(noise), ]
size <-  size[complete.cases(size), ]

crowdVsSingle$RT <- as.numeric(crowdVsSingle$RT)
crowdVsSingle <-  crowdVsSingle[complete.cases(crowdVsSingle), ]

compiledFearGathered <- GetFearCompiled()
compiledNoiseGathered <- GetNoiseCompiled()


GetFearAnova <- function(fearDF, outlierByParticipant = FALSE) {
  if (outlierByParticipant) {
    fearRT<- fearDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))  
  } else {
    fearRT<- fearDF %>%
      dplyr::group_by(ParticipantNumber, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarise(meanRT = mean(ResponseTime))  
  }
  
  anova <- 
    summary(
      aov(
        meanRT ~ Emotion * Intensity * GroupSize + Error(
          ParticipantNumber / Emotion * Intensity * GroupSize
        ), 
        data = fearRT
      )
    )
  
  return(anova)
}

GetGenderAnova <- function(genderDF, outlierByParticipant = FALSE) {
  if (outlierByParticipant) {
    genderRT <- genderDF %>%
      dplyr::group_by(ParticipantNumber) %>%
        dplyr::filter(
          !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(ParticipantNumber, Gender, GroupSize, Emotion, Intensity) %>%
        dplyr::summarize(meanRT = mean(ResponseTime))
  } else {
    genderRT <- genderDF %>%
      dplyr::group_by(ParticipantNumber, Gender, GroupSize, Emotion, Intensity) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))  
  }
  
  anova <-
    summary(
      aov(
        meanRT ~ Gender * Emotion * Intensity * GroupSize + Error(
          ParticipantNumber / Gender *Emotion * Intensity * GroupSize
        ), 
        data = genderRT
      )
    )  
  
  return(anova)
}
  
GetIdentityAnova <- function(identityDF, outlierByParticipant = FALSE) {
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
  } else {
    identityRT <- identityDF %>%
      dplyr::group_by(
        ParticipantNumber, Condition, Emotion, Intensity
      ) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))
  }
  
  anova <- 
    summary(
      aov(
        meanRT ~  Emotion * Condition * Intensity + Error(
          ParticipantNumber / Emotion * Condition * Intensity
        ), 
        data = identityRT
      )
    )  
  
  return(anova)
}

GetNoiseAnova <- function(noiseDF, outlierByParticipant = FALSE) {
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
  } else {
    noiseRT <- noiseDF %>%
      dplyr::group_by(
        ParticipantNumber, GroupSize, Emotion, Noise
      ) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))
  }
  
  anova <- 
    summary(
      aov(
        meanRT ~  GroupSize * Emotion * Noise + Error(
          ParticipantNumber / GroupSize * Emotion * Noise
        ), 
        data = noiseRT
      )
    )  

  return(anova)
}

GetSizeAnova <- function(sizeDF, outlierByParticipant = FALSE) {
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
  } else {
    sizeRT <- sizeDF %>%
      dplyr::group_by(
        ParticipantNumber, GroupSize, Emotion, Intensity
      ) %>%
      dplyr::filter(
        !(abs(ResponseTime - median(ResponseTime)) > 2 * sd(ResponseTime))
      ) %>%
      dplyr::summarize(meanRT = mean(ResponseTime))
  }
  
  anova <- 
    summary(
      aov(
        meanRT ~  GroupSize * Emotion * Intensity + Error(
          ParticipantNumber / GroupSize * Emotion * Intensity
        ), 
        data = sizeRT
      )
    )  
  
  return(anova)
}

GetCrowdVsSingleAnova <- function(crowdVsSingleDF, outlierByParticipant = FALSE) {
  if (outlierByParticipant) {
    crowdVsSingleRT <- crowdVsSingleDF %>%
      dplyr::group_by(ParticipantNumber) %>%
      dplyr::filter(
        !(abs(RT - median(RT)) > 2 * sd(RT))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(
        ParticipantNumber, Emotion, Group
      ) %>%
      dplyr::summarize(meanRT = mean(RT))  
  } else {
    crowdVsSingleRT <- crowdVsSingleDF %>%
      dplyr::group_by(
        ParticipantNumber, Emotion, Group
      ) %>%
      dplyr::filter(
        !(abs(RT - median(RT)) > 2 * sd(RT))
      ) %>%
      dplyr::summarize(meanRT = mean(RT))
  }
  
  anova <- 
    summary(
      aov(
        meanRT ~  Group * Emotion + Error(
          ParticipantNumber / Group * Emotion
        ), 
        data = crowdVsSingleRT
      )
    )  
  
  return(anova)
  
  
}

GetFearRTCorr <- function(fearDF, fearCompiledDF) {
  fearRT <- fearDF %>% 
    dplyr::mutate(Condition = paste0(GroupSize, ".", Intensity))%>%
    dplyr::group_by(ParticipantNumber, Condition) %>% 
    dplyr::summarise(MeanRT = mean(ResponseTime)) %>% 
    dplyr::select(ParticipantNumber, Condition, MeanRT)
  
  fearZackJoin <- dplyr::inner_join(fearRT, fearCompiledDF)
  
  plt <- 
    ggplot2::ggplot(data = fearZackJoin, aes(x = Criterion, y = MeanRT)) + 
      ggplot2::geom_point() + 
      ggplot2::geom_smooth(method = "lm") + 
      ggplot2::facet_wrap(~Condition) 
  
  t <- summary(lmList(Criterion ~ MeanRT | Condition, data = fearZackJoin))$adj.r.squared
  t <- data.frame(unlist(t)) 
  prettyT <- t %>% tibble::rownames_to_column("Condition")
  names(prettyT) <- c("Condition", "R2")
  
  out <- list(
    plot = plt,
    table = prettyT
  )
  return(out)
}

GetNoiseRTCorr <- function(noiseDF, noiseCompiledDF) {
  noiseRT <- noiseDF %>% 
    dplyr::mutate(Condition = paste0(GroupSize, ".", Noise)) %>%
    dplyr::group_by(ParticipantNumber, Condition) %>% 
    dplyr::summarise(MeanRT = mean(ResponseTime)) %>% 
    dplyr::select(ParticipantNumber, Condition, MeanRT)
  
  noiseZackJoin <- dplyr::inner_join(noiseRT, noiseCompiledDF)
  
  plt <- 
    ggplot2::ggplot(data = noiseZackJoin, aes(x = Criterion, y = MeanRT)) + 
    ggplot2::geom_point() + 
    ggplot2::geom_smooth(method = "lm") + 
    ggplot2::facet_wrap(~Condition) 
  
  t <- summary(lmList(Criterion ~ MeanRT | Condition, data = noiseZackJoin))$adj.r.squared
  t <- data.frame(unlist(t)) 
  prettyT <- t %>% tibble::rownames_to_column("Condition")
  names(prettyT) <- c("Condition", "R2")
  
  out <- list(
    plot = plt,
    table = prettyT
  )
  return(out)
}



GetFearRTCorr(fear, compiledFearGathered)
GetNoiseRTCorr(noise, compiledNoiseGathered)

######################################
# Get RT Anova results for each experiment
GetFearAnova(fear)
GetGenderAnova(gender)
GetIdentityAnova(identity)  
GetNoiseAnova(noise)
GetSizeAnova(size)
GetCrowdVsSingleAnova(crowdVsSingle)

######################################
# Investigate RT and magnitude of bias




######################################
# Remove anything extra from inline stuff
rm(list = setdiff(ls(), c(
  "fear", "gender", "identity", "noise", "size", "crowdVsSingle",      # datasets
  "compiledFearGathered", "compiledNoiseGathered",                     # datasets
  "numCores", "cl", "CONTROLS",                                        # parallel infra
  "DownloadFiles", "PopulateData", "MergeFiles","DownloadZackThings",  # functions
  "GetFearCompiled", "GetNoiseCompiled", "GetCrowdVsSingleAnova")))                             # functions

library(sjstats)
# should make functional, but /shrug

# Fear
#########################
fearAnovaData <- allSupplemental %>% 
  dplyr::filter(Experiment == "fear")
fearDprimeAnova <- summary(
  aov(
    dPrime ~ Intensity * GroupSize + Error(ParticipantNumber / (Intensity * GroupSize)),
    data = fearAnovaData
  )
)
fearDprimeEtaSq <- sjstats::anova_stats(
  aov(
    dPrime ~ Intensity * GroupSize + Error(ParticipantNumber / (Intensity * GroupSize)),
    data = fearAnovaData
  )
)
fearCriterionAnova <- summary(
  aov(
    Criterion ~ Intensity * GroupSize + Error(ParticipantNumber / (Intensity * GroupSize)),
    data = fearAnovaData
  )
)
fearCriterionEtaSq <- sjstats::anova_stats(
  aov(
    Criterion ~ Intensity * GroupSize + Error(ParticipantNumber / (Intensity * GroupSize)),
    data = fearAnovaData
  )
)

# Noise
#########################
noiseAnovaData <- allSupplemental %>% 
  dplyr::filter(Experiment == "noise")
noiseDprimeAnova <- summary(
  aov(
    dPrime ~ Noise * GroupSize + Error(ParticipantNumber / (Noise * GroupSize)),
    data = noiseAnovaData
  )
)
noiseCriterionAnova <- summary(
  aov(
    Criterion ~ Noise * GroupSize + Error(ParticipantNumber / (Noise * GroupSize)),
    data = noiseAnovaData
  )
)
noiseDprimeEtaSq <- sjstats::anova_stats(
  aov(
    dPrime ~ Noise * GroupSize + Error(ParticipantNumber / (Noise * GroupSize)),
    data = noiseAnovaData
  )
)
noiseCriterionEtaSq<-sjstats::anova_stats(
  aov(
    Criterion ~ Noise * GroupSize + Error(ParticipantNumber / (Noise * GroupSize)),
    data = noiseAnovaData
  )
)

# Group Size Intensity Collapsed
#########################
groupSizeIntensityCollapsedAnovaData <- allSupplemental %>% 
  dplyr::filter(Experiment == "group_size_intensity_collapsed")
groupSizeIntensityCollapsedDprimeAnova <- summary(
  aov(
    dPrime ~ GroupSize + Error(ParticipantNumber / GroupSize),
    data = groupSizeIntensityCollapsedAnovaData
  )
)
groupSizeIntensityCollapsedCriterionAnova <- summary(
  aov(
    Criterion ~ GroupSize + Error(ParticipantNumber / GroupSize),
    data = groupSizeIntensityCollapsedAnovaData
  )
)
groupSizeIntensityCollapsedDprimeEtaSq <- sjstats::anova_stats(
  aov(
    dPrime ~ GroupSize + Error(ParticipantNumber / GroupSize),
    data = groupSizeIntensityCollapsedAnovaData
  )
)
groupSizeIntensityCollapsedCriterionEtaSq <- sjstats::anova_stats(
  aov(
    Criterion ~ GroupSize + Error(ParticipantNumber / GroupSize),
    data = groupSizeIntensityCollapsedAnovaData
  )
)

# Group Size Group Collapsed
#########################
groupSizeIGroupCollapsedAnovaData <- allSupplemental %>% 
  dplyr::filter(Experiment == "group_size_group_collapsed")
groupSizeGroupCollapsedDprimeAnova <- summary(
  aov(
    dPrime ~ Intensity + Error(ParticipantNumber / Intensity),
    data = groupSizeIGroupCollapsedAnovaData
  )
)
groupSizeGroupCollapsedCriterionAnova <- summary(
  aov(
    Criterion ~ Intensity + Error(ParticipantNumber / Intensity),
    data = groupSizeIGroupCollapsedAnovaData
  )
)
groupSizeGroupCollapsedDprimeEtaSq <- sjstats::anova_stats(
  aov(
    dPrime ~ Intensity + Error(ParticipantNumber / Intensity),
    data = groupSizeIGroupCollapsedAnovaData
  )
)
groupSizeGroupCollapsedCriterionEtaSq <- sjstats::anova_stats(
  aov(
    Criterion ~ Intensity + Error(ParticipantNumber / Intensity),
    data = groupSizeIGroupCollapsedAnovaData
  )
)

# Identity
#########################
identityAnovaData <- allSupplemental %>% 
  dplyr::filter(Experiment == "identity")
identityDprimeAnova <- summary(
  aov(
    dPrime ~ Intensity * IdentityCondition + Error(ParticipantNumber / (Intensity * IdentityCondition)),
    data = identityAnovaData
  )
)
identityCriterionAnova <- summary(
  aov(
    Criterion ~ Intensity * IdentityCondition + Error(ParticipantNumber / (Intensity * IdentityCondition)),
    data = identityAnovaData
  )
)
identityDprimeEtaSq <- sjstats::anova_stats(
  aov(
    dPrime ~ Intensity * IdentityCondition + Error(ParticipantNumber / (Intensity * IdentityCondition)),
    data = identityAnovaData
  )
)
identityCriterionEtaSq <- sjstats::anova_stats(
  aov(
    Criterion ~ Intensity * IdentityCondition + Error(ParticipantNumber / (Intensity * IdentityCondition)),
    data = identityAnovaData
  )
)

# Gender
#########################
genderAnovaData <- allSupplemental %>% 
  dplyr::filter(Experiment == "gender_crowd_type")
genderDprimeAnova <- summary(
  aov(
    dPrime ~ IntensityType * GroupType + Error(ParticipantNumber / (IntensityType * GroupType)),
    data = genderAnovaData
  )
)
genderCriterionAnova <- summary(
  aov(
    Criterion ~ IntensityType * GroupType + Error(ParticipantNumber / (IntensityType * GroupType)),
    data = genderAnovaData
  )
)
genderDprimeEtaSq <- sjstats::anova_stats(
  aov(
    dPrime ~ IntensityType * GroupType + Error(ParticipantNumber / (IntensityType * GroupType)),
    data = genderAnovaData
  )
)
genderCriterionEtaSq <- sjstats::anova_stats(
  aov(
    Criterion ~ IntensityType * GroupType + Error(ParticipantNumber / (IntensityType * GroupType)),
    data = genderAnovaData
  )
)
# Gender Group Size
#########################
genderGroupSizeAnovaData <- allSupplemental %>% 
  dplyr::filter(
    Experiment == "gender", 
    Gender != 3
  )
genderGroupSizeDprimeAnova <- summary(
  aov(
    dPrime ~ Intensity * Gender * GroupSize + Error(ParticipantNumber / (Intensity * Gender * GroupSize)),
    data = genderGroupSizeAnovaData
  )
)
genderGroupSizeCriterionAnova <- summary(
  aov(
    Criterion ~ Intensity * Gender * GroupSize + Error(ParticipantNumber / (Intensity * Gender * GroupSize)),
    data = genderGroupSizeAnovaData
  )
)
genderGroupSizeDprimeEtaSq <- sjstats::anova_stats(
  aov(
    dPrime ~ Intensity * Gender * GroupSize + Error(ParticipantNumber / (Intensity * Gender * GroupSize)),
    data = genderGroupSizeAnovaData
  )
)
genderGroupSizeCriterionEtaSq <- sjstats::anova_stats(
  aov(
    Criterion ~ Intensity * Gender * GroupSize + Error(ParticipantNumber / (Intensity * Gender * GroupSize)),
    data = genderGroupSizeAnovaData
  )
)

# Crowd Vs Single
#########################
crowdSingleAnovaData <- allSupplemental %>%
  dplyr::filter(Experiment == "crowdVsSingle")
crowdSingleDprimeAnova <- summary(
  aov(
    dPrime ~ Intensity * GroupSize + Error(ParticipantNumber / (Intensity * GroupSize)),
    data = crowdSingleAnovaData
  )
)
crowdSingleCriterionAnova <- summary(
  aov(
    Criterion ~ Intensity * GroupSize + Error(ParticipantNumber / (Intensity * GroupSize)),
    data = crowdSingleAnovaData
  )
)
crowdSingleDprimeEtaSq <- sjstats::anova_stats(
  aov(
    dPrime ~ Intensity * GroupSize + Error(ParticipantNumber / (Intensity * GroupSize)),
    data = crowdSingleAnovaData
  )
)
crowdSingleCriterionEtaSq <- sjstats::anova_stats(
  aov(
    Criterion ~ Intensity * GroupSize + Error(ParticipantNumber / (Intensity * GroupSize)),
    data = crowdSingleAnovaData
  )
)
















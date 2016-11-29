# Import data for transparency experiment
rm(list=ls())
library(xlsx)
wd <- "Z:/Projects/R Projects/transparency_submission" # specify working directory here
setwd(wd)
df <- read.xlsx(file = paste0(wd, "/0 results_raw.xlsx"), sheetIndex = 1, startRow = 3, header = TRUE, endRow = 217, colIndex = c(6:61))

# Create one Contribution variable (numeric)
df$Contribution <- ifelse(is.na(df$Control) == FALSE, df$Control,
                      ifelse(is.na(df$Nudge) == FALSE, df$Nudge, 
                             ifelse(is.na(df$Nudge.I) == FALSE, df$Nudge.I, 
                                    ifelse(is.na(df$Nudge.I.P) == FALSE, df$Nudge.I.P,
                                           ifelse(is.na(df$Nudge.P) == FALSE, df$Nudge.P, NA)))))
               
# Create one Treatment variable (factor)
df$Treatment <- ifelse(is.na(df$Control) == FALSE, 0,
                       ifelse(is.na(df$Nudge) == FALSE, 1, 
                              ifelse(is.na(df$Nudge.I) == FALSE, 2, 
                                     ifelse(is.na(df$Nudge.I.P) == FALSE, 3,
                                            ifelse(is.na(df$Nudge.P) == FALSE, 4, NA)))))
df$Treatment <- factor(df$Treatment, levels = c(0,1,2,4,3), 
                       labels = c("Control", "Default", "Default\n+Info", "Default\n+Purpose", "Default\n+Info\n+Purpose"))
## delete "old" variables
df <- df[,-c(3:7)]
df <- df[,c(1:2, 53, 52, 3:51)]

# Create Reactance score
df$RegulationD <- ifelse(df$Regulation > 3, 1, 0)
df$ContradictD <- ifelse(df$Contradict > 3, 1, 0)
df$ProhibitD <- ifelse(df$Prohibit > 3, 1, 0)
df$DependentD <- ifelse(df$Dependent > 3, 1, 0)
df$AdviceD <- ifelse(df$Advice > 3, 1, 0)
df$FrustratedD <- ifelse(df$Frustrated > 3, 1, 0)
df$IrritatedObviousD <- ifelse(df$IrritatedObvious > 3, 1, 0)
df$RestrictionD <- ifelse(df$Restriction > 3, 1, 0)
df$RecommendationD <- ifelse(df$Recommendation > 3, 1, 0)
df$FreewillD <- ifelse(df$Freewill > 3, 1, 0)
df$InfluenceMeD <- ifelse(df$InfluenceMe > 3, 1, 0)
df$RolemodelD <- ifelse(df$Rolemodel > 3, 1, 0)
df$ForceD <- ifelse(df$Force > 3, 1, 0)
df$StandardrulesD <- ifelse(df$Standardrules > 3, 1, 0)

df$Reactance <- df$RegulationD + df$ContradictD + df$ProhibitD + df$DependentD + df$AdviceD + df$FrustratedD +
  df$IrritatedObviousD + df$RestrictionD + df$RecommendationD + df$FreewillD + df$InfluenceMeD + df$RolemodelD +
  df$ForceD + df$StandardrulesD
df$Reactance2 <- df$Regulation + df$Contradict + df$Prohibit + df$Dependent + df$Advice + df$Frustrated +
  df$IrritatedObvious + df$Restriction + df$Recommendation + df$Freewill + df$InfluenceMe + df$Rolemodel +
  df$Force + df$Standardrules

# 4 reactance-categories
df$EmoRes <- df$DependentD+df$FrustratedD+df$IrritatedObviousD+df$RestrictionD
df$ReaCom <- df$RegulationD+df$ContradictD+df$ProhibitD+df$StandardrulesD
df$ResInf <- df$FreewillD+df$InfluenceMeD+df$RolemodelD+df$ForceD
df$ReaAdv <- df$AdviceD+df$RecommendationD


### set all observations of Reactance in Control group in Sessions 1-3 to NA, because it was not asked there
df$Reactance <- ifelse(df$Reactance == 0 & df$Treatment == "Control" & df$Session <= 3, NA, df$Reactance)


## center Reacance
df$ReactanceM <- df$Reactance - mean(df$Reactance, na.rm = T)

### create dummy for high or low average trait reactance
df$AboveTraitReact <- ifelse(df$ReactanceM < 0, 0, 1)
df$AboveTraitReact <- factor(df$AboveTraitReact, levels = c(0,1), labels = c("Below", "Above"))

### Create 11 item reactance scale in order to be able to compare it to my experiment
df$Reactance11 <- df$FrustratedD + df$RolemodelD + df$IrritatedObviousD + df$AdviceD + df$ContradictD +
  df$InfluenceMeD + df$ProhibitD + df$ForceD + df$RegulationD + df$RestrictionD + df$RecommendationD
# center
df$Reactance11M <- df$Reactance11 - mean(df$Reactance11, na.rm = T)

# Create one Contributed variable (dummy)#
df$Contributed <- ifelse(df$Contribution > 0, 1,0)

## compare Treatments without control (for state reactance)
df$TreatmentnoC <- ifelse(df$Treatment == "Control", NA, df$Treatment)
df$TreatmentnoC <- factor(df$TreatmentnoC, levels = c(2,3,4,5), labels = c("Default", "Default+Info", "Default+Purpose", "Default+Info+Purpose"))

# Create Distance variable 
df$Dist <- df$Contribution - 8
df$Dist <- ifelse(df$Dist < 0, df$Dist * (-1), df$Dist)
df$Distno8 <- ifelse(df$Dist == 8, NA, df$Dist)

df$Intervention <- ifelse(df$Treatment == "Control", 0, 1)
df$Intervention <- factor(df$Intervention, levels = c(0,1), labels = c("No Default", "Default"))
df$SomeInfo <- ifelse(df$Treatment == "Control" | df$Treatment == "Default", 0, 1)
df$SomeInfo <- factor(df$SomeInfo, levels = c(0,1), labels = c("No Info", "SomeInfo"))

# Create binary variables for statistical tests of pariwise Treatment comparisons
df$ConvsDef <- ifelse(df$Treatment == "Control", 0, ifelse(df$Treatment == "Default", 1, NA))
df$ConvsDef <- factor(df$ConvsDef, levels = c(0,1), labels = c("Control", "Default"))
df$DefvsInf <- ifelse(df$Treatment == "Default", 0, ifelse(df$Treatment == "DefaultInfo", 1, NA))
df$DefvsInf <- factor(df$DefvsInf, levels = c(0,1), labels = c("Default", "DefaultInfo"))
df$ConvsPur <- ifelse(df$Treatment == "Control", 0, ifelse(df$Treatment == "DefaultPurpose", 1, NA))
df$ConvsPur <- factor(df$ConvsPur, levels = c(0,1), labels = c("Control", "DefaultPurpose"))
df$PurvsDef <- ifelse(df$Treatment == "DefaultPurpose", 0, ifelse(df$Treatment == "Default", 1, NA))
df$PurvsDef <- factor(df$PurvsDef, levels = c(0,1), labels = c("DefaultPurpose", "Default"))

df$PurvsInfPurvsInf <- ifelse(df$Treatment == "DefaultPurpose", 0, ifelse(df$Treatment == "DefaultInfoPurpose", 1, ifelse(df$Treatment == "DefaultInfo", 2, NA)))
df$PurvsInfPurvsInf <- factor(df$PurvsInfPurvsInf, levels = c(0,1,2), labels = c("DefaultPurpose", "DefaultInfoPurpose", "DefaultInfo"))
df$PurvsInfPur <- ifelse(df$Treatment == "DefaultPurpose", 0, ifelse(df$Treatment == "DefaultInfoPurpose", 1,NA))
df$PurvsInfPur <- factor(df$PurvsInfPur, levels = c(0,1), labels = c("DefaultPurpose", "DefaultInfoPurpose"))
df$InfPurvsInf <- ifelse(df$Treatment == "DefaultInfoPurpose", 0, ifelse(df$Treatment == "DefaultInfo", 1, NA))
df$InfPurvsInf <- factor(df$InfPurvsInf, levels = c(0,1), labels = c("DefaultInfoPurpose", "DefaultInfo"))

df$Influence <- ifelse(df$Treatment == "Control", NA, df$Influence)
df$Influence <- factor(df$Influence, levels = c(0,1), labels = c("No", "Yes"))


df$Contributionno0 <- ifelse(df$Contribution == 0, NA, df$Contribution)


# State reactance INDEX
## SubIndex "perceived threat to freedom"
df$Threat <- ifelse(df$Treatment == "Control", NA, df$Threat)
df$ThreatD <- ifelse(df$Threat > 3,1,0)
df$Decide <- ifelse(df$Treatment == "Control", NA, df$Decide)
df$DecideD <- ifelse(df$Decide > 3,1,0)
df$Manipulate <- ifelse(df$Treatment == "Control", NA, df$Manipulate)
df$ManipulateD <- ifelse(df$Manipulate > 3,1,0)
df$Pressure <- ifelse(df$Treatment == "Control", NA, df$Pressure)
df$PressureD <- ifelse(df$Pressure > 3,1,0)
df$ThreatToFreedom <- (df$ThreatD + df$DecideD + df$ManipulateD + df$PressureD)
df$ThreatToFreedom2 <- (df$Threat + df$Decide + df$Manipulate + df$Pressure)

## SubIndex "Anger"
df$Irritated <- ifelse(df$Treatment == "Control", NA, df$Irritated)
df$IrritatedD <- ifelse(df$Irritated > 3,1,0)
df$angry <- ifelse(df$Treatment == "Control", NA, df$angry)
df$angryD <- ifelse(df$angry > 3,1,0)
df$annoyed <- ifelse(df$Treatment == "Control", NA, df$annoyed)
df$annoyedD <- ifelse(df$annoyed > 3,1,0)
df$aggravated <- ifelse(df$Treatment == "Control", NA, df$aggravated)
df$aggravatedD <- ifelse(df$aggravated > 3,1,0)
df$Anger <- (df$IrritatedD + df$angryD + df$annoyedD + df$aggravatedD)
df$Anger2 <- (df$Irritated + df$angry + df$annoyed + df$aggravated)

## SubIndex "attitudes toward the message advocacy" 
df$Donation1 <- ifelse(df$Treatment == "Control", NA, df$Donation1)
df$Donation2 <- ifelse(df$Treatment == "Control", NA, df$Donation2)
df$Donation3 <- ifelse(df$Treatment == "Control", NA, df$Donation3)
df$Donation4 <- ifelse(df$Treatment == "Control", NA, df$Donation4)
df$Donation5 <- ifelse(df$Treatment == "Control", NA, df$Donation5)
df$Donation6 <- ifelse(df$Treatment == "Control", NA, df$Donation6)
df$Donation7 <- ifelse(df$Treatment == "Control", NA, df$Donation7)

df$MessageAdvocacy <- (df$Donation1 + df$Donation2 + df$Donation3 + df$Donation4 + df$Donation5 + df$Donation6 + df$Donation7)

# Gender as factor
df$gender <- as.factor(ifelse(df$Gender == 1, "Male", "Female"))

# Importance of climate protection
df$Important <- ifelse(df$Importance == 4 | df$Importance == 5, 1, 0)
df$Important <- factor(df$Important, levels = c(0,1), labels = c("Unimportant", "Important"))

# Effectiveness of EU ETS
df$EUETSuseful <- ifelse(df$EU_ETS_Usefulness == 2, 0, 1)
df$EUETSuseful <- factor(df$EUETSuseful, levels = c(0,1), labels = c("Not effective", "Effective" ))

# Manipulate feeling factor
df$ManipulateD <- factor(df$ManipulateD, levels = c(0,1), labels = c("No FM", "FM"))


## Used default to include the value, so including control and default
df$default.value <- ifelse(df$Contribution == 8, 1, 0)

## coding of study area as factor
df$StudyAreaF <- as.factor(ifelse(df$StudyArea == 1, "Economics", 
                        ifelse(df$StudyArea == 2, "Law", 
                               ifelse(df$StudyArea == 3, "Social Science",
                                      ifelse(df$StudyArea == 4, "Management",
                                             ifelse(df$StudyArea == 5, "Philosophy",
                                                    ifelse(df$StudyArea == 6, "History",
                                                           "Other")))))))
df$Gender <- factor(df$Gender, levels = c(0,1), labels = c("Female", "Male"))

df$PastParticipation <- factor(df$PastParticipation, levels = c(0,1), labels = c("Participated", "Not Participated"))

df$ReactanceMean <- rowMeans(df[,c(39:46, 48:53)])


df$EmoRes <- df$DependentD+df$FrustratedD+df$IrritatedObviousD+df$RestrictionD
df$ReaCom <- df$RegulationD+df$ContradictD+df$ProhibitD+df$StandardrulesD
df$ResInf <- df$FreewillD+df$InfluenceMeD+df$RolemodelD+df$ForceD
df$ReaAdv <- df$AdviceD+df$RecommendationD

dfsub <- df[df$Treatment != "Control",]


write.xlsx(x = df, paste0(wd, "/2 results_cleaned.xlsx"))

save.image(paste0(wd, "/2 results_cleaned.RData"))

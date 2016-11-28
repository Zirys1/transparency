# Data Import
rm(list=ls())
Sys.setenv(LANG = "en")
wd <- "Z:/Projects/R Projects/transparency_submission" # specify working directory here
setwd(wd)
load("2 results_cleaned.RData")

library(psych)
library(dunn.test)
library(ggplot2)
# Data Analysis

# Manuscript ----

# Experimental Design ----
mean(df$Age)
table(df$Gender)
table(df$StudyAreaF)

# 4.1 Default effects ----
# Results ----
sum(df$Contribution)
mean(df$Contribution)
mean(df$Dist)
table(df$Contributed)
table(df$Used.default)

# Table 2 ----
describeBy(df$Contribution, df$Treatment)
describeBy(df$Dist, df$Treatment)
table(df$Contributed, df$Treatment)
table(df$Used.default, df$Treatment)

# Figure 1 ----
ggplot(data = df, aes(x = Treatment, y = Contribution)) +
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8,9,10)) +
  labs(x = "Experimental group", y='Contribution [€]') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 26),
        axis.title.y = element_text(size = 26),
        axis.text = element_text(vjust=.5, size=16))

# Shapiro Wilk normality test ----
shapiro.test(df$Contribution)
shapiro.test(df$Dist)

# Tests of H1 (MWU) ----
wilcox.test(df$Contribution[df$Treatment == "Control" | df$Treatment == "Default"] ~ df$Treatment[df$Treatment == "Control" | df$Treatment == "Default"])
wilcox.test(df$Contribution[df$Treatment == "Control" | df$Treatment == "Default+Info"] ~ df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info"])
wilcox.test(df$Contribution[df$Treatment == "Control" | df$Treatment == "Default+Purpose"] ~ df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Purpose"])
wilcox.test(df$Contribution[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"] ~ df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"])

# Pairwise tests ----
# Table B.7 ----
pairwise.wilcox.test(df$Contribution, df$Treatment, p.adjust.method = "none", exact = FALSE) # Control sigdiff from all
# Table B.8 ----
pairwise.wilcox.test(df$Dist, df$Treatment, p.adjust.method = "none", exact = FALSE)
# Table B.9 ----
chisq.test(df$Contributed[df$Treatment == "Control" | df$Treatment == "Default"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default"])
chisq.test(df$Contributed[df$Treatment == "Control" | df$Treatment == "Default+Info"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info"])
chisq.test(df$Contributed[df$Treatment == "Control" | df$Treatment == "Default+Purpose"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Purpose"])
chisq.test(df$Contributed[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"])
chisq.test(df$Contributed[df$Treatment == "Default" | df$Treatment == "Default+Info"], df$Treatment[df$Treatment == "Default" | df$Treatment == "Default+Info"])
chisq.test(df$Contributed[df$Treatment == "Default" | df$Treatment == "Default+Purpose"], df$Treatment[df$Treatment == "Default" | df$Treatment == "Default+Purpose"])
chisq.test(df$Contributed[df$Treatment == "Default" | df$Treatment == "Default+Info+Purpose"], df$Treatment[df$Treatment == "Default" | df$Treatment == "Default+Info+Purpose"])
chisq.test(df$Contributed[df$Treatment == "Default+Info" | df$Treatment == "Default+Purpose"], df$Treatment[df$Treatment == "Default+Info" | df$Treatment == "Default+Purpose"])
chisq.test(df$Contributed[df$Treatment == "Default+Info" | df$Treatment == "Default+Info+Purpose"], df$Treatment[df$Treatment == "Default+Info" | df$Treatment == "Default+Info+Purpose"])
chisq.test(df$Contributed[df$Treatment == "Default+Purpose" | df$Treatment == "Default+Info+Purpose"], df$Treatment[df$Treatment == "Default+Purpose" | df$Treatment == "Default+Info+Purpose"])

# Table B.10 ----
fisher.test(df$default.value[df$Treatment == "Control" | df$Treatment == "Default"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default"])
fisher.test(df$default.value[df$Treatment == "Control" | df$Treatment == "Default+Info"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info"])
fisher.test(df$default.value[df$Treatment == "Control" | df$Treatment == "Default+Purpose"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Purpose"])
fisher.test(df$default.value[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"])
fisher.test(df$default.value[df$Treatment == "Default" | df$Treatment == "Default+Info"], df$Treatment[df$Treatment == "Default" | df$Treatment == "Default+Info"])
fisher.test(df$default.value[df$Treatment == "Default" | df$Treatment == "Default+Purpose"], df$Treatment[df$Treatment == "Default" | df$Treatment == "Default+Purpose"])
fisher.test(df$default.value[df$Treatment == "Default" | df$Treatment == "Default+Info+Purpose"], df$Treatment[df$Treatment == "Default" | df$Treatment == "Default+Info+Purpose"])
fisher.test(df$default.value[df$Treatment == "Default+Info" | df$Treatment == "Default+Purpose"], df$Treatment[df$Treatment == "Default+Info" | df$Treatment == "Default+Purpose"])
fisher.test(df$default.value[df$Treatment == "Default+Info" | df$Treatment == "Default+Info+Purpose"], df$Treatment[df$Treatment == "Default+Info" | df$Treatment == "Default+Info+Purpose"])
fisher.test(df$default.value[df$Treatment == "Default+Purpose" | df$Treatment == "Default+Info+Purpose"], df$Treatment[df$Treatment == "Default+Purpose" | df$Treatment == "Default+Info+Purpose"])

# Table 3 ----
describeBy(df$Age, df$Treatment)
table(df$Gender, df$Treatment)
table(df$Important, df$Treatment)
table(df$PastParticipation, df$Treatment)
table(df$EUETSuseful, df$Treatment)

# Chi²-Tests ----
chisq.test(df$Important, df$Treatment)
chisq.test(df$Important[df$Treatment == "Control" | df$Treatment == "Default"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default"])
chisq.test(df$Important[df$Treatment == "Control" | df$Treatment == "Default+Info"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info"])
chisq.test(df$Important[df$Treatment == "Control" | df$Treatment == "Default+Purpose"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Purpose"])
chisq.test(df$Important[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"], df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"])

# Regression results are in the Stata Do-File ----

# 4.2 Influence of transparency on default effectiveness ----
kruskal.test(df$Contribution, df$TreatmentnoC) # excluding the Control group
dunn.test(df$Contribution, df$TreatmentnoC)
kruskal.test(df$Dist, df$TreatmentnoC) # excluding the Control group
dunn.test(df$Dist, df$TreatmentnoC)
chisq.test(df$Contributed, df$TreatmentnoC)
fisher.test(df$default.value, df$TreatmentnoC)

# Figure B.4
ggplot(data = df, aes(x = Contribution, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .5) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35), limits = c(0, .37)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8,9,10), limits = c(-0.5, 10.5))+
  geom_vline(xintercept = 8, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))



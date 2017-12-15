# Data Import
rm(list=ls())
Sys.setenv(LANG = "en")
wd <- "Z:/Projects/R Projects/transparency_submission" # specify working directory here
setwd(wd)
load("2 results_cleaned.RData")

library(psych)
library(dunn.test)
library(ggplot2) # for visualization
library(dplyr) # for data manipulation
library(tikzDevice)
# Data Analysis

# Manuscript ----

# Experimental Design ----
nrow(df)
mean(df$Age)
median(df$Age)
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
detach("package:plyr")
tab2 <- df %>%
  group_by(Treatment) %>% 
  summarise(
    ContributionM = round(mean(Contribution), 2),
    ContributionSD = round(sd(Contribution), 2),
    DistanceM = round(mean(Dist), 2),
    DistanceSD = round(sd(Dist), 2),
    ContributedM = round(length(Pariticipant[Contributed == 1])/length(Pariticipant), 4)*100,
    PickedDefM = round(length(Pariticipant[default.value == 1])/length(Pariticipant), 4)*100,
    n = length(Pariticipant)
    )

write.xlsx(as.data.frame(tab2), file = paste0(wd, "/Table2.xlsx")) # as excel file


# Figure 1 ----
# below function calculates necessary components of bar plots
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
common_ops <-  list(theme(plot.title = element_text(size=8),
                          axis.text = element_text(size = 8),
                          axis.title = element_text(size = 8),
                          strip.text = element_text(size = 8),
                          legend.position="bottom",
                          #   panel.grid.major=element_blank(),
                          #   panel.grid.minor=element_blank(),
                          panel.border=element_blank(),
                          axis.line=element_line(),
                          text=element_text()))

levels(df$Treatment) <- c("Control", "Default", "Default\n+Info", "Default\n+Purpose", "Default\n+Info\n+Purpose") 
bar <- summarySE(df, measurevar = "Contribution", groupvars = "Treatment")

ggplot(data = bar, aes(x = Treatment, y = Contribution)) +
  geom_bar(fill = "grey40", stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = (Contribution - ci), ymax = (Contribution + ci)), width = 0.1) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(0,5)) +
  labs(x = "Experimental group", y = "Contribution [EUR]") +
  theme_bw() +
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

# Tests of H1 (MWU) ----
levels(df$Treatment) <- c("Control", "Default", "Default+Info", "Default+Purpose", "Default+Info+Purpose") 
wilcox.test(df$Contribution[df$Treatment == "Control" | df$Treatment == "Default"] ~ df$Treatment[df$Treatment == "Control" | df$Treatment == "Default"])
wilcox.test(df$Contribution[df$Treatment == "Control" | df$Treatment == "Default+Info"] ~ df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info"])
wilcox.test(df$Contribution[df$Treatment == "Control" | df$Treatment == "Default+Purpose"] ~ df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Purpose"])
wilcox.test(df$Contribution[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"] ~ df$Treatment[df$Treatment == "Control" | df$Treatment == "Default+Info+Purpose"])

# Table 3 ----
detach("package:plyr")
tab3 <- df %>%
  group_by(Treatment) %>% 
  summarise(
    AgeM = round(mean(Age), 2),
    AgeSD = round(sd(Age), 2),
    GenderM = round(length(Pariticipant[Gender == "Male"])/length(Pariticipant), 4)*100,
    ImportantM = round(length(Pariticipant[Important == "Important"])/length(Pariticipant), 4)*100,
    PastParticipationM = round(length(Pariticipant[PastParticipation == "Not Participated"])/length(Pariticipant), 4)*100,
    EUETSusefulM = round(length(Pariticipant[EUETSuseful == "Not effective"])/length(Pariticipant), 4)*100,
  )

write.xlsx(as.data.frame(tab3), file = paste0(wd, "/Table3.xlsx")) # as excel file

describeBy(df$Age, df$Treatment)
table(df$Gender, df$Treatment)
table(df$Important, df$Treatment)
table(df$PastParticipation, df$Treatment)
table(df$EUETSuseful, df$Treatment)

# Chi²-Tests ----
chisq.test(df$Important, df$Treatment)

# Regression results are in the Stata Do-File ----

# 4.2 Influence of transparency on default effectiveness ----
kruskal.test(df$Contribution, df$TreatmentnoC) 
dunn.test(df$Contribution, df$TreatmentnoC)
kruskal.test(df$Dist, df$TreatmentnoC)
dunn.test(df$Dist, df$TreatmentnoC)
chisq.test(df$Contributed, df$TreatmentnoC)
fisher.test(df$default.value, df$TreatmentnoC)

# Figure B.4
x1 <- ggplot(data = df, aes(x = Contribution, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .5, fill = "grey40") +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35), limits = c(0, .37)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8,9,10), limits = c(-0.5, 10.5))+
  geom_vline(xintercept = 8, linetype = "dashed") +
  labs(x = "Contribution [EUR]", y='Fraction') +
  theme_bw()+
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#library(tikzDevice)
#tikz(file = "FigureB4.tex", width = 4.5, height = 3.1)
x1
#dev.off()


# Intensive margin in Conclusions and discussion part
bar <- df %>%
  group_by(Treatment) %>%
  summarise(
    n = sum(Contributed),
    meanCon = mean(Contribution),
    intMarg = mean(Contribution[Contributed == 1]),
    sdintMarg = sd(Contribution[Contributed == 1]),
    seintMarg = sd(Contribution[Contributed == 1])/sqrt(n),
    meintMarg = qt(1-0.05/2, df = n)*seintMarg
  )

ggplot(data = bar, aes(x = Treatment, y = intMarg)) +
  geom_bar(color = "black", stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = (intMarg - meintMarg), ymax = (intMarg + meintMarg)), width = 0.1) +
#  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(0,5)) +
  labs(x = "Experimental group", y = "Intensive margin [€]") +
  theme_bw()+
  common_ops +
  theme_bw(base_size = 26)

# Comparing subjects from Rotterdam and Hamburg ----
df %>%
  group_by(Treatment, HH) %>% 
  summarise(
    ContributionM = round(mean(Contribution), 2),
    ContributionSD = round(sd(Contribution), 2),
    DistanceM = round(mean(Dist), 2),
    DistanceSD = round(sd(Dist), 2),
    ContributedM = round(length(Pariticipant[Contributed == 1])/length(Pariticipant), 4)*100,
    PickedDefM = round(length(Pariticipant[default.value == 1])/length(Pariticipant), 4)*100,
    n = length(Pariticipant)
  )

wilcox.test(df$Contribution ~ df$HH) #not sig
wilcox.test(df$Contribution[df$Treatment == "Control"] ~ df$HH[df$Treatment == "Control"]) #not sig
wilcox.test(df$Contribution[df$Treatment == "Default"] ~ df$HH[df$Treatment == "Default"]) #not sig
wilcox.test(df$Contribution[df$Treatment == "Default+Info"] ~ df$HH[df$Treatment == "Default+Info"]) #sig (.09)


df$Location <- df$HH
df$Location <- factor(df$Location, labels = c("Rotterdam", "Hamburg"))
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

bar <- summarySE(df[df$Treatment != "Default+Purpose" & df$Treatment != "Default+Info+Purpose",], measurevar = "Contribution", groupvars = c("Treatment", "Location"))

ggplot(data = bar, aes(x = Treatment, y = Contribution, fill = Location)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge(width = .35)) +
  geom_errorbar(aes(ymin = (Contribution - ci), ymax = (Contribution + ci)), width = 0.1, position = position_dodge(width = .35)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(0,5)) +
  labs(x = "Experimental group", y = "Contribution [€]") +
  theme_bw()+
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = guide_legend(title.position="top"))


wilcox.test(df$Contribution[df$HH == "R" & (df$Treatment == "Control" | df$Treatment == "Default")] ~ df$Treatment[df$HH == "R" & (df$Treatment == "Control" | df$Treatment == "Default")])
wilcox.test(df$Contribution[df$HH == "HH" & (df$Treatment == "Control" | df$Treatment == "Default")] ~ df$Treatment[df$HH == "HH" & (df$Treatment == "Control" | df$Treatment == "Default")])
wilcox.test(df$Contribution[df$HH == "R" & (df$Treatment == "Control" | df$Treatment == "Default+Info")] ~ df$Treatment[df$HH == "R" & (df$Treatment == "Control" | df$Treatment == "Default+Info")])
wilcox.test(df$Contribution[df$HH == "HH" & (df$Treatment == "Control" | df$Treatment == "Default+Info")] ~ df$Treatment[df$HH == "HH" & (df$Treatment == "Control" | df$Treatment == "Default+Info")])
wilcox.test(df$Contribution[df$HH == "R" & (df$Treatment == "Default" | df$Treatment == "Default+Info")] ~ df$Treatment[df$HH == "R" & (df$Treatment == "Default" | df$Treatment == "Default+Info")])
wilcox.test(df$Contribution[df$HH == "HH" & (df$Treatment == "Default" | df$Treatment == "Default+Info")] ~ df$Treatment[df$HH == "HH" & (df$Treatment == "Default" | df$Treatment == "Default+Info")])


df %>%
  group_by(Treatment, HH) %>% 
  summarise(
    AgeM = round(mean(Age), 2),
    AgeSD = round(sd(Age), 2),
    GenderM = round(length(Pariticipant[Gender == "Male"])/length(Pariticipant), 4)*100,
    ImportantM = round(length(Pariticipant[Important == "Important"])/length(Pariticipant), 4)*100,
    PastParticipationM = round(length(Pariticipant[PastParticipation == "Not Participated"])/length(Pariticipant), 4)*100,
    EUETSusefulM = round(length(Pariticipant[EUETSuseful == "Not effective"])/length(Pariticipant), 4)*100,
  )

t.test(df$Age ~ df$HH) 
chisq.test(df$Gender, df$HH) 
chisq.test(df$StudyArea, df$HH) 

chisq.test(df$Important, df$HH)
chisq.test(df$PastParticipation, df$HH)
chisq.test(df$EUETSuseful, df$HH)


# randomization
chisq.test(df$Important, df$Treatment)
kruskal.test(df$Age ~ df$Treatment) 
chisq.test(df$Gender, df$Treatment) 
chisq.test(df$StudyArea, df$Treatment) 
chisq.test(df$PastParticipation, df$Treatment)
chisq.test(df$EUETSuseful, df$Treatment)

# Efect sizes
cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
  return(cd)
}

dr <- round(cohens_d(df$Contribution[df$Treatment == "Control" & df$Location == "Rotterdam"], df$Contribution[df$Treatment == "Default" & df$Location == "Rotterdam"]),2)
n1 <- length(df$Contribution[df$Treatment == "Control" & df$Location == "Rotterdam"])
n2 <- length(df$Contribution[df$Treatment == "Default" & df$Location == "Rotterdam"])
library(compute.es) # Calculate confidence intervals for effect sizes
t = dr*sqrt((n1*n2)/(n1+n2))
tes1 <- tes(t=t, n.1=n1, n.2=n2)

dh <- round(cohens_d(df$Contribution[df$Treatment == "Control" & df$Location == "Hamburg"], df$Contribution[df$Treatment == "Default" & df$Location == "Hamburg"]),2)
n1 <- length(df$Contribution[df$Treatment == "Control" & df$Location == "Hamburg"])
n2 <- length(df$Contribution[df$Treatment == "Default" & df$Location == "Hamburg"])
t = dh*sqrt((n1*n2)/(n1+n2))
tes2 <- tes(t=t, n.1=n1, n.2=n2)

dr1 <- round(cohens_d(df$Contribution[df$Treatment == "Control" & df$Location == "Rotterdam"], df$Contribution[df$Treatment == "Default+Info" & df$Location == "Rotterdam"]),2)
n1 <- length(df$Contribution[df$Treatment == "Control" & df$Location == "Rotterdam"])
n2 <- length(df$Contribution[df$Treatment == "Default+Info" & df$Location == "Rotterdam"])
t = dr1*sqrt((n1*n2)/(n1+n2))
tes3 <- tes(t=t, n.1=n1, n.2=n2)

dh1 <- round(cohens_d(df$Contribution[df$Treatment == "Control" & df$Location == "Hamburg"], df$Contribution[df$Treatment == "Default+Info" & df$Location == "Hamburg"]),2)
n1 <- length(df$Contribution[df$Treatment == "Control" & df$Location == "Hamburg"])
n2 <- length(df$Contribution[df$Treatment == "Default+Info" & df$Location == "Hamburg"])
t = dh1*sqrt((n1*n2)/(n1+n2))
tes4 <- tes(t=t, n.1=n1, n.2=n2)

dr2 <- round(cohens_d(df$Contribution[df$Treatment == "Default" & df$Location == "Rotterdam"], df$Contribution[df$Treatment == "Default+Info" & df$Location == "Rotterdam"]),2)
n1 <- length(df$Contribution[df$Treatment == "Default" & df$Location == "Rotterdam"])
n2 <- length(df$Contribution[df$Treatment == "Default+Info" & df$Location == "Rotterdam"])
t = dr2*sqrt((n1*n2)/(n1+n2))
tes5 <- tes(t=t, n.1=n1, n.2=n2)

dh2 <- round(cohens_d(df$Contribution[df$Treatment == "Default" & df$Location == "Hamburg"], df$Contribution[df$Treatment == "Default+Info" & df$Location == "Hamburg"]),2)
n1 <- length(df$Contribution[df$Treatment == "Default" & df$Location == "Hamburg"])
n2 <- length(df$Contribution[df$Treatment == "Default+Info" & df$Location == "Hamburg"])
t = dh2*sqrt((n1*n2)/(n1+n2))
tes6 <- tes(t=t, n.1=n1, n.2=n2)

d_df1 <- c(dr, dh, dr1, dh1, dr2, dh2)
Location <- c("Rotterdam", "Hamburg","Rotterdam", "Hamburg","Rotterdam", "Hamburg")
t <- c("Con vs. Def", "Con vs. Def", "Con vs. Def+Inf", "Con vs. Def+Inf", "Def vs. Def+Inf", "Def vs. Def+Inf")
ld <- c(tes1$l.d, tes2$l.d, tes3$l.d, tes4$l.d, tes5$l.d, tes6$l.d)
ud <- c(tes1$u.d, tes2$u.d, tes3$u.d, tes4$u.d, tes5$u.d, tes6$u.d)
d_df <- as.data.frame(cbind(d_df1, Location, t, ld, ud))
d_df$d_df1 <- as.numeric(as.character(d_df$d_df1))
d_df$ld <- as.numeric(as.character(d_df$ld))
d_df$ud <- as.numeric(as.character(d_df$ud))

ggplot(d_df, aes(x = t, y = d_df1, color = Location)) +
  geom_point(position = position_dodge(width = .2)) +
  geom_errorbar(aes(ymin = ld, ymax = ud, width = 0), position = position_dodge(width = .2)) +
  scale_y_continuous(breaks = c(seq(-.15,1,.1)), limits = c(-.2, 1.1)) +
  ylab("Cohen's d") +
  theme_bw()+
  common_ops +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_grey() +
  scale_color_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

# Tobit coefficients (manually)
tobit_df <- NULL
tobit_df$d_df1 <- c(2.362969, 1.412202, -1.298468, 1.814153, 1.064501, .4019515,1.659097, 1.669844, .0107466)
tobit_df$ld <- c(0.6038211, 0.0902231, -2.97337, .4914717, -0.7052376, -.5232933, .6007994, .6121485, -.803587)
tobit_df$ud <- c(4.122117, 2.734181, .3764345, 3.136835, 2.83424, 1.327196, 2.717395, 2.727539, .8250801)
tobit_df$Location <- c("Rotterdam      ", "Hamburg      ","Rotterdam      ", "Hamburg      ","Rotterdam      ", "Hamburg      ", "Aggregated      ", "Aggregated      ", "Aggregated      " )

tobit_df$t <- c("Con vs.\nDef", "Con vs.\nDef", "Def vs.\nDef+Inf", "Con vs.\nDef+Inf", "Con vs.\nDef+Inf", "Def vs.\nDef+Inf", "Con vs.\nDef", "Con vs.\nDef+Inf", "Def vs.\nDef+Inf")
tobit_df <- as.data.frame(tobit_df)

ggplot(tobit_df, aes(x = t, y = d_df1, color = Location)) +
  geom_point(position = position_dodge(width = .2)) +
  geom_errorbar(aes(ymin = ld, ymax = ud, width = 0), position = position_dodge(width = .2)) +
  scale_y_continuous(breaks = c(seq(-3,4, 1)), limits = c(-3.1,4.2)) +
  ylab("Tobit regression coefficient") +
  xlab("Experimenteal group") +
  theme_bw()+
  common_ops +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_grey(start = 0.4, end =0.8) +
  scale_color_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5, color = NA),
        legend.key.size = unit(1, 'lines')) +
  guides(color = guide_legend(nrow = 3,title.position="top"))



# Further analyses (not in manuscript) ----
# Figure B.4
x1 <- ggplot(data = df, aes(x = Contribution, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .5, fill = "grey40") +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35), limits = c(0, .37)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8,9,10), limits = c(-0.5, 10.5))+
  geom_vline(xintercept = 8, linetype = "dashed") +
  labs(x = "Contribution [EUR]", y='Fraction') +
  theme_bw()+
  common_ops +
  ggtitle("Aggregated") +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

x2 <- ggplot(data = df, aes(x = Contribution, y = ..count../sum(..count..), fill = Location)) +
  geom_histogram(binwidth = .5, position = position_dodge(width = .35)) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2), limits = c(0, .22)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8,9,10), limits = c(-0.5, 10.5))+
  geom_vline(xintercept = 8, linetype = "dashed") +
  labs(x = "Contribution [EUR]", y='Fraction') +
  theme_bw()+
  common_ops +
  ggtitle("Disaggregated") +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = guide_legend(title.position="top"))

multiplot(x1,x2, cols = 1)




# Intensive margin in Conclusions and discussion part ----
bar <- df %>%
  group_by(Treatment) %>%
  summarise(
    n = sum(Contributed),
    meanCon = mean(Contribution),
    intMarg = mean(Contribution[Contributed == 1]),
    sdintMarg = sd(Contribution[Contributed == 1]),
    seintMarg = sd(Contribution[Contributed == 1])/sqrt(n),
    meintMarg = qt(1-0.05/2, df = n)*seintMarg
  )

x1 <- ggplot(data = bar, aes(x = Treatment, y = intMarg)) +
  geom_bar(fill = "grey40",  stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = (intMarg - meintMarg), ymax = (intMarg + meintMarg)), width = 0.1) +
  #  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(0,5)) +
  labs(x = "Experimental group", y = "Intensive margin [€]") +
  theme_bw()+
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

bar <- df %>%
  group_by(Treatment, Location) %>%
  summarise(
    n = sum(Contributed),
    meanCon = mean(Contribution),
    intMarg = mean(Contribution[Contributed == 1]),
    sdintMarg = sd(Contribution[Contributed == 1]),
    seintMarg = sd(Contribution[Contributed == 1])/sqrt(n),
    meintMarg = qt(1-0.05/2, df = n)*seintMarg
  )

x2 <- ggplot(data = bar, aes(x = Treatment, y = intMarg, fill = Location)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge(width = .35)) +
  geom_errorbar(aes(ymin = (intMarg - meintMarg), ymax = (intMarg + meintMarg)), width = 0.1, position = position_dodge(width = .35)) +
  #  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(0,5)) +
  labs(x = "Experimental group", y = "Intensive margin [€]") +
  theme_bw()+
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

multiplot(x1,x2, cols = 1)
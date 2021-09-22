##############################################
#
#  R Code to reproduce analyses from:
#  "Tips From the Top: Do the best performers really give the best advice?"
#  David Levari, Timothy Wilson & Daniel Gilbert
#  Code by David Levari
#  email: david.levari@gmail.com
#  please email me directly if you see any errors or have any questions.
#  last update : 2021 September 8
#
################################################################################################

# Instructions ------------------------------------------------------------

# How to use this code: launch a new RStudio session by opening the ".Rproj" 
# file in this folder, and then open this R file in that session. Then the code 
# should run properly. Please email me with any problems. 

# Outline -----------------------------------------------------------------

# 1) Set up environment
# 2) Study 1
# 3) Study 2 (Word Scramble)
# 4) Study 3
# 5) Study 4
# 6) Study S1 (Darts)
# 7) Study S2 (Colorlink)
# 8) Task Performance Permutation Tests
# 9) Analyses of Curvilinear Relationships in Studies 2, S1, and S2
# 10) Reliability of helpfulness and improvement measures
# 11) Studies 2, S1 and S2 Advisor Improvement
# 12) Studies S3a and S3b
# 13) Study S4a
# 14) Study S4b
# 15) Generate figure images

# 1) Set up environment ---------------------------------------------------

# function to load the packages or install them if not present
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("plyr","reshape2","ggplot2","lme4","pbkrtest",
              "lsr","ltm","piecewiseSEM","mediation",
              "lmerTest","expm","stringi",
              "psych","performance")
ipak(packages) # install packages

set.seed(101) # set seed for reproducible random draws

# function to check models with multiple optimizers
# to assess lme4 convergence issues
allFits <- function(model){
  modelname <- paste(deparse(substitute(model)),"allFit",sep=".")
  summaryname <- paste("ss",deparse(substitute(model)),sep=".")
  model.allFit <- allFit(model)
  ss <- summary(model.allFit)
  assign(modelname, model.allFit, env=.GlobalEnv)
  assign(summaryname, summary(model.allFit), env=.GlobalEnv)
  print(ss$fixef)
  print(ss$which.OK)
}

std.error <- function(x) {
  x <- x[!is.na(x)]
  sd(x,na.rm=TRUE) / sqrt(length(x))} #define standard error

# Set default theme for journal-compliant GGPlots
theme_journal <- function () { 
  font <- "Helvetica"   #assign font family up front
  theme_classic() %+replace%    #replace elements we want to change
  theme(
    axis.ticks = element_blank(),
    # legend.key = element_blank(),
    # legend.background = element_blank(),
    # panel.grid = element_blank(),
    axis.text = element_text(family = font,
                             size = 9),
    axis.title = element_text(family = font,
                              size = 10)
  )
}

# 2) Study 1 -----------------------------------

## LOAD AND CLEAN UP DATA

study1.all <- read.csv("study1_data.csv") #import datafile

# Label which condition participants saw
study1.all$condition <- gsub("Q167\\|","",study1.all$hypothetical_DO)
study1.all$condition <- gsub("\\|qual_t","",study1.all$condition)
study1.all$condition <- factor(study1.all$condition)
study1.all$condition <- revalue(study1.all$condition, 
                                c("qual_fr"="open response", 
                                  "qual_mc"="forced choice",
                                  "qual_mc_na"="free choice",
                                  "qual_percs"="decile",
                                  "qual_slide"="percentile"))

## DESCRIPTIVES AND EXCLUSIONS

# Descriptive statistics, pre-exclusions
nrow(study1.all) # total number of participants
count(study1.all$gender) # counts by gender
summary(study1.all$age) # age
sd(study1.all$age,na.rm=TRUE) # age (SD)
count(study1.all$condition) # counts by condition

# Exclusions (failed attention check)
study1.all <- study1.all[study1.all$catch == "Other",] 

# Descriptive statistics, post-exclusions
nrow(study1.all) # total number of participants
count(study1.all$gender) # counts by gender
summary(study1.all$age) # age
sd(study1.all$age,na.rm=TRUE) # age (SD)

# Create separate dataframes for each condition
study1.freeResponse <- study1.all[which((study1.all$condition) == 
                                          "open response"),] 
study1.forcedChoice <- study1.all[which(study1.all$condition == 
                                          "forced choice"),] 
study1.freeChoice <- study1.all[which(study1.all$condition == 
                                        "free choice"),] 
study1.decile <- study1.all[which((study1.all$condition) == 
                                    "decile"),] 
study1.percentile <- study1.all[which((study1.all$condition) == 
                                        "percentile"),] 

## ANALYSIS OF FREE CHOICE CONDITION

# Descriptives for this condition
count(study1.freeChoice$gender)
summary(study1.freeChoice$age)
sd(study1.freeChoice$age,na.rm=TRUE)

# Tally number and percentage of each response
study1.freeChoiceCount <- count(study1.freeChoice$qual_mc_na)
study1.freeChoiceCount
study1.freeChoice$qual_mc_na <- factor(study1.freeChoice$qual_mc_na)
prop.table(table(study1.freeChoice$qual_mc_na))*100

## ANALYSIS OF FORCED CHOICE CONDITION

# Descriptives for this condition
count(study1.forcedChoice$gender)
summary(study1.forcedChoice$age)
sd(study1.forcedChoice$age,na.rm=TRUE)

# Tally number and percentage of each response
study1.forcedChoiceCount <- count(study1.forcedChoice$qual_mc)
study1.forcedChoiceCount
study1.forcedChoice$qual_mc <- factor(study1.forcedChoice$qual_mc)
prop.table(table(study1.forcedChoice$qual_mc))*100

# TABLE 1: Combined FREE CHOICE and FORCED CHOICE response percentages
study1.forcedFreeChoiceFreqs <- 
  merge(as.data.frame(prop.table(table(study1.freeChoice$qual_mc_na))*100),
        as.data.frame(prop.table(table(study1.forcedChoice$qual_mc))*100),
                                            by = "Var1",all=TRUE)
colnames(study1.forcedFreeChoiceFreqs) <- c("Response Chosen",
                                            "% of Responses (free choice)",
                                            "% of Responses (forced condition)")
print(study1.forcedFreeChoiceFreqs,digits=3) # view table

## ANALYSIS OF DECILE CONDITION

# Descriptives for this condition
count(study1.decile$gender)
summary(study1.decile$age)
sd(study1.decile$age,na.rm=TRUE)

# Clean up data
study1.percvars <- names(study1.decile[,(grepl("^qual_percs_", 
                     names(study1.decile)))]) #find vars with decile ratings
# convert dataframe to long format
study1.decile <- melt(study1.decile,measure.vars=study1.percvars,
                      variable.name="decile", 
                      value.name="helpfulness",
                      na.rm=TRUE) 
study1.decile$decile <- as.numeric(study1.decile$decile)
study1.decile$decilebin <- as.factor(study1.decile$decile)
study1.decile$helpfulness <- as.factor(study1.decile$helpfulness)
study1.decile$helpfulness <- as.numeric(study1.decile$helpfulness)

# Did people predict that higher performing deciles would give better advice?
study1.decile$decile_rs <- abs(study1.decile$decile - 11) # revalue deciles
study1.decileModel1 <- lmer(helpfulness~decile_rs + (decile_rs|ResponseId), 
                            data = study1.decile)
summary(study1.decileModel1) # model summary
confint(study1.decileModel1) # confidence intervals

# Assess fit based on random terms in model
study1.decileModel2 <- lmer(helpfulness~decile + (1|ResponseId), 
                            data = study1.decile)
anova(study1.decileModel2,study1.decileModel1) # random slopes improve model fit
anova(study1.decileModel2,study1.decileModel1)$Chisq[2]
study1.decileModel3 <- lmer(helpfulness~decile + (decile - 1|ResponseId), 
                            data = study1.decile)
anova(study1.decileModel3,study1.decileModel1) # random intercepts improve model fit
anova(study1.decileModel3,study1.decileModel1)$Chisq[2]

# FIGURE 1: Responses in the Decile Condition of Study 1
figure1 <- ggplot(study1.decile,aes(x=decile,y=helpfulness)) + 
  stat_summary(fun="mean", geom="bar",fill="white",colour="black") +
  stat_summary(fun.data=mean_se,geom="errorbar",width=.3) +
  labs(x="Advisor's Performance Decile", y="Predicted Helpfulness") + 
  scale_x_continuous(trans="reverse",breaks=c(1:10),labels=c("1st","2nd","3rd",
                                                             "4th","5th","6th",
                                                             "7th","8th","9th",
                                                             "10th")) + 
  expand_limits(y=c(0,7)) +
  scale_y_continuous(breaks=c(0:7)) +
  theme_journal() +
  NULL
figure1

## ANALYSIS OF THE PERCENTILE CONDITION

# Descriptives for this condition
count(study1.percentile$gender)
summary(study1.percentile$age)
sd(study1.percentile$age,na.rm=TRUE)

# create table of percentage chosen for each percentile of performance
study1.percentileTable <- as.data.frame(table(study1.percentile$qual_slide_1))
study1.percentileTable$percentage <- 
  study1.percentileTable$Freq/sum(study1.percentileTable$Freq)*100
study1.percentileTable$percentile <- 
  as.numeric(levels(study1.percentileTable$Var1))

# what % of participants wanted advice from the 99th percentile of performance?
study1.percentileTable[study1.percentileTable$percentile==99,]$percentage

# FIGURE 2: Preferred Performance Percentiles in Study 1
figure2 <- ggplot(study1.percentileTable,aes(x=percentile,y=percentage)) + 
  geom_bar(stat="identity", fill="white",color="black") +
  xlab("Performance Percentile Chosen") + ylab("Percent Who Chose") + theme_bw() +
  geom_segment(aes(x=0,y=0,xend=99,yend=0)) + 
  theme_journal() +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks=c(0,25,50,75,99)) +
  NULL
figure2

# ANALYSIS FOR THE FREE RESPONSE CONDITION

# Descriptives for this condition
count(study1.freeResponse$gender)
summary(study1.freeResponse$age)
sd(study1.freeResponse$age,na.rm=TRUE)

# import RA coding for free response condition
study1.freeResponseCoding <- read.csv("study1_freeResponseCoding.csv")
count(study1.freeResponseCoding$type.of.explanation) # how often each category was mentioned
print(prop.table(table(study1.freeResponseCoding$type.of.explanation))*100,digits=3) # percentage

# redo counts with and without people who didn't understand questions
study1.freeResponseFrequencies <- merge(as.data.frame(prop.table(table(
                      study1.freeResponseCoding$type.of.explanation))*100),
                      as.data.frame(prop.table(table(study1.freeResponseCoding
                         [which(study1.freeResponseCoding$type.of.explanation!=
                              "No answer/did not understand question")
                             ,]$type.of.explanation))*100),
                      by = "Var1",all=TRUE)
colnames(study1.freeResponseFrequencies) <- c("Quality of Teacher Desired",
                                              "% of Responses (all participants)",
                                              "% of Responses (with exclusions) ")
study1.freeResponseFrequencies

# 3) Study 2 (Word Scramble) ---------------------------------------

## LOAD ADVISOR DATA
study2.advisors.wide <- read.csv("study2_advisors.csv",header=TRUE)

## DESCRIPTIVE STATS AND EXCLUSIONS

# Descriptive stats for all advisors
nrow(study2.advisors.wide) # number of participants
count(study2.advisors.wide$gender) # counts by gender
summary(study2.advisors.wide$age) # age breakdown of participants
sd(study2.advisors.wide$age) # age SD of participants

# Exclusions (see main text for details)
study2.advisors.wide <- study2.advisors.wide[is.na(study2.advisors.wide$exclude),]

# Descriptive stats for all advisors, post-exclusions
count(study2.advisors.wide$gender)
summary(study2.advisors.wide$age)
sd(study2.advisors.wide$age)

## CLEAN ADVISOR DATA

ordervars <- c("s1","s2","s3","s4","s5","s6") # make list of the order of boards
allboards <- names(study2.advisors.wide[,
                  (grepl("_s$", names(study2.advisors.wide)))])[-1] # make list of all rounds
study2.advisors.long <- melt(study2.advisors.wide,measure.vars = ordervars,
                             variable.name = "round",value.name = "score")
study2.advisors.long$roundnum <- as.numeric(study2.advisors.long$round)
study2.advisors.long.byboard <- melt(study2.advisors.wide,measure.vars = allboards,
                                     variable.name = "board",value.name = "score")

# ADVISOR PERFORMANCE
study2.advisors.wide$advisormean <- 
  apply(study2.advisors.wide[,c(allboards)],1,mean)
summary(study2.advisors.wide$advisormean) # How many words found on average
sd(study2.advisors.wide$advisormean)

# Did advisors improve across rounds?
study2.model1a <- lmer(score ~ roundnum + (1|participant),
                       data=study2.advisors.long)
summary(study2.model1a)
study2.model1b <- lmer(score ~ roundnum + (roundnum|participant),
                       data=study2.advisors.long)
summary(study2.model1b)
anova(study2.model1a,study2.model1b) # random slopes do not improve model fit
anova(study2.model1a,study2.model1b)$Chisq[2]
confint(study2.model1a)

#Advice wordcount
summary(study2.advisors.wide$advice_wordcount) # mean
sd(study2.advisors.wide$advice_wordcount) # SD

# Advisor's expectations of advice quality
summary(study2.advisors.wide$adv_imp_1) # predictions of advice efficacy
sd(study2.advisors.wide$adv_imp_1) # SD of predictions of advice efficacy
cor.test(study2.advisors.wide$adv_imp_1,study2.advisors.wide$advisor_mean)

# FIGURE 3: The Relationship Between Advisors’ Performances and 
# Estimates of the Quality of Their Advice in Study 2
figure3 <- ggplot(study2.advisors.wide,aes(x=advisor_mean,y=adv_imp_1)) + 
  geom_point() + 
  stat_smooth(method="lm",colour="black") + 
  labs(x="Advisor Performance",y="Advisor Rating of Advice Quality") +
  xlim(0,28) + scale_x_continuous(breaks=c(0,5,10,15,20,25)) +
  theme_journal() + 
  theme(panel.grid = element_blank()) +
  NULL
figure3

# LOAD ADVISEE DATA
study2.advisees_wide <- read.csv("study2_advisees.csv",header=TRUE) # load data

# Descriptive stats, all advisees
nrow(study2.advisees_wide)
count(study2.advisees_wide$gender)
summary(study2.advisees_wide$age)
sd(study2.advisees_wide$age)

# Create variables to track order for each round
allboards <- names(study2.advisees_wide[,(grepl(
  "_d$", names(study2.advisees_wide)))]) # make list of all rounds
ordervars <- c("s1","s2","s3","s4","s5","s6") # make list of the order of boards
postboards <- allboards[-1] # list of all rounds except pre-advice round
count(study2.advisees_wide$condition) # participants in each condition, pre-exclusion

# Exclusions
findNAs <- function(v1,v2,v3,v4,v5,v6){
  sum(is.na(v1),is.na(v2),is.na(v3),
      is.na(v4),is.na(v5),is.na(v6))} # function to find missing values in any boards
study2.advisees_wide <- ddply(study2.advisees_wide,.(participant), mutate,
                              rmissing = findNAs(eise_d,lnpk_d,mpeh_d,
                                                 woln_d,demi_d,nwsr_d))
sum(study2.advisees_wide$rmissing) # 21 missing values
dim(study2.advisees_wide[which(
  study2.advisees_wide$rmissing > 0),])[1] # find pps with missing values
study2.advisees_wide <- study2.advisees_wide[which(
  study2.advisees_wide$rmissing == 0),] # remove participants with missing rounds
study2.advisees_wide$rmissing <- NULL # remove variable computing NA sums
rm(findNAs) # remove function computing NAs
study2.advisees_wide$totalwordsfound <- apply(study2.advisees_wide[,allboards],
                                              1,sum)
study2.advisees_wide <- study2.advisees_wide[which(
  study2.advisees_wide$totalwordsfound != 0),] # remove participants with no words found

# Exclude the advisee who reported switching from trackpad to mouse mid-task
study2.advisees_wide <- study2.advisees_wide[!study2.advisees_wide$participant 
                                             %in% c(162),]

# Descriptive stats, all advisees post exclusions
nrow(study2.advisees_wide) 
count(study2.advisees_wide$condition) 

count(study2.advisees_wide[study2.advisees_wide$condition=="treatment",]$gender)
summary(study2.advisees_wide[study2.advisees_wide$condition=="treatment",]$age)
sd(study2.advisees_wide[study2.advisees_wide$condition=="treatment",]$age)

count(study2.advisees_wide[study2.advisees_wide$condition=="control",]$gender)
summary(study2.advisees_wide[study2.advisees_wide$condition=="control",]$age)
sd(study2.advisees_wide[study2.advisees_wide$condition=="control",]$age)

# Clean up advisee data
study2.advisees_wide$baseline <- study2.advisees_wide$eise_d # compute pre-advice score

# Compute board means
study2.advisees_wide$board_mean <- apply(study2.advisees_wide[,allboards],1,mean)
study2.advisees_wide$board_median <- apply(study2.advisees_wide[,allboards],1,median)
# Compute difference scores
study2.advisees_wide$pmean <- apply(study2.advisees_wide[,postboards],1,mean)
study2.advisees_wide$pmedian <- apply(study2.advisees_wide[,postboards],1,median)

study2.advisees_wide$diff12 <- study2.advisees_wide$lnpk_d - 
  study2.advisees_wide$baseline #first board to second
study2.advisees_wide$diff16 <- study2.advisees_wide$nwsr_d - 
  study2.advisees_wide$baseline #first board to last
study2.advisees_wide$pdiffmean <- study2.advisees_wide$pmean - 
  study2.advisees_wide$baseline #first board to average of rest
study2.advisees_wide$pdiffmedian <- study2.advisees_wide$pmedian - 
  study2.advisees_wide$baseline #first board to average of rest

study2.advisees_wide$percentImprovement <- ifelse(study2.advisees_wide$baseline==0, NA,
                    ifelse(study2.advisees_wide$condition=="treatment",
                       study2.advisees_wide$pdiffmean/
                       study2.advisees_wide$baseline*100,NA)) # percent improvement using mean
summary(study2.advisees_wide$percentImprovement)

# Convert data from wide to long format
study2.advisees_long <- melt(study2.advisees_wide,measure.vars=allboards,
                             variable.name="board", value.name="score",na.rm=TRUE)
study2.advisees_long <- study2.advisees_long[order(study2.advisees_long$participant),]
study2.advisees_long$boardnum <- as.numeric(study2.advisees_long$board)

# Advisee's performance

# Baseline difference between conditions?
bartlett.test(baseline ~ condition, data=study2.advisees_wide) 
t.test(study2.advisees_wide$baseline ~ study2.advisees_wide$condition) 
cohensD(study2.advisees_wide$baseline ~ study2.advisees_wide$condition) 

# Pre-post advice difference by condition?
bartlett.test(pdiffmean ~ condition, data=study2.advisees_wide) 
t.test(study2.advisees_wide$pdiffmean ~ study2.advisees_wide$condition)
cohensD(study2.advisees_wide$pdiffmean ~ study2.advisees_wide$condition)

# Models: Does advisor performance predict advisee score?
study2.model2a <- lmer(score ~ (boardnum + advisor_mean)^2 + (boardnum|participant), 
                      data = study2.advisees_long)
summary(study2.model2a) # convergence warning but let's examine on next line
allFits(study2.model2a) # diagnose convergence issue, looks fine
confint(study2.model2a)

# Compare simpler model without interaction between terms
study2.model2b <- lmer(score ~ boardnum + advisor_mean + (boardnum|participant), 
                      data = study2.advisees_long)
summary(study2.model2b)
anova(study2.model2b,study2.model2a) # does interaction improve model fit?

# Compare simpler model without random intercept term
study2.model2c <- lmer(score ~ (boardnum + advisor_mean)^2 + (boardnum-1|participant), 
                       data = study2.advisees_long)
summary(study2.model2c)
anova(study2.model2a,study2.model2c)

# Compare simpler model without random slope term
study2.model2d <- lmer(score ~ (boardnum + advisor_mean)^2 + (1|participant), 
                       data = study2.advisees_long)
summary(study2.model2d)
anova(study2.model2a,study2.model2d)

# FIGURE 4: The Relationship Between Advisors’ Performances and Advisee’s Improvement in Study 2
figure4 <- ggplot(study2.advisees_wide[study2.advisees_wide$condition=="treatment",],
                  aes(x=advisor_mean,y=pdiffmean)) + geom_point() +
  stat_smooth(method="lm",colour="black") + xlab("Advisor Performance") + 
  ylab("Advisee Improvement") +
  xlim(0,28) + scale_x_continuous(breaks=c(0,5,10,15,20,25)) +
  theme_journal() + 
  theme(panel.grid = element_blank()) +
  NULL
figure4

# Advisee perceptions of advice
summary(study2.advisees_wide$adv_helpf) # perceived helpfulness
summary(study2.advisees_wide$adv_imp_1) # perceived improvement

# Create perceived helpfulness and improvement scores for each advisor
study2.advisorMeans <- ddply(study2.advisees_wide,.(advisor,
                                                    advisor_mean,
                                                    advisor_var,
                                                    advisor_adv_imp,
                                                    advisor_sd),
                             summarise,
                             advisor_efficacy = mean(pdiffmean),
                             mean_helpf = mean(adv_helpf,na.rm=TRUE),
                             mean_imp = mean(adv_imp_1,na.rm=TRUE),
                             advisor_efficacy_perc = mean(percentImprovement,na.rm=TRUE),
                             upper = mean(pdiffmean)+ #XX might be a bug here with var()
                               (sqrt(var(pdiffmean)/length(pdiffmean))),
                             lower = mean(pdiffmean)-
                               (sqrt(var(pdiffmean)/length(pdiffmean))),
                             .drop=TRUE)

study2.advisorMeans <- transform(study2.advisorMeans, 
                                 advisor=reorder(advisor, advisor_efficacy)) 
study2.advisorMeans <- study2.advisorMeans[which(study2.advisorMeans$advisor != "ad00"),]

# Correlations between advisor ability and perceived helpfulness/improvement scores
cor.test(study2.advisorMeans$advisor_mean, study2.advisorMeans$mean_helpf)
cor.test(study2.advisorMeans$advisor_mean, study2.advisorMeans$mean_imp) 

# FIGURE 5: Perceived Helpfulness in Study 2
figure5 <- ggplot(study2.advisorMeans,aes(x=advisor_mean,y=mean_helpf)) + geom_point() +
  stat_smooth(method="lm",colour="black")  + 
  labs(x="Advisor Performance",  y="Advisee Rating of Helpfulness") +
  xlim(0,28) + scale_x_continuous(breaks=c(0,5,10,15,20,25)) +
  theme_journal() +
  theme(panel.grid = element_blank()) +
  NULL
figure5

# 4) Study 3 --------------------------------------------------------------

## LOAD DATA
study3.wide <- read.csv("study3_data.csv",header=TRUE)
study3.advisors <- read.csv("study3_advisorEfficacy.csv",header=TRUE) 
study3.wide$participant <- factor(study3.wide$participant)

## DESCRIPTIVES AND EXCLUSIONS 

# Sample descriptive stats, pre exclusion
nrow(study3.wide) # number of participants
count(study3.wide$gender)
summary(study3.wide$age)
sd(study3.wide$age)

# Create lists of all displayed pieces of advice and their order
adviceitems <- names(study3.wide[,(grepl("^ad", names(study3.wide)))]) 
adviceorders <- names(study3.wide[,(grepl("^s_", names(study3.wide)))]) 

# Exclude pps (see text for details)
study3.wide <- study3.wide[which(is.na(study3.wide$exclude)),] 

# Sample descriptive stats, post exclusion
nrow(study3.wide) # number of participants
count(study3.wide$gender)
summary(study3.wide$age)
sd(study3.wide$age)

## MERGE IN STUDY 2 ADVISOR DATA

study3.ratings_wide <- study3.wide[,adviceitems] # just subset advice items
item.sw <- apply(study3.ratings_wide, 2, shapiro.test)
item.ID <- colnames(study3.ratings_wide)
extract.wp <- function(x) { c((x)$statistic[[1]],(x)$p.value[[1]]) }
itemdf <- ldply(item.sw,extract.wp)
itemdf$mean_rating <- apply(study3.ratings_wide, 2, mean, na.rm=TRUE)
itemdf$sd <- apply(study3.ratings_wide, 2, sd, na.rm=TRUE)
std <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
itemdf$se <- apply(study3.ratings_wide, 2, std)
names(itemdf) <- c("ID", "W", "p", "mean_rating", "sd", "se")
itemdf <- itemdf[order(itemdf$mean_rating),] #order by mean
itemdf$ID <- factor(itemdf$ID)
study3.advisors$ID <- study3.advisors$advisor
study3.advisors <- merge(itemdf,study3.advisors,by="ID")
study3.advisors <- study3.advisors[order(study3.advisors$mean_rating),] #order by mean
study3.advisors$mean_efficacy <- study3.advisors$advisor_efficacy
study3.long <- melt(study3.wide,measure.vars=c(item.ID,"catch1","catch2"),
                 variable.name="item", value.name="rating",na.rm=TRUE)
study3.long <- merge(study3.long,study3.advisors,by.x="item",by.y="ID",all=TRUE)
rm(itemdf,item.ID,item.sw)

# Correlations between mean ratings and actual advisor efficacy/ability
cor.test(study3.advisors$mean_rating,study3.advisors$advisor_mean) 
cor.test(study3.advisors$mean_rating,study3.advisors$mean_efficacy)

# FIGURE 6: The Relationship Between Advisors’ Performances and Estimates of the 
# Effectiveness of Their Advice in Study 3
figure6 <- ggplot(study3.advisors, aes(x=advisor_mean,y=mean_rating)) + geom_point() + 
  stat_smooth(method="lm",colour="black") +
  labs(x= "Advisor Performance", y="Participant Rating of Effectiveness") +
  scale_x_continuous(breaks=c(0,5,10,15,20,25)) +
  theme_journal() +
  theme(panel.grid = element_blank()) +
  NULL
figure6

# 5) Study 4 --------------------------------------------------------------

## LOAD DATA
study4.coderA <- read.csv("study4_coderA.csv",header=TRUE)
study4.coderB <- read.csv("study4_coderB.csv",header=TRUE)
study4.data <- merge(study4.coderA,study4.coderB,by="advice_id",suffixes=c(".a",".b"))
study4.data$advicetext.b <- NULL
study4.data$advicetext <- study4.data$advicetext.a
study4.data$advicetext.a <- NULL
rm(study4.coderA,study4.coderB)

# Create names of different coding category variables
ath <- names(study4.data)[c(2,9)]
act <- names(study4.data)[c(3,10)]
art <- names(study4.data)[c(4,11)]
amt <- names(study4.data)[c(5,12)]
aff <- names(study4.data)[c(6,13)]
neg <- names(study4.data)[c(7,14)]
obv <- names(study4.data)[c(8,15)]
allvars <- c("ath","act","art","amt","aff","neg","obv")

# Reliability analysis
study4.alpha <- data.frame(matrix(ncol = 2, nrow = length(allvars)))
for (i in 1:length(allvars)){
  study4.alpha$dimension[i] <- allvars[i]
  study4.alpha$cronbach[i] <- cronbach.alpha(study4.data[,get(allvars[i])])$alpha
}
study4.alpha <- study4.alpha[,3:4]
study4.alpha # Cronbach's alphas for all seven properties

# Average together RA ratings
study4.data$ath <- apply(study4.data[,ath],1,mean)
study4.data$act <- apply(study4.data[,act],1,mean)
study4.data$art <- apply(study4.data[,art],1,mean)
study4.data$amt <- apply(study4.data[,amt],1,mean)
study4.data$aff <- apply(study4.data[,aff],1,mean)
study4.data$neg <- apply(study4.data[,neg],1,mean)
study4.data$obv <- apply(study4.data[,obv],1,mean)

study4.ratings <- read.csv("study4_ratings.csv",header=TRUE)

names(study4.ratings) <- c("advisor",
                         "advicetext",
                         "wordcount",
                         "self_eff",
                         "age",
                         "gender",
                         "ability",
                         "efficacy",
                         "helpf",
                         "imp",
                         "turkrating")

study4.ratings <- merge(study4.ratings,study4.data[,c(1,17:18,20:23)],by.x="advisor",
                        by.y="advice_id")

# Standardize ratings by task
study4.ratings$helpfZ <- scale(study4.ratings$helpf,center=TRUE,scale=TRUE)
study4.ratings$impZ <- scale(study4.ratings$imp,center=TRUE,scale=TRUE)
study4.ratings$shine <- apply(study4.ratings[,c("helpfZ","impZ")],1,mean)
study4.ratings$abilityZ <- scale(study4.ratings$ability,center=TRUE,scale=TRUE)

# Create ratio of affirmative to negative statements
study4.ratings$affneg <- study4.ratings$aff/(study4.ratings$aff+study4.ratings$neg)

# Model predicting helpfulness
study4.helpf.lm1 <- lm(helpfZ ~ ath,data=study4.ratings)
study4.helpf.lm2 <- lm(helpfZ ~ ath + act,data=study4.ratings)
study4.helpf.lm3 <- lm(helpfZ ~ ath + act + obv,data=study4.ratings)
study4.helpf.lm4 <- lm(helpfZ ~ ath + act + obv + amt,data=study4.ratings)
study4.helpf.lm5 <- lm(helpfZ ~ ath + act + obv + amt + affneg,data=study4.ratings)
study4.modlist1 <- list(study4.helpf.lm1,study4.helpf.lm2,study4.helpf.lm3,
                        study4.helpf.lm4,study4.helpf.lm5)
rsquared(study4.modlist1,study4.ratings)

summary(study4.helpf.lm3)
summary(study4.helpf.lm4)
summary(study4.helpf.lm5)
anova(study4.helpf.lm3,study4.helpf.lm4)
anova(study4.helpf.lm4,study4.helpf.lm5)
confint(study4.helpf.lm4)

# Models predicting perceived improvement 
study4.imp.lm1 <- lm(impZ ~ ath,data=study4.ratings)
study4.imp.lm2 <- lm(impZ ~ ath + act,data=study4.ratings)
study4.imp.lm3 <- lm(impZ ~ ath + act + obv,data=study4.ratings)
study4.imp.lm4 <- lm(impZ ~ ath + act + obv + amt,data=study4.ratings)
study4.imp.lm5 <- lm(impZ ~ ath + act + obv + amt + affneg,data=study4.ratings)
study4.modlist2 <- list(study4.imp.lm1,study4.imp.lm2,study4.imp.lm3,
                        study4.imp.lm4,study4.imp.lm5)
rsquared(study4.modlist2,study4.ratings)

summary(study4.imp.lm3)
summary(study4.imp.lm4)
summary(study4.imp.lm5)
anova(study4.imp.lm4,study4.imp.lm5)
confint(study4.imp.lm4)

# Models controlling for wordcount
study4.helpf.lm.wordcount <- lm(helpfZ ~ ath + act + obv + amt + wordcount,
                                data=study4.ratings)
summary(study4.helpf.lm.wordcount)
confint(study4.helpf.lm.wordcount)
study4.imp.lm.wordcount <- lm(impZ ~ ath + act + obv + amt + wordcount,
                              data=study4.ratings)
summary(study4.imp.lm.wordcount)
confint(study4.imp.lm.wordcount)

# Relationship between amount and improvement
cor.test(study4.ratings$amt,study4.ratings$efficacy)

# Average together helpfulness and perceived improvement
study4.ratings$perceivedQuality <- apply(study4.ratings[,c("helpfZ","impZ")],1,mean)

# Mediation analysis
perceivedQuality.model.m <- lm(amt~ability,data=study4.ratings)
perceivedQuality.model.y <- lm(perceivedQuality~ability+amt,data=study4.ratings)
summary(perceivedQuality.model.m)
summary(perceivedQuality.model.y) 
confint(perceivedQuality.model.y)
perceivedQuality.model.m2 <- lm(perceivedQuality~ability,data=study4.ratings)
perceivedQuality.model.y2 <- lm(amt~ability+perceivedQuality,data=study4.ratings)
summary(perceivedQuality.model.m2)

perceivedQuality.med.out <- mediation::mediate(perceivedQuality.model.m, 
                                               perceivedQuality.model.y, sims = 10000, 
                                               boot = TRUE, boot.ci.type = "bca",
                                               treat = "ability", mediator = "amt", 
                                               covariates = NULL, outcome = NULL,
                                               control = NULL, conf.level = .95, 
                                               control.value = 0, treat.value = 1, 
                                               long = TRUE, dropobs = FALSE,
                                               robustSE = FALSE, cluster = NULL, 
                                               group.out = NULL)

summary(perceivedQuality.med.out)

# Function to extract mediation results for plotting
extract_mediation_summary <- function (x) { 
  clp <- 100 * x$conf.level
  isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) || 
                   (inherits(x$model.y, "glm") && x$model.y$family$family == 
                      "gaussian" && x$model.y$family$link == "identity") || 
                   (inherits(x$model.y, "survreg") && x$model.y$dist == 
                      "gaussian"))
  printone <- !x$INT && isLinear.y
  if (printone) {
    smat <- c(x$d1, x$d1.ci, x$d1.p)
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    rownames(smat) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")
  } else {
    smat <- c(x$d0, x$d0.ci, x$d0.p)
    smat <- rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    smat <- rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
    smat <- rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
    smat <- rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
    smat <- rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
    rownames(smat) <- c("ACME (control)", "ACME (treated)", 
                        "ADE (control)", "ADE (treated)", "Total Effect", 
                        "Prop. Mediated (control)", "Prop. Mediated (treated)", 
                        "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
  }
  colnames(smat) <- c("Estimate", paste(clp, "% CI Lower", sep = ""), 
                      paste(clp, "% CI Upper", sep = ""), "p-value")
  smat
}

perceivedQuality.med.out.df <- data.frame(extract_mediation_summary(perceivedQuality.med.out))
rownames(perceivedQuality.med.out.df) <- c("Indirect Effect","Direct Effect",
                                          "Total Effect","Prop Mediated")
perceivedQuality.med.out.df <- tibble::rownames_to_column(perceivedQuality.med.out.df,"Effect")
perceivedQuality.med.out.df <- perceivedQuality.med.out.df[perceivedQuality.med.out.df$Effect!="Prop Mediated",]
perceivedQuality.med.out.df$Effect <- factor(perceivedQuality.med.out.df$Effect, 
                                       levels=c("Total Effect",
                                                "Direct Effect",
                                                "Indirect Effect"))

# FIGURE 8: Effect sizes of mediation analysis in Study 4
figure8 <- ggplot(perceivedQuality.med.out.df,
                  aes(x=Effect,y=Estimate)) +
                  geom_point() +
                  geom_hline(yintercept=0,linetype="dashed") +
                  geom_errorbar(aes(ymin=X95..CI.Lower, ymax=X95..CI.Upper),
                                width=0,position=position_dodge(.9)) +
                  coord_flip() + theme_journal() +
                  scale_y_continuous(breaks=c(seq(0,.12,.02))) +
                  theme(axis.title.y=element_blank()) +
                  NULL
figure8  

# 6) Study S1 (Darts) ----------------------------------------------------

# LOAD ADVISOR DATA
studyS1.advisors.wide <- read.csv("studyS1_advisor_data.csv",header=TRUE)

# Sample descriptive stats, pre-exclusions
nrow(studyS1.advisors.wide) # number of participants
count(studyS1.advisors.wide$advisor_gender) # gender breakdown of participants
summary(studyS1.advisors.wide$advisor_age) # age breakdown of participants
sd(studyS1.advisors.wide$advisor_age,na.rm=TRUE) # age SD of participants

# Exclusions (see text for details)
studyS1.advisors.wide <- studyS1.advisors.wide[!studyS1.advisors.wide$advisornum 
                                               %in% c(14,55,62,56,87),]

# Sample descriptive stats, post-exclusions
nrow(studyS1.advisors.wide) # number of participants
count(studyS1.advisors.wide$advisor_gender) # gender breakdown of participants
summary(studyS1.advisors.wide$advisor_age) # age breakdown of participants
sd(studyS1.advisors.wide$advisor_age,na.rm=TRUE) # age SD of participants

# Clean up advisor data
advisorThrows=c("advisorThrow1","advisorThrow2","advisorThrow3","advisorThrow4",
                "advisorThrow5", "advisorThrow6","advisorThrow7","advisorThrow8",
                "advisorThrow9","advisorThrow10",
                "advisorThrow11","advisorThrow12")

studyS1.advisors.long <- melt(studyS1.advisors.wide,measure.vars = advisorThrows,
                              variable.name = "throw",value.name = "distance")

# Rescale throws so 0 is a miss and 10 is a bullseye
# Darts were thrown at a board with an actual radius of 20.15 cm, but 
# recorded from a photocopied scoring sheet of the board with a radius of 
# 9.3cm. Raw scores in this dataset ($distance_raw) reflect distances on 
# the paper scoring sheet rather than the actual dimensions of the board.
studyS1.advisors.long$distance_raw <- studyS1.advisors.long$distance
studyS1.advisors.long$accuracy <- studyS1.advisors.long$distance*-1
studyS1.advisors.long$accuracy <- studyS1.advisors.long$accuracy + 9.3
studyS1.advisors.long$accuracy[studyS1.advisors.long$accuracy > 20] <- 0
studyS1.advisors.long$accuracy <- studyS1.advisors.long$accuracy*(10/9.3)

# Make throw into numbers
studyS1.advisors.long$thrownum <- studyS1.advisors.long$throw
studyS1.advisors.long$thrownum <- as.numeric(studyS1.advisors.long$throw)

# Other misc cleanup
studyS1.advisors.long$advisor_handed <- as.factor(studyS1.advisors.long$advisor_handed)

studyS1.advisors.wide <- dcast(studyS1.advisors.long, advisornum + advisor + 
                                 advisor_misses +
                                 advisor_obs + advisor_handed + advisor_playedbefore + 
                                 advisors_playedyears +
                                 advisor_playedoften + advisor_adv_helpf + 
                                 advisor_well + advisor_advicebefore +
                                 advisor_english + advisor_age + advisor_gender + 
                                 advisor_advice_text + advisor_advice_wordcount ~ throw, 
                               value.var="accuracy")

# Advisor performance
studyS1.advisors.wide$advisormean <- apply(studyS1.advisors.wide[,advisorThrows],
                                           1,mean) # make mean of all throws for each participant
studyS1.advisors.wide$advisormedian <- apply(studyS1.advisors.wide[,advisorThrows],
                                             1,median) # same thing, with median
summary(studyS1.advisors.wide$advisormean)
sd(studyS1.advisors.wide$advisormean,na.rm=TRUE)

# Did advisors improve across rounds?
studyS1.advisor.model1a <- lmer(accuracy ~ thrownum + (thrownum|advisor),
                                data=studyS1.advisors.long)
summary(studyS1.advisor.model1a)
allFits(studyS1.advisor.model1a) # diagnose convergence issue, looks fine
confint(studyS1.advisor.model1a)

# Compare simpler model without random slopes
studyS1.advisor.model1b <- lmer(accuracy ~ thrownum + (1|advisor),
                                data=studyS1.advisors.long)
summary(studyS1.advisor.model1b)
anova(studyS1.advisor.model1b,studyS1.advisor.model1a) # random slopes don't really improve model fit

# Compare simpler model without random intercepts
studyS1.advisor.model1c <- lmer(accuracy ~ thrownum + (thrownum - 1|advisor),
                                data=studyS1.advisors.long)
summary(studyS1.advisor.model1c)
anova(studyS1.advisor.model1c,studyS1.advisor.model1a) # random intercepts improve model fit

# Advisor's perceptions of advice quality
summary(studyS1.advisors.wide$advisor_advice_wordcount) # advice wordcount
sd(studyS1.advisors.wide$advisor_advice_wordcount,na.rm=TRUE) # advice wordcount SD
summary(studyS1.advisors.wide$advisor_adv_helpf) # predictions of advice efficacy
sd(studyS1.advisors.wide$advisor_adv_helpf) # SD of predictions of advice efficacy
cor.test(studyS1.advisors.wide$advisor_adv_helpf,studyS1.advisors.wide$advisormean)

# Analyze advisee data
studyS1.advisees.wide <- read.csv("studyS1_advisee_data.csv",header=TRUE)

# Pre-exclusion demographics
nrow(studyS1.advisees.wide) # number of participants
count(studyS1.advisees.wide$gender) # gender breakdown of participants
summary(studyS1.advisees.wide$age) # age breakdown of participants
sd(studyS1.advisees.wide$age,na.rm=TRUE) # age SD of participants
count(studyS1.advisees.wide$condition)

# Exclusions (see text for details)
studyS1.advisees.wide <- studyS1.advisees.wide[is.na(studyS1.advisees.wide$exclude),]
studyS1.advisees.wide <- studyS1.advisees.wide[studyS1.advisees.wide$advicecode != 56,]

# post exclusion demographics
nrow(studyS1.advisees.wide) # number of participants
count(studyS1.advisees.wide$gender) # gender breakdown of participants
summary(studyS1.advisees.wide$age) # age breakdown of participants
sd(studyS1.advisees.wide$age,na.rm=TRUE) # age SD of participants
count(studyS1.advisees.wide$condition)

# clean up advisee data
adviseeThrows=c("throw1","throw2","throw3","throw4","throw5",
                "throw6","throw7","throw8","throw9","throw10",
                "throw11","throw12")

studyS1.advisees.long <- melt(studyS1.advisees.wide,measure.vars = adviseeThrows,
                              variable.name = "throw",value.name = "distance")

# rescale throws so 0 is a miss and 10 is a bullseye
studyS1.advisees.long$distance_raw <- studyS1.advisees.long$distance
studyS1.advisees.long$accuracy <- studyS1.advisees.long$distance*-1
studyS1.advisees.long$accuracy <- studyS1.advisees.long$accuracy + 9.3
studyS1.advisees.long$accuracy[studyS1.advisees.long$accuracy > 20] <- 0
studyS1.advisees.long$accuracy <- studyS1.advisees.long$accuracy*(10/9.3)

#Make throw into numbers
studyS1.advisees.long$thrownum <- studyS1.advisees.long$throw
studyS1.advisees.long$thrownum <- as.numeric(studyS1.advisees.long$throw)

# Other misc cleanup
studyS1.advisees.long$handed <- as.factor(studyS1.advisees.long$handed)

studyS1.advisees.wide <- dcast(studyS1.advisees.long, participant+ log_duration+
                                 advicecode+handed+observers+condition+
                                 playedbefore+playedyearsago+playedoften_txt+
                                 playedoften_num+adv_helpf+advice_imp+useadvice+
                                 english+age+gender ~ throw, value.var="accuracy")

# Mark the advisor for everyone in the control condition as "0"
studyS1.advisees.wide$advicecode <- ifelse(is.na(studyS1.advisees.wide$advicecode),0,
                                                 studyS1.advisees.wide$advicecode)

# Merge in advisor data
studyS1.advisees.wide <- merge(studyS1.advisees.wide, studyS1.advisors.wide,
                               by.x="advicecode",
                               by.y="advisornum",all.x=TRUE)
studyS1.advisees.long <- merge(studyS1.advisees.long, studyS1.advisors.wide,
                               by.x="advicecode",
                               by.y="advisornum",all.x=TRUE)


# Advisee performance
studyS1.advisees.wide$adviseemean <- apply(studyS1.advisees.wide[,adviseeThrows],
                                           1,mean) # make mean of all throws for each participant
studyS1.advisees.wide$adviseemedian <- apply(studyS1.advisees.wide[,adviseeThrows],
                                             1,median) # same thing, with median
summary(studyS1.advisees.wide$adviseemean)
sd(studyS1.advisees.wide$adviseemean,na.rm=TRUE)

# Calculate performance means pre and post advice
studyS1.advisees.wide$baselinemean <- apply(studyS1.advisees.wide
                                            [,adviseeThrows[1:6]],
                                            1,mean) # compute pre-advice score
studyS1.advisees.wide$baselinemedian <- apply(studyS1.advisees.wide
                                              [,adviseeThrows[1:6]],
                                              1,median) # compute pre-advice score
studyS1.advisees.wide$postadvicemean <- apply(studyS1.advisees.wide
                                              [,adviseeThrows[7:12]],
                                              1,mean) # compute pre-advice score
studyS1.advisees.wide$postadvicemedian <- apply(studyS1.advisees.wide
                                                [,adviseeThrows[7:12]],
                                                1,median) # compute pre-advice score
studyS1.advisees.wide$meanprepost <- studyS1.advisees.wide$postadvicemean - 
  studyS1.advisees.wide$baselinemean
studyS1.advisees.wide$medianprepost <- studyS1.advisees.wide$postadvicemedian - 
  studyS1.advisees.wide$baselinemedian

# Baseline difference between conditions?
bartlett.test(baselinemean ~ condition, data=studyS1.advisees.wide) # equal variances
t.test(studyS1.advisees.wide$baselinemean ~ studyS1.advisees.wide$condition) # no difference
cohensD(studyS1.advisees.wide$baselinemean ~ studyS1.advisees.wide$condition) # no difference

# Pre-post advice difference by condition?
bartlett.test(meanprepost ~ condition, data=studyS1.advisees.wide) # equal variances
t.test(studyS1.advisees.wide$meanprepost ~ studyS1.advisees.wide$condition) # n.s.
cohensD(studyS1.advisees.wide$meanprepost ~ studyS1.advisees.wide$condition)

studyS1.advisees.long <- merge(studyS1.advisees.long,studyS1.advisees.wide
                               [,c("participant","baselinemean")],
                               by="participant",all.x=TRUE,all.y=FALSE)

studyS1.advisees.long$postadvicethrowimprovement <- ifelse(studyS1.advisees.long$thrownum > 6,
  studyS1.advisees.long$accuracy - studyS1.advisees.long$baselinemean,NA)

# Models: Does advisor performance predict advisee score?
studyS1.advisee.model5a <- lmer(accuracy ~ thrownum + advisormean + (thrownum|participant), 
                                data = studyS1.advisees.long)
summary(studyS1.advisee.model5a)
allFits(studyS1.advisee.model5a) # diagnose convergence issue
confint(studyS1.advisee.model5a)
studyS1.advisee.model5b <- lmer(accuracy ~ (thrownum + advisormean)^2 + (thrownum|participant), 
                       data = studyS1.advisees.long)
summary(studyS1.advisee.model5b)
allFits(studyS1.advisee.model5b)
confint(studyS1.advisee.model5b)
anova(studyS1.advisee.model5b,studyS1.advisee.model5a) # drop interaction term
studyS1.advisee.model5c <- lmer(accuracy ~ thrownum + advisormean + (1|participant), 
                               data = studyS1.advisees.long)

summary(studyS1.advisee.model5c)
allFits(studyS1.advisee.model5c)
confint(studyS1.advisee.model5c)
anova(studyS1.advisee.model5a,studyS1.advisee.model5c) # keep random slopes
studyS1.advisee.model5d <- lmer(accuracy ~ thrownum + advisormean + (thrownum - 1|participant), 
                                data = studyS1.advisees.long)
summary(studyS1.advisee.model5d)
allFits(studyS1.advisee.model5d)
confint(studyS1.advisee.model5d)
anova(studyS1.advisee.model5a,studyS1.advisee.model5d) # keep random intercepts

figureS1 <- ggplot(studyS1.advisees.wide,aes(x=advisormean,y=meanprepost)) + geom_point() +
  stat_smooth(method="lm",colour="black") + xlab("Advisor Performance") + 
  ylab("Advisee Improvement") +
  # xlim(,10) + 
  theme_journal() + 
  # theme(panel.grid = element_blank()) +
  NULL
figureS1

# Advice helpfulness and performance
studyS1.advisors.summary <- ddply(studyS1.advisees.wide,.(advisor,
                                                          advisor_age,
                                                          advisor_gender,
                                                          advisormean,
                                                          advisormedian),
                                  summarise,
                                  advisor_efficacy = mean(meanprepost,na.rm=TRUE),
                                  advisor_helpf = mean(adv_helpf,na.rm=TRUE),
                                  advisor_imp = mean(advice_imp,na.rm=TRUE),
                                  .drop=TRUE)


cor.test(studyS1.advisors.summary$advisor_helpf,studyS1.advisors.summary$advisormean)
cor.test(studyS1.advisors.summary$advisor_imp,studyS1.advisors.summary$advisormean)


# 7) Study S2 (Colorlink) --------------------------------------------------

## LOAD ADVISOR DATA
studyS2.advisors.wide <- read.csv("studyS2_advisor_data.csv",header=TRUE)

# Clean up data
studyS2.advisors.wide$advisor_advice <- as.character(studyS2.advisors.wide$advisor_advice)
studyS2.advisors.wide$advice_wordcount <- stri_count(studyS2.advisors.wide$advisor_advice,
                                                     regex="\\S+")

# Descriptive stats, full sample
nrow(studyS2.advisors.wide) # number of participants
count(studyS2.advisors.wide$advisor_gender) # gender breakdown of participants
summary(studyS2.advisors.wide$advisor_age) # age breakdown of participants
sd(studyS2.advisors.wide$advisor_age,na.rm=TRUE) # age SD of participants

# Exclusions (see text for details)
studyS2.advisors.wide <- studyS2.advisors.wide[is.na(studyS2.advisors.wide$exclude),]

# Descriptive stats, post-exclusions
nrow(studyS2.advisors.wide) # number of participants
count(studyS2.advisors.wide$advisor_gender) # gender breakdown of participants
summary(studyS2.advisors.wide$advisor_age) # age breakdown of participants
sd(studyS2.advisors.wide$advisor_age,na.rm=TRUE) # age SD of participants

advisorBoards <- c("advisor_s1","advisor_s2","advisor_s3","advisor_s4","advisor_s5",
                   "advisor_s6","advisor_s7","advisor_s8","advisor_s9","advisor_s10")

# Reshape advisor data from long to wide format
studyS2.advisors.long <- melt(studyS2.advisors.wide,measure.vars = advisorBoards,
                              variable.name = "board",value.name = "score")

studyS2.advisors.long$boardnum <- as.numeric(studyS2.advisors.long$board)

# How long did it take advisors to complete an average board?
summary(studyS2.advisors.wide$advisor_board_mean)
sd(studyS2.advisors.wide$advisor_board_mean)

# Did advisors improve across rounds?
studyS2.advisor.model1a <- lmer(log(score) ~ boardnum + (boardnum|advisor_pid),
                                data=studyS2.advisors.long)
summary(studyS2.advisor.model1a)
confint(studyS2.advisor.model1a)
studyS2.advisor.model1b <- lmer(log(score) ~ boardnum + (1|advisor_pid),
                                data=studyS2.advisors.long)
summary(studyS2.advisor.model1b)
anova(studyS2.advisor.model1b,studyS2.advisor.model1a) # random slopes don't improve model fit
studyS2.advisor.model1c <- lmer(log(score) ~ boardnum + 
                                  (boardnum - 1|advisor_pid),
                                data=studyS2.advisors.long)
summary(studyS2.advisor.model1c)
anova(studyS2.advisor.model1c,studyS2.advisor.model1a) # random intercepts improve model fit

# Advisor advice wordcount and predictions about improvement
summary(studyS2.advisors.wide$advice_wordcount)
sd(studyS2.advisors.wide$advice_wordcount)
summary(studyS2.advisors.wide$advisor_adv_eff_1)
sd(studyS2.advisors.wide$advisor_adv_eff_1)
cor.test(log(studyS2.advisors.wide$advisor_board_mean),studyS2.advisors.wide$advisor_adv_eff_1)

# LOAD ADVISEE DATA
studyS2.advisees.wide <- read.csv("studyS2_advisee_data.csv",header=TRUE)

# Descriptive stats, all advisees
nrow(studyS2.advisees.wide) # number of participants
count(studyS2.advisees.wide$gender) # gender breakdown of participants
summary(studyS2.advisees.wide$age) # age breakdown of participants
sd(studyS2.advisees.wide$advisor_age,na.rm=TRUE) # age SD of participants
count(studyS2.advisees.wide$condition)

# Clean up advisee data
adviseeBoards=c("iud_c","hjt_c","ypl_c","sqt_c","jfm_c","lnc_c")
adviseeBaseline <- "iud_c"
postboards <- adviseeBoards[-1]

# Convert advisee data from wide to long format
studyS2.advisees.long <- melt(studyS2.advisees.wide,measure.vars = adviseeBoards,
                              variable.name = "board",value.name = "score")

# Make board factor into numbers
studyS2.advisees.long$boardnum <- as.numeric(studyS2.advisees.long$board)

# Calculate measures of advisee performance
studyS2.advisees.wide$pmedian <- apply(studyS2.advisees.wide[,postboards],1,median)
studyS2.advisees.wide$pmean <- apply(studyS2.advisees.wide[,postboards],1,mean)
studyS2.advisees.wide$adviseemedian <- apply(studyS2.advisees.wide[,adviseeBoards],1,median)
studyS2.advisees.wide$adviseemean <- apply(studyS2.advisees.wide[,adviseeBoards],1,mean)

studyS2.advisees.wide$diff12 <- studyS2.advisees.wide$hjt_c - 
  studyS2.advisees.wide$baseline #first board to second
studyS2.advisees.wide$diff16 <- studyS2.advisees.wide$lnc_c - 
  studyS2.advisees.wide$baseline #first board to last
studyS2.advisees.wide$pdiffmean <- studyS2.advisees.wide$pmean - 
  studyS2.advisees.wide$baseline #first board to average of rest
studyS2.advisees.wide$pdiffmedian <- studyS2.advisees.wide$pmedian - 
  studyS2.advisees.wide$baseline #first board to average of rest

studyS2.advisees.wide$perf12 <- apply(studyS2.advisees.wide
                                      [,c("iud_c","hjt_c")],1,mean)
studyS2.advisees.wide$perf123 <- apply(studyS2.advisees.wide
                                       [,c("iud_c","hjt_c","ypl_c")],1,mean)

# Take log of advisee performance
studyS2.advisees.wide$pdiffmeanlog <- log(studyS2.advisees.wide$pmean) - 
  log(studyS2.advisees.wide$baseline) #first board to average of rest

# Create advisor summary table from advisee data
studyS2.advisors.summary <- ddply(studyS2.advisees.wide,.(advisor,
                                                          advisor_advice,
                                                          advisor_adv_eff_1,
                                                          advisor_age,
                                                          advisor_gender,
                                                          advisor_board_mean,
                                                          advisor_board_median),
                                  summarise,
                                  advisor_efficacy = mean(pdiffmean,na.rm=TRUE),
                                  advisor_helpf = mean(adv_help,na.rm=TRUE),
                                  advisor_eff = mean(adv_eff_1,na.rm=TRUE),
                                  .drop=TRUE)

# Baseline difference between conditions?
bartlett.test(baseline ~ condition, data=studyS2.advisees.wide) # unequal variances
t.test(studyS2.advisees.wide$baseline ~ studyS2.advisees.wide$condition)
cohensD(studyS2.advisees.wide$baseline ~ studyS2.advisees.wide$condition) 
wilcox.test(studyS2.advisees.wide$baseline ~ studyS2.advisees.wide$condition)

# Pre-post advice difference by condition?
bartlett.test(pdiffmean ~ condition, data=studyS2.advisees.wide) # unequal variances
t.test(studyS2.advisees.wide$pdiffmean ~ studyS2.advisees.wide$condition)
cohensD(studyS2.advisees.wide$pdiffmean ~ studyS2.advisees.wide$condition)

# Models: Does advisor performance predict advisee score?
studyS2.advisee.model2a <- lmer(log(score) ~ (boardnum + log(advisor_board_mean))^2 + 
                                  (boardnum|participant), 
                                data = studyS2.advisees.long)
summary(studyS2.advisee.model2a)
confint(studyS2.advisee.model2a)
studyS2.advisee.model2b <- lmer(log(score) ~ (boardnum + log(advisor_board_mean))^2 + 
                                  (boardnum - 1|participant), 
                                data = studyS2.advisees.long)
summary(studyS2.advisee.model2b)
anova(studyS2.advisee.model2b,studyS2.advisee.model2a) # keep random intercepts
studyS2.advisee.model2c <- lmer(log(score) ~ (boardnum + log(advisor_board_mean))^2 + 
                                  (1|participant), 
                                data = studyS2.advisees.long)
summary(studyS2.advisee.model2c)
anova(studyS2.advisee.model2c,studyS2.advisee.model2a) # keep random slopes


figureS3 <- ggplot(studyS2.advisees.wide,aes(x=log(advisor_board_mean),y=pdiffmean)) + 
  geom_point() +
  stat_smooth(method="lm",colour="black") + xlab("Advisor Performance") + 
  ylab("Advisee Improvement") +
  # xlim(,10) + 
  theme_journal() + 
  NULL
figureS3

# Advisee's perceptions of advice quality
summary(studyS2.advisees.wide$adv_help) # perceptions of advice efficacy
sd(studyS2.advisees.wide$adv_help,na.rm=TRUE) # SD of perceptions of advice efficacy
summary(studyS2.advisees.wide$adv_eff_1) # perceptions of advice efficacy
sd(studyS2.advisees.wide$adv_eff_1,na.rm=TRUE) # SD of perceptions of advice efficacy

studyS2.advisors.summary <- ddply(studyS2.advisees.wide,.(advisor,
                                                          advisor_age,
                                                          advisor_gender,
                                                          advisor_board_mean,
                                                          advisor_board_median),
                                  summarise,
                                  advisor_efficacy = mean(pdiffmean,na.rm=TRUE),
                                  advisor_helpf = mean(adv_help,na.rm=TRUE),
                                  advisor_imp = mean(adv_eff_1,na.rm=TRUE),
                                  .drop=TRUE)

cor.test(studyS2.advisors.summary$advisor_helpf,studyS2.advisors.summary$advisor_board_mean)
cor.test(studyS2.advisors.summary$advisor_imp,studyS2.advisors.summary$advisor_board_mean)

# 8) Task Performance Permutation Tests -------------------------

# Note -- the sections below often are calling data loaded above; you should run 
# the entire script to this point for the code in these sections to work properly

# Word Scramble Permutation Test (Study 2)

study2.advisorBoards <- names(study2.advisors.wide[,
                                                   (grepl("_s$", names(study2.advisors.wide)))])[-1] 
study2.advisorBoards
study2.advisorScores <- study2.advisors.wide[,c(study2.advisorBoards)]
study2.actualCorrs <- cor(study2.advisorScores)
study2.actualCorrs[study2.actualCorrs==1] = NA
mean(study2.actualCorrs,na.rm=TRUE)
rObs.Study2 <- mean(study2.actualCorrs,na.rm=TRUE)

N <- 1000
study2.permutationCorrs <- vector("double", N)
for (iter in 1:N) {
  sampleData <- data.frame(lapply(study2.advisorScores, sample))
  sampleCorrs <- cor(sampleData)
  sampleCorrs[sampleCorrs==1] = NA
  study2.permutationCorrs[[iter]] <- mean(sampleCorrs,na.rm=TRUE)
}

mean(study2.permutationCorrs)
sd(study2.permutationCorrs)

study2.rObs.p_value <- sum(study2.permutationCorrs >= rObs.Study2) / N
study2.rObs.p_value

# Darts Permutation Test (Study S1)

studyS1.advisorScores <- studyS1.advisors.wide[,c(advisorThrows)]
studyS1.actualCorrs <- cor(studyS1.advisorScores)
studyS1.actualCorrs[studyS1.actualCorrs==1] = NA
mean(studyS1.actualCorrs,na.rm=TRUE)
rObs.StudyS1 <- mean(studyS1.actualCorrs,na.rm=TRUE)
rObs.StudyS1

N <- 1000
studyS1.permutationCorrs <- vector("double", N)
for (iter in 1:N) {
  sampleData <- data.frame(lapply(studyS1.advisorScores, sample))
  sampleCorrs <- cor(sampleData)
  sampleCorrs[sampleCorrs==1] = NA
  studyS1.permutationCorrs[[iter]] <- mean(sampleCorrs,na.rm=TRUE)
}

studyS1.rObs.p_value <- sum(studyS1.permutationCorrs >= rObs.StudyS1) / N
studyS1.rObs.p_value

mean(studyS1.permutationCorrs)
sd(studyS1.permutationCorrs)

# Colorlink Permutation Test (Study S2)

studyS2.advisorBoards <- names(studyS2.advisors.wide[,
                                                     (grepl("_c$", names(studyS2.advisors.wide)))])[-1:-2] 

studyS2.advisorScores <- studyS2.advisors.wide[,c(studyS2.advisorBoards)]
studyS2.actualCorrs <- cor(studyS2.advisorScores)
studyS2.actualCorrs[studyS2.actualCorrs==1] = NA
mean(studyS2.actualCorrs,na.rm=TRUE)
rObs.StudyS2 <- mean(studyS2.actualCorrs,na.rm=TRUE)

N <- 1000
studyS2.permutationCorrs <- vector("double", N)
for (iter in 1:N) {
  sampleData <- data.frame(lapply(studyS2.advisorScores, sample))
  sampleCorrs <- cor(sampleData)
  sampleCorrs[sampleCorrs==1] = NA
  studyS2.permutationCorrs[[iter]] <- mean(sampleCorrs,na.rm=TRUE)
}

studyS2.rObs.p_value <- sum(studyS2.permutationCorrs >= rObs.StudyS2) / N
studyS2.rObs.p_value

mean(studyS2.permutationCorrs)
sd(studyS2.permutationCorrs)

# Create plot for all task permutation tests

study2.permutationData <- as.data.frame(cbind("Word Scramble",
                                              study2.permutationCorrs,rObs.Study2))
studyS1.permutationData <- as.data.frame(cbind("Darts",
                                               studyS1.permutationCorrs,rObs.StudyS1))
studyS2.permutationData <- as.data.frame(cbind("Colorlink",
                                               studyS2.permutationCorrs,rObs.StudyS2))

names(study2.permutationData) <- c("task","simCorr","rObs")
names(studyS1.permutationData) <- c("task","simCorr","rObs")
names(studyS2.permutationData) <- c("task","simCorr","rObs")

allTasks.permutationData <- rbind(study2.permutationData,studyS1.permutationData,
                                  studyS2.permutationData)
str(allTasks.permutationData)
allTasks.permutationData$simCorr <- as.numeric(allTasks.permutationData$simCorr)
allTasks.permutationData$rObs <- as.numeric(allTasks.permutationData$rObs)

str(allTasks.permutationData)
allTasks.permutationData$task <- factor(allTasks.permutationData$task)
allTasks.permutationData$task <- relevel(allTasks.permutationData$task,
                                         "Word Scramble")

figureS4 <- ggplot(allTasks.permutationData,aes(x=simCorr)) + 
  geom_histogram(color="black",fill="white",bins=100) +
  facet_grid(rows=vars(task)) +
  geom_vline(aes(xintercept = rObs), linetype="dotted", 
             color = "black", size=1) +
  lims(x=c(-.1,.9)) +
  labs(x="Average Inter-item Correlation",
       y="Count in 1000 Simulations") +
  theme_journal() + 
  NULL
figureS4


# 9) Analyses of Curvilinear Relationships in Studies 2, S1, and S2 -------

#  This section uses code from Uri Simonsohn's Two-Lines test 
#  In the paper "Two-Lines: A Valid Alternative to the Invalid Testing of U-Shaped 
#  Relationships with Quadratic Regressions"

source("twolines.R") # install two-Lines test functions and dependencies

# Study 2
study2.twolines=twolines(pdiffmean~advisor_mean,data=study2.advisees_wide)

# Study S1
studyS1.twolines=twolines(meanprepost~advisormean,data=studyS1.advisees.wide)

# Study S2
studyS2.twolines=twolines(pdiffmean~advisor_board_mean,data=studyS2.advisees.wide)
studyS2.twolines.noOutlier=twolines(pdiffmean~advisor_board_mean,data=studyS2.advisees.wide
                                    [studyS2.advisees.wide$advisor_board_mean<140,]) # Same test without extreme outlier


# 10) Reliability of helpfulness and improvement measures ------------------

# CORRELATIONS BETWEEN HELPFULNESS AND IMPROVEMENT

# Study 2
cor.test(study2.advisees_wide$adv_helpf,study2.advisees_wide$adv_imp_1)

# Study S1
cor.test(studyS1.advisees.wide$adv_helpf,studyS1.advisees.wide$advice_imp)

# Study S2
cor.test(studyS2.advisees.wide$adv_help,studyS2.advisees.wide$adv_eff_1)

# ICC in Study 3
psych::alpha(study3.ratings_wide,n.iter=500)

# Advice permutation tests for helpfulness/improvement variance

# Word Scramble Permutation Test (Study 2) for variance

study2.adviceRatings <- study2.advisees_wide[,c("advisor","participant","adv_helpf",
                                                "adv_imp_1")] # get advisee ratings of advice
study2.adviceRatings <- study2.adviceRatings[
  !study2.adviceRatings$advisor == "ad00",] # remove control trials

study2.adviceRatings <- ddply(study2.adviceRatings, .(advisor), 
                              mutate, numRating = seq_along(adv_helpf)) # add row dummy

study2.adviceRatings_helpf <- dcast(study2.adviceRatings, advisor ~ numRating, 
                                    value.var="adv_helpf") # long to wide (helpfulness)
study2.adviceRatings_imp <- dcast(study2.adviceRatings, advisor ~ numRating, 
                                  value.var="adv_imp_1") # long to wide (improvement)

# Study 2 Helpfulness Permutation Test for Variance
study2.advisorScores_helpf <- study2.adviceRatings_helpf[-1]
study2.helpf.actualVar <- apply(study2.advisorScores_helpf,1,FUN=std.error)
mean(study2.helpf.actualVar)
rObs.Study2_helpf <- mean(study2.helpf.actualVar)

N <- 1000
study2.permutationVar_helpf <- vector("double", N)
for (iter in 1:N) {
  sampleData <- data.frame(lapply(study2.advisorScores_helpf, sample))
  sampleVar <- apply(sampleData,1,FUN=std.error)
  study2.permutationVar_helpf[[iter]] <- mean(sampleVar)
}

mean(study2.permutationVar_helpf)
sd(study2.permutationVar_helpf)

study2.rObs.helpf.p_value <- sum(study2.permutationVar_helpf <= rObs.Study2_helpf) / N
study2.rObs.helpf.p_value

# Study 2 Improvement Permutation Test for Variance

study2.advisorScores_imp <- study2.adviceRatings_imp[-1]
study2.imp.actualVar <- apply(study2.advisorScores_imp,1,FUN=std.error)
mean(study2.imp.actualVar) 
rObs.Study2_imp <- mean(study2.imp.actualVar)

N <- 1000
study2.permutationVar_imp <- vector("double", N)
for (iter in 1:N) {
  sampleData <- data.frame(lapply(study2.advisorScores_imp, sample))
  sampleVar <- apply(sampleData,1,FUN=std.error)
  study2.permutationVar_imp[[iter]] <- mean(sampleVar)
}

mean(study2.permutationVar_imp)
sd(study2.permutationVar_imp)

study2.rObs.imp.p_value <- sum(study2.permutationVar_imp <= rObs.Study2_imp) / N
study2.rObs.imp.p_value

# Study S1
studyS1.adviceRatings <- studyS1.advisees.wide[,c("advicecode","participant","adv_helpf",
                                                  "advice_imp")] # get advisee ratings of advice
studyS1.adviceRatings <- studyS1.adviceRatings[
  !studyS1.adviceRatings$advicecode == "0",] # remove control trials

studyS1.adviceRatings <- ddply(studyS1.adviceRatings, .(advicecode), 
                               mutate, numRating = seq_along(adv_helpf)) # add row dummy

studyS1.adviceRatings_helpf <- dcast(studyS1.adviceRatings, advicecode ~ numRating, 
                                     value.var="adv_helpf") # long to wide (helpfulness)
studyS1.adviceRatings_imp <- dcast(studyS1.adviceRatings, advicecode ~ numRating, 
                                   value.var="advice_imp") # long to wide (improvement)

# Study S1 Helpfulness Permutation Test for Variance
studyS1.advisorScores_helpf <- studyS1.adviceRatings_helpf[-1]
studyS1.helpf.actualVar <- apply(studyS1.advisorScores_helpf,1,FUN=std.error)
mean(studyS1.helpf.actualVar,na.rm=TRUE)
rObs.studyS1_helpf <- mean(studyS1.helpf.actualVar,na.rm=TRUE)

N <- 1000
studyS1.permutationVar_helpf <- vector("double", N)
for (iter in 1:N) {
  sampleData <- data.frame(lapply(studyS1.advisorScores_helpf, sample))
  sampleVar <- apply(sampleData,1,FUN=std.error)
  studyS1.permutationVar_helpf[[iter]] <- mean(sampleVar,na.rm=TRUE)
}

mean(studyS1.permutationVar_helpf)
sd(studyS1.permutationVar_helpf)

studyS1.rObs.helpf.p_value <- sum(studyS1.permutationVar_helpf <= rObs.studyS1_helpf) / N
studyS1.rObs.helpf.p_value

# Study S1 Improvement Permutation Test for Variance
studyS1.advisorScores_imp <- studyS1.adviceRatings_imp[-1]
studyS1.imp.actualVar <- apply(studyS1.advisorScores_imp,1,FUN=std.error)
mean(studyS1.imp.actualVar,na.rm=TRUE)
rObs.studyS1_imp <- mean(studyS1.imp.actualVar,na.rm=TRUE)

N <- 1000
studyS1.permutationVar_imp <- vector("double", N)
for (iter in 1:N) {
  sampleData <- data.frame(lapply(studyS1.advisorScores_imp, sample))
  sampleVar <- apply(sampleData,1,FUN=std.error)
  studyS1.permutationVar_imp[[iter]] <- mean(sampleVar,na.rm=TRUE)
}

mean(studyS1.permutationVar_imp)
sd(studyS1.permutationVar_imp)

studyS1.rObs.imp.p_value <- sum(studyS1.permutationVar_imp <= rObs.studyS1_imp) / N
studyS1.rObs.imp.p_value

# Study S2
studyS2.adviceRatings <- studyS2.advisees.wide[,c("advisor","participant","adv_help",
                                                  "adv_eff_1")] # get advisee ratings of advice
studyS2.adviceRatings <- studyS2.adviceRatings[
  !studyS2.adviceRatings$advisor == "ap00",] # remove control trials

studyS2.adviceRatings <- ddply(studyS2.adviceRatings, .(advisor), 
                               mutate, numRating = seq_along(adv_help)) # add row dummy

studyS2.adviceRatings_helpf <- dcast(studyS2.adviceRatings, advisor ~ numRating, 
                                     value.var="adv_help") # long to wide (helpfulness)
studyS2.adviceRatings_imp <- dcast(studyS2.adviceRatings, advisor ~ numRating, 
                                   value.var="adv_eff_1") # long to wide (improvement)

# Study S2 Helpfulness Permutation Test for Variance
studyS2.advisorScores_helpf <- studyS2.adviceRatings_helpf[-1]
studyS2.helpf.actualVar <- apply(studyS2.advisorScores_helpf,1,FUN=std.error)
mean(studyS2.helpf.actualVar,na.rm=TRUE)
rObs.studyS2_helpf <- mean(studyS2.helpf.actualVar,na.rm=TRUE)

N <- 1000
studyS2.permutationVar_helpf <- vector("double", N)
for (iter in 1:N) {
  sampleData <- data.frame(lapply(studyS2.advisorScores_helpf, sample))
  sampleVar <- apply(sampleData,1,FUN=std.error)
  studyS2.permutationVar_helpf[[iter]] <- mean(sampleVar,na.rm=TRUE)
}

mean(studyS2.permutationVar_helpf)
sd(studyS2.permutationVar_helpf)

studyS2.rObs.helpf.p_value <- sum(studyS2.permutationVar_helpf <= rObs.studyS2_helpf) / N
studyS2.rObs.helpf.p_value

# Study S2 Improvement Permutation Test for Variance
studyS2.advisorScores_imp <- studyS2.adviceRatings_imp[-1]
studyS2.imp.actualVar <- apply(studyS2.advisorScores_imp,1,FUN=std.error)
mean(studyS2.imp.actualVar,na.rm=TRUE)
rObs.studyS2_imp <- mean(studyS2.imp.actualVar,na.rm=TRUE)

N <- 1000
studyS2.permutationVar_imp <- vector("double", N)
for (iter in 1:N) {
  sampleData <- data.frame(lapply(studyS2.advisorScores_imp, sample))
  sampleVar <- apply(sampleData,1,FUN=std.error)
  studyS2.permutationVar_imp[[iter]] <- mean(sampleVar,na.rm=TRUE)
}

mean(studyS2.permutationVar_imp)
sd(studyS2.permutationVar_imp)

studyS2.rObs.imp.p_value <- sum(studyS2.permutationVar_imp <= rObs.studyS2_imp) / N
studyS2.rObs.imp.p_value


# Create plot for all task permutation tests

study2.permutationData_helpf <- as.data.frame(cbind("Word Scramble",
                                                    study2.permutationVar_helpf,rObs.Study2_helpf))
studyS1.permutationData_helpf <- as.data.frame(cbind("Darts",
                                                     studyS1.permutationVar_helpf,rObs.studyS1_helpf))
studyS2.permutationData_helpf <- as.data.frame(cbind("Colorlink",
                                                     studyS2.permutationVar_helpf,rObs.studyS2_helpf))

names(study2.permutationData_helpf) <- c("task","simVar","rObs")
names(studyS1.permutationData_helpf) <- c("task","simVar","rObs")
names(studyS2.permutationData_helpf) <- c("task","simVar","rObs")

allTasks.permutationData_helpf <- rbind(study2.permutationData_helpf,
                                        studyS1.permutationData_helpf,
                                        studyS2.permutationData_helpf)
str(allTasks.permutationData_helpf)
allTasks.permutationData_helpf$simVar <- as.numeric(allTasks.permutationData_helpf$simVar)
allTasks.permutationData_helpf$rObs <- as.numeric(allTasks.permutationData_helpf$rObs)
str(allTasks.permutationData_helpf)

allTasks.permutationData_helpf$task <- factor(allTasks.permutationData_helpf$task)
allTasks.permutationData_helpf$task <- relevel(allTasks.permutationData_helpf$task,
                                         "Word Scramble")

figureS5 <- ggplot(allTasks.permutationData_helpf,aes(x=simVar)) + 
  geom_histogram(color="black",fill="white",bins=100) +
  facet_grid(rows=vars(task)) +
  geom_vline(aes(xintercept = rObs), linetype="dotted", 
             color = "black", size=1) +
  lims(x=c(.28,.55)) +
  labs(x="Average Intra-item Standard Error",
       y="Count in 1000 Simulations") +
  theme_journal() + 
  NULL
figureS5

study2.permutationData_imp <- as.data.frame(cbind("Word Scramble",
                                                  study2.permutationVar_imp,rObs.Study2_imp))
studyS1.permutationData_imp <- as.data.frame(cbind("Darts",
                                                   studyS1.permutationVar_imp,rObs.studyS1_imp))
studyS2.permutationData_imp <- as.data.frame(cbind("Colorlink",
                                                   studyS2.permutationVar_imp,rObs.studyS2_imp))

names(study2.permutationData_imp) <- c("task","simVar","rObs")
names(studyS1.permutationData_imp) <- c("task","simVar","rObs")
names(studyS2.permutationData_imp) <- c("task","simVar","rObs")

allTasks.permutationData_imp <- rbind(study2.permutationData_imp,
                                      studyS1.permutationData_imp,
                                      studyS2.permutationData_imp)
str(allTasks.permutationData_imp)
allTasks.permutationData_imp$simVar <- as.numeric(allTasks.permutationData_imp$simVar)
allTasks.permutationData_imp$rObs <- as.numeric(allTasks.permutationData_imp$rObs)
str(allTasks.permutationData_imp)

allTasks.permutationData_imp$task <- factor(allTasks.permutationData_imp$task)
allTasks.permutationData_imp$task <- relevel(allTasks.permutationData_imp$task,
                                               "Word Scramble")

figureS6 <- ggplot(allTasks.permutationData_imp,aes(x=simVar)) + 
  geom_histogram(color="black",fill="white",bins=100) +
  facet_grid(rows=vars(task)) +
  geom_vline(aes(xintercept = rObs), linetype="dotted", 
             color = "black", size=1) +
  lims(x=c(1,6.6)) +
  labs(x="Average Intra-item Standard Error",
       y="Count in 1000 Simulations") +
  theme_journal() +
  NULL
figureS6

# 11) Studies 2, S1 and S2 Advisor Improvement ----------------------------------

# Study 2

study2.advisors.wide$advisor_improvement <- apply(study2.advisors.wide[,c("s5",
                                                                          "s6")],
                                                  1,FUN = mean)-
  apply(study2.advisors.wide[,c("s2",
                                "s1")],
        1,FUN = mean)

study2.advisees_long <- merge(study2.advisees_long,
                              study2.advisors.wide[,c("participant","advisor_improvement")],
                              by.y="participant",
                              by.x="advisor",
                              all.x=TRUE)

study2.advisorMeans <- merge(study2.advisorMeans,
                             study2.advisors.wide[,c("participant","advisor_improvement")],
                             by.y="participant",
                             by.x="advisor",
                             all.x=TRUE)

study2.advisees_wide <- merge(study2.advisees_wide,
                              study2.advisors.wide[,c("participant","advisor_improvement")],
                              by.y="participant",
                              by.x="advisor",
                              all.x=TRUE)

study2.modelIMPa <- lmer(score ~ (boardnum + advisor_improvement)^2 + (boardnum|participant), 
                         data = study2.advisees_long)
summary(study2.modelIMPa)
print(confint(study2.modelIMPa),digits=3)

# rerun model but control for advisor mean performance
study2.modelIMPb <- lmer(score ~ (boardnum + advisor_improvement)^2 + 
                           advisor_mean + (boardnum|participant), 
                         data = study2.advisees_long)
summary(study2.modelIMPb) 
allFits(study2.modelIMPb) # diagnose convergence issues

# Study S1

studyS1.advisors.wide$advisor_improvement <- apply(studyS1.advisors.wide
                                                   [,c("advisorThrow11",
                                                       "advisorThrow12")],
                                                   1,FUN = mean)-
  apply(studyS1.advisors.wide[,c("advisorThrow1",
                                 "advisorThrow2")],
        1,FUN = mean)

studyS1.advisees.long <- merge(studyS1.advisees.long,
                               studyS1.advisors.wide[,c("advisor","advisor_improvement")],
                               by.x="advisor",
                               by.y="advisor",
                               all.x=TRUE)

studyS1.advisorMeans <- ddply(studyS1.advisees.wide,.(advisor,
                                                      advisormean),
                              summarise,
                              advisor_efficacy = mean(meanprepost),
                              .drop=TRUE)


studyS1.advisorMeans <- merge(studyS1.advisorMeans,
                              studyS1.advisors.wide[,c("advisor","advisor_improvement")],
                              by.x="advisor",
                              by.y="advisor",
                              all.x=TRUE)

studyS1.advisees.wide <- merge(studyS1.advisees.wide,
                               studyS1.advisors.wide[,c("advisor","advisor_improvement")],
                               by.y="advisor",
                               by.x="advisor",
                               all.x=TRUE)

studyS1.modelIMPa <- lmer(accuracy ~ (thrownum + advisor_improvement)^2 + 
                            (thrownum|participant), 
                          data = studyS1.advisees.long)
summary(studyS1.modelIMPa) 
allFits(studyS1.modelIMPa) # convergence failure looks ok
confint(studyS1.modelIMPa)

# rerun model but control for advisor mean performance
studyS1.modelIMPb <- lmer(accuracy ~ (thrownum + advisor_improvement)^2 + advisormean + 
                            (thrownum|participant), 
                          data = studyS1.advisees.long)  
summary(studyS1.modelIMPb) 

# Study S2

studyS2.advisors.wide$advisor_improvement <- apply(log(studyS2.advisors.wide[,c("advisor_s10",
                                                                                "advisor_s9")]),
                                                   1,FUN = mean)-
  apply(log(studyS2.advisors.wide[,c("advisor_s1",
                                     "advisor_s2")]),
        1,FUN = mean)

studyS2.advisees.long <- merge(studyS2.advisees.long,
                               studyS2.advisors.wide[,c("advisor","advisor_improvement")],
                               by.x="advisor",
                               by.y="advisor",
                               all.x=TRUE)

studyS2.advisee.modelIMPa <- lmer(log(score) ~ (boardnum + advisor_improvement)^2 + 
                                    (boardnum|participant), 
                                  data = studyS2.advisees.long)
summary(studyS2.advisee.modelIMPa) 
allFits(studyS2.advisee.modelIMPa)
confint(studyS2.advisee.modelIMPa)

studyS2.advisee.modelIMPb <- lmer(log(score) ~ (boardnum + advisor_improvement)^2 + 
                                    log(advisor_board_mean) + (boardnum|participant), 
                                  data = studyS2.advisees.long)
summary(studyS2.advisee.modelIMPb) 
allFits(studyS2.advisee.modelIMPb)

# 12) Studies S3a and S3b ----------------------------------------------------

## LOAD DATA
studyS3a.wide <- read.csv("studyS3a_data.csv",header=TRUE,na.strings="")
studyS3b.wide <- read.csv("studyS3b_data.csv",header=TRUE,na.strings="")

# remove participants who didn't finish study
studyS3a.wide <- studyS3a.wide[studyS3a.wide$Finished=="TRUE",] 
studyS3b.wide <- studyS3b.wide[studyS3b.wide$Finished=="TRUE",] 

# Study S3a sample demographics, pre exclusion
nrow(studyS3a.wide) # number of participants
count(studyS3a.wide$gender)
summary(studyS3a.wide$age)
sd(studyS3a.wide$age,na.rm=TRUE)

# Study S3b sample demographics, pre exclusion
nrow(studyS3b.wide) # number of participants
count(studyS3b.wide$gender)
summary(studyS3b.wide$age)
sd(studyS3b.wide$age,na.rm=TRUE)

# Exclude participants who didn't pass attentional catch
studyS3a.wide <- studyS3a.wide[!is.na(studyS3a.wide$catch_4_TEXT),]
studyS3b.wide <- studyS3b.wide[!is.na(studyS3b.wide$catch_4_TEXT),]

# Study S3 sample demographics, post exclusion
nrow(studyS3a.wide) # number of participants
count(studyS3a.wide$gender)
summary(studyS3a.wide$age)
sd(studyS3a.wide$age,na.rm=TRUE)

# Study S4 sample demographics, post exclusion
nrow(studyS3b.wide) # number of participants
count(studyS3b.wide$gender)
summary(studyS3b.wide$age)
sd(studyS3b.wide$age,na.rm=TRUE)

# Combine data from Studies S3 and S4 for analysis
studyS3a.wide$condition <- "advice rating"
studyS3b.wide$condition <- "perceived advisor performance"
names(studyS3b.wide) <- names(studyS3a.wide) # make column names consistent
studyS3a3b.wide <- rbind(studyS3a.wide,studyS3b.wide) # merge datsets

# Get order of trials for each study
temporderlist <- strsplit(as.character(studyS3a3b.wide$advicerating_DO),"|",fixed=TRUE)
trialdf <- as.data.frame(do.call(rbind, lapply(temporderlist, rbind)))
names(trialdf) <- c("trial1","trial2","trial3")
studyS3a3b.wide <- cbind(studyS3a3b.wide,trialdf)
rm(temporderlist,trialdf)
alladvice <- c("adv_long","adv_medium","adv_short")
trialnames <- c("trial1","trial2","trial3")
studyS3a3b.wide$firsttrial <- ifelse(studyS3a3b.wide$trial1 == "adv_short",
                                    studyS3a3b.wide$adv_short,
                                    ifelse(studyS3a3b.wide$trial1 == "adv_medium",
                                           studyS3a3b.wide$adv_medium,
                                           studyS3a3b.wide$adv_long))

# convert data from wide to long format and clean up for analysis
studyS3a3b.long <- reshape2::melt(studyS3a3b.wide,measure.vars=alladvice,
                                 variable.name="advicelength", 
                                 value.name="rating",na.rm=FALSE)
studyS3a3b.long$advicelength <- factor(studyS3a3b.long$advicelength, 
                                      levels=c("adv_short",
                                               "adv_medium",
                                               "adv_long"))
studyS3a3b.long$advicelength <- revalue(studyS3a3b.long$advicelength, 
                                       c("adv_short" = "short",
                                         "adv_medium" = "medium",
                                         "adv_long" = "long"))

# Mean advice rating for Study S3a and S3b by length
aggregate(rating ~ advicelength + condition, 
          studyS3a3b.long[studyS3a3b.long$condition=="advice rating",], mean)
aggregate(rating ~ advicelength + condition, 
          studyS3a3b.long[studyS3a3b.long$condition=="advice rating",], sd)

# Regression models

studyS3a3b.long <- within(studyS3a3b.long, advicelength <- relevel(advicelength,                                                     
                                                                 ref = "short"))
studyS3a.model1 <- lmer(rating ~ advicelength + (1|ResponseId), 
                       data=studyS3a3b.long[studyS3a3b.long$condition=="advice rating",])
summary(studyS3a.model1)
confint(studyS3a.model1)

studyS3a3b.long <- within(studyS3a3b.long, advicelength <- relevel(advicelength,                                                     
                                                                 ref = "medium"))

studyS3a.model1.releveled <- lmer(rating ~ advicelength + (1|ResponseId), 
                                 data=studyS3a3b.long[studyS3a3b.long$condition==
                                                       "advice rating",])
summary(studyS3a.model1.releveled)
confint(studyS3a.model1.releveled)

# Mean advice rating for Study S3 by length
aggregate(rating ~ advicelength + condition, 
          studyS3a3b.long[studyS3a3b.long$condition=="perceived advisor performance",], mean)
aggregate(rating ~ advicelength + condition, 
          studyS3a3b.long[studyS3a3b.long$condition=="perceived advisor performance",], sd)

# Regression models

studyS3a3b.long <- within(studyS3a3b.long, advicelength <- relevel(advicelength,                                                     
                                                                 ref = "short"))
studyS3b.model1 <- lmer(rating ~ advicelength + (1|ResponseId), 
                       data=studyS3a3b.long[studyS3a3b.long$condition==
                                             "perceived advisor performance",])
summary(studyS3b.model1)
confint(studyS3b.model1)

studyS3a3b.long <- within(studyS3a3b.long, advicelength <- relevel(advicelength,                                                     
                                                                 ref = "medium"))

studyS3b.model1.releveled <- lmer(rating ~ advicelength + (1|ResponseId), 
                                 data=studyS3a3b.long[studyS3a3b.long$condition==
                                                       "perceived advisor performance",])
summary(studyS3b.model1.releveled)
confint(studyS3b.model1.releveled)

# Analyzing first trial only

studyS3a.anova1 <- aov(firsttrial ~ trial1, data = studyS3a3b.long
                      [studyS3a3b.long$condition=="advice rating",])
summary(studyS3a.anova1)
etaSquared(studyS3a.anova1)
TukeyHSD(studyS3a.anova1)

studyS3b.anova1 <- aov(firsttrial ~ trial1, data = studyS3a3b.long
                      [studyS3a3b.long$condition=="perceived advisor performance",])
summary(studyS3b.anova1)
etaSquared(studyS3b.anova1)
TukeyHSD(studyS3b.anova1)

# means and SDs of first trial
aggregate(firsttrial ~ trial1,data=studyS3a3b.long
          [studyS3a3b.long$condition=="advice rating",],FUN=mean)
aggregate(firsttrial ~ trial1,data=studyS3a3b.long
          [studyS3a3b.long$condition=="advice rating",],FUN=sd)
aggregate(firsttrial ~ trial1,data=studyS3a3b.long
          [studyS3a3b.long$condition=="perceived advisor performance",],FUN=mean)
aggregate(firsttrial ~ trial1,data=studyS3a3b.long
          [studyS3a3b.long$condition=="perceived advisor performance",],FUN=sd)

# 13) Study S4a ----------------------------------

# Import study 2 topic coding

study2.adviceTopics.raw <- read.csv("studyS4a_advice_topics_from_study2.csv",header=TRUE)
study2.adviceTopics.raw$advisor <- factor(study2.adviceTopics.raw$advisor)

# merge study 2 topic coding with study 2 data
studyS4a.topicData_long <- merge(study2.advisees_long,study2.adviceTopics.raw,
                               by="advisor",all.x=TRUE)
studyS4a.topicData_long <- studyS4a.topicData_long[studyS4a.topicData_long$
                                                 condition=="treatment",]

# advisee improvement dummy coding

studyS4a.topicData_long$n_lettersDummy <- ifelse(studyS4a.topicData_long$
                                                 n_letters == 0,0,1)
studyS4a.topicData_long$n_mechanicsDummy <- ifelse(studyS4a.topicData_long$
                                                   n_mechanics == 0,0,1)
studyS4a.topicData_long$n_spatialDummy <- ifelse(studyS4a.topicData_long$
                                                 n_spatial == 0,0,1)
studyS4a.topicData_long$n_speedPacingMindsetDummy <- ifelse(studyS4a.topicData_long$
                                                            n_speedPacingMindset == 0,0,1)
studyS4a.topicData_long$n_wordLengthDummy <- ifelse(studyS4a.topicData_long$
                                                    n_wordLength == 0,0,1)
studyS4a.topicData_long$n_wordsDummy <- ifelse(studyS4a.topicData_long$
                                               n_words == 0,0,1)

studyS4a.model1a <- lmer(score ~ boardnum +
                                n_lettersDummy + n_mechanicsDummy + n_spatialDummy +
                                n_speedPacingMindsetDummy + n_wordLengthDummy +
                                n_wordsDummy +
                                (boardnum|participant), 
                              data = studyS4a.topicData_long)
summary(studyS4a.model1a)
confint(studyS4a.model1a)
r2_nakagawa(studyS4a.model1a)

# model without random slopes
studyS4a.model1b <- lmer(score ~ boardnum + 
                                    n_lettersDummy + n_mechanicsDummy + n_spatialDummy +
                                    n_speedPacingMindsetDummy + n_wordLengthDummy +
                                    n_wordsDummy +
                                    (1|participant), 
                                  data = studyS4a.topicData_long)
summary(studyS4a.model1b)
anova(studyS4a.model1b,studyS4a.model1a)

# model without random intercepts
studyS4a.model1c <- lmer(score ~ boardnum + 
                                    n_lettersDummy + n_mechanicsDummy + n_spatialDummy +
                                    n_speedPacingMindsetDummy + n_wordLengthDummy +
                                    n_wordsDummy +
                                    (boardnum - 1|participant), 
                        data = studyS4a.topicData_long)
summary(studyS4a.model1c)
anova(studyS4a.model1c,studyS4a.model1a)$Chisq

# model without topics
studyS4a.model1d <- lmer(score ~ boardnum +
                          (boardnum|participant), 
                        data = studyS4a.topicData_long)
summary(studyS4a.model1d)
anova(studyS4a.model1a,studyS4a.model1d)

# 14) Study S4b --------------------------------

## LOAD AND CLEAN UP DATA

studyS4b_wide <- read.csv("studyS4b_data.csv") # read in data from CSV file
studyS4b_wide$condition <- factor(studyS4b_wide$condition)
allboards <- names(studyS4b_wide[,(grepl(
  "_c$", names(studyS4b_wide)))]) # make list of all rounds except practice
ordervars <- c("s1","s2","s3","s4","s5","s6") # make list of the order of boards
postboards <- allboards[-1] # list of all rounds except pre-advice round

# Convert survey data from wide to long format
studyS4b_long <- melt(studyS4b_wide,measure.vars=allboards,
                    variable.name="board", value.name="score",na.rm=TRUE)
studyS4b_long <- studyS4b_long[order(studyS4b_long$participant),]
studyS4b_long$boardnum <- as.numeric(studyS4b_long$board)

# Calculate total words found, etc 
studyS4b_long <- ddply(studyS4b_long,.(participant),mutate,
                     totalWordsFound = sum(score),
                     board_mean = mean(score),
                     board_median = median(score),
                     .drop=FALSE)
studyS4b_long$preOrPost <- ifelse(studyS4b_long$boardnum == 1, "preAdvice","postAdvice")

studyS4b_long <- ddply(studyS4b_long,.(participant),mutate,
                     baseline = mean(score[preOrPost=="preAdvice"]),
                     pmean = mean(score[preOrPost=="postAdvice"]),
                     pmedian = median(score[preOrPost=="postAdvice"]),
                     .drop=FALSE)
studyS4b_long <- ddply(studyS4b_long,.(participant),mutate,
                     pdiffmean = pmean-baseline,
                     pdiffmedian = pmedian-baseline,
                     scoreMinusBaseline = score - baseline,
                     .drop=FALSE)

# Put summary score stats back into wide dataframe
length(unique(studyS4b_long$participant))
studyS4b_wide <- merge(studyS4b_wide,studyS4b_long[studyS4b_long$boardnum==1,
                                             c("participant","baseline",
                                               "pmean","pmedian",
                                               "pdiffmean","pdiffmedian",
                                               "scoreMinusBaseline")],
                     by="participant",all.x=TRUE,all.y=FALSE)


# Figure out which participants have missing values
sum(is.na(studyS4b_long$score)) # no boards with missing scores
studyS4b_long <- ddply(studyS4b_long,.(participant),mutate,
                     howManyRounds = length(boardnum),
                     numZeroBoards = length(score[score==0]),
                     .drop=FALSE)
count(studyS4b_long$howManyRounds)
count(studyS4b_long$numZeroBoards)

studyS4b_long <- studyS4b_long[studyS4b_long$howManyRounds == 6,] # remove pps with missing values
studyS4b_wide <- studyS4b_wide[studyS4b_wide$participant %in% studyS4b_long$participant,]

studyS4b_wide_scores <- dcast(studyS4b_long, participant ~ board, value.var="score")


### Demographics and exclusions

# Pre-exclusion demographics, All Conditions
nrow(studyS4b_wide)
summary(studyS4b_wide$age)
sd(studyS4b_wide$age,na.rm=TRUE)
count(studyS4b_wide$gender)
summary(studyS4b_wide$Duration..in.seconds.)/60
count(studyS4b_wide$condition)

# Exclusions

studyS4b_long <- studyS4b_long[studyS4b_long$totalWordsFound > 0,] # remove pps with 0 words found
studyS4b_wide <- studyS4b_wide[studyS4b_wide$participant %in% studyS4b_long$participant,]

# Post-exclusion demographics, All Conditions
nrow(studyS4b_wide)
summary(studyS4b_wide$age)
sd(studyS4b_wide$age,na.rm=TRUE)
count(studyS4b_wide$gender)
nrow(studyS4b_wide)
summary(studyS4b_wide$Duration..in.seconds.)/60

# Post-exclusion demographics by condition
count(studyS4b_wide[studyS4b_wide$condition=="goodadvice",]$gender)
summary(studyS4b_wide[studyS4b_wide$condition=="goodadvice",]$age)
sd(studyS4b_wide[studyS4b_wide$condition=="goodadvice",]$age)

count(studyS4b_wide[studyS4b_wide$condition=="badadvice",]$gender)
summary(studyS4b_wide[studyS4b_wide$condition=="badadvice",]$age)
sd(studyS4b_wide[studyS4b_wide$condition=="badadvice",]$age)

count(studyS4b_wide[studyS4b_wide$condition=="control",]$gender)
summary(studyS4b_wide[studyS4b_wide$condition=="control",]$age)
sd(studyS4b_wide[studyS4b_wide$condition=="control",]$age)

count(studyS4b_wide$condition)
count(studyS4b_wide_all$condition)

### MODELS

# set reference condition for regression
studyS4b_long <- within(studyS4b_long, condition <- relevel(condition,                                                     
                                                          ref = "goodadvice"))

studyS4b.model1a <- lmer(score ~ (boardnum + condition)^2 + (1|participant), 
                        data = studyS4b_long,REML=FALSE)
summary(studyS4b.model1a)
confint(studyS4b.model1a)

studyS4b.model1b <- lmer(score ~ boardnum + condition + (1|participant), 
                        data = studyS4b_long,REML=FALSE)
summary(studyS4b.model1b)
anova(studyS4b.model1a,studyS4b.model1b)

# relevel model to compare other two condition
studyS4b_long <- within(studyS4b_long, condition <- relevel(condition,                                                     
                                                          ref = "badadvice"))
studyS4b.model1a.releveled1 <- lmer(score ~ (boardnum + condition)^2 + (1|participant), 
                        data = studyS4b_long,REML=FALSE)
summary(studyS4b.model1a.releveled1)
confint(studyS4b.model1a.releveled1)

# relevel model to compare other two condition
studyS4b_long <- within(studyS4b_long, condition <- relevel(condition,                                                     
                                                          ref = "control"))
studyS4b.model1a.releveled2 <- lmer(score ~ (boardnum + condition)^2 + (1|participant), 
                                   data = studyS4b_long,REML=FALSE)
summary(studyS4b.model1a.releveled2)
confint(studyS4b.model1a.releveled2)

# 15) Generate figure images --------------------------------------------------

ggsave("LevariFig1.pdf",figure1,w=6,h=3)
ggsave("LevariFig2.pdf",figure2,w=6,h=3)
ggsave("LevariFig3.pdf",figure3,w=6,h=3)
ggsave("LevariFig4.pdf",figure4,w=6,h=3)
ggsave("LevariFig5.pdf",figure5,w=6,h=3)
ggsave("LevariFig6.pdf",figure6,w=6,h=3)
ggsave("LevariFig8.pdf",figure8,w=6,h=3)
ggsave("LevariFigS1.pdf",figureS1,w=6,h=3)
ggsave("LevariFigS3.pdf",figureS3,w=6,h=3)
ggsave("LevariFigS4.pdf",figureS4,w=6,h=5)
ggsave("LevariFigS5.pdf",figureS5,w=6,h=5)
ggsave("LevariFigS6.pdf",figureS6,w=6,h=5)


library(ggplot2)
library(ggpubr) # need this for ggboxplot()
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(lme4)

setwd("/Users/morganmoyer/Dropbox/Moyer_research/Embedded_Questions/Dissertation/Experiments/Reaction_time/results/")
source("../../helpers.R")


bn <- read.csv("bottnov.csv")
nrow(bn) #35028
#################################################################################
# Remove the people who participated twice
table(bn$ID,bn$SentType)
# Subjects who participanted twice (from CB):
# 19723, G< E, 216
# 19489, P < H, 54
# 18670, J < R, 54
# Remove the second participation
# Find which Group is the second participation
View(subset(bn, bn$ID=="18670"))

# Make a subset for each participant/group pair 
rem1 = subset(bn, (bn$ID=="19723" & bn$Group=="E"))
rem2 = subset(bn, (bn$ID=="19489" & bn$Group=="H"))
rem3 = subset(bn, (bn$ID=="18670" & bn$Group=="R"))
# combine them all together
to_remove = rbind(rem1, rem2, rem3)
# remove these with anti_join()
cv = anti_join(bn,to_remove)
nrow(rem3)
# Length before removing = 13428
# Length after removing should be 13428 - (3*108) = 13104
# double check to make sure
table(cv$ID,cv$SentType)
table(cv$ID,cv$Group)


# NEED TO REMOVE THESE SUBJECTS FOR NON-NATIVE SPEAKER STATUS
# 7380	Punjabi
# vm407	English and Malayalm
# 19783	Tamil
# 16657	Hindi
# 19723	Arabic
# 19822	ENGLISH%2C MALAYALAM
# 19816	English%2C Malayalam
# 17128	Russian
# 19723	Arabic
# 18283	Chinese
# A30KYQGABO7JER	Turkish



#################################################################################
#################################################################################
View(cv)
# Factors of interest: Verb, Embedded, SentType


bn_rt = cv %>%  
  filter(Type %in% c("test") & (Parameter == "Selection" | Value == "Start")) %>%
  # mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  select(-c("Time.results.were.received","MD5.hash.of.participant.s.IP.address","Controller.name","Group.Ibex","PennElementType","PennElementName")) %>%
  # mutate(Accurate = rep(Value[Parameter=="Selection"]==gsub("No-s","two", gsub("-s", "one", Ending[Parameter=="Selection"])), each=2)) %>%
  group_by(ID,SentType,Verb,Token,SentNumber,Group) %>%
  summarise( RT = mean(EventTime[Parameter=="Selection"] - EventTime[Value=="Start"]), N = length(Value)/2 ) #Value = Value[Parameter=="Selection"], 
# OK this for some reason won't allow that extra step so we have to create a second DF to get the value of Value for each subject and trial

bn_vals = cv %>%
  filter(Type %in% c("test") & (Parameter == "Selection")) %>%
  select(-c("Time.results.were.received","MD5.hash.of.participant.s.IP.address","Controller.name","Group.Ibex","PennElementType","PennElementName")) %>%
  select(ID,SentType,SentNumber,Verb, Embedded, Token,order,goal,study,Value)

b = inner_join(bn_rt, bn_vals)
nrow(b) # 8640


#################################################################################
# GRAPHS before removing outliers
bn_agr = b %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(SentType,Verb) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)

ggplot(bn_agr, aes(SentType,y=Proportion_Agree,fill=Verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # facet_wrap(~WhichQuestion) +
  # scale_fill_grey() +
  ggtitle(label = "Bott & Noveck") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

length(unique(b$ID)) #240 subjects
#################################################################################
# RT graphs
# Wow, you can see there's one really really long outlierfor T3 no verb

# Box and Whiskters
ggplot(b, aes(x=SentType, y=RT, fill=Value)) +
  facet_wrap(~Verb) +
  stat_boxplot(geom = 'errorbar')+
  geom_boxplot()

# Aggregate for bargraph
bn_RT_agr = b %>%
  group_by(SentType,Value) %>%
  summarise(mean_RT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = mean_RT - CILow, YMax = mean_RT + CIHigh) %>%
  mutate(., Value = recode(Value, "1" = "Agree", "0" = "Disagree"))
dodge = position_dodge(.9)
# bn_RT_agr = bn_RT_agr %>% mutate(., Value = recode(Value, Agree = 1, Disagree = 0))

# View(bn_RT_agr)
ggplot(bn_RT_agr, aes(SentType,y=mean_RT,fill=Value)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # facet_wrap(~Verb) +
  # scale_fill_grey() +
  ggtitle(label = "Bott & Noveck") +
  theme(plot.title = element_text(hjust = 0.5))



#################################################################################

# Take a look at summary stats
plot(density(b$RT))
h<-hist(b$RT, breaks=100, col="red", xlab="RT (ms)",
        main="Histogram with Normal Curve")

mean(b$RT) #1274.588
sd(b$RT) #10908.72

summary(b$RT)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0     473.0     764.5    1274.6    1301.0 1000000.0 
range(b$RT)
# [1] 0e+00 1e+06

# Then, calculate the quantile in whatever way you want.
# Below is for 3* the quantile, 3* the interquartile range.
# You will have to look up wheter you want this or whether
# you want 2 STDV, there are different philosophies
# If you do STDV, first remove the extreme values because the STDV is 
# very sensitive to that. But you need some principled way to determine
# what is in principle an "extreme value"
# For the IQR you just look at the median and don't remove the extremes.
# Check out Ratcliff (1993)
quantile(b$RT)[4] + IQR(b$RT)*3
# 75% 
# 3785 

# Cleaning up the RT data
# REMOVE RESPONSES LONGER THAN 10s and faster than .1s
bs <- subset(b, RT < 10000)
bs <- subset(bs, RT > 100)
100-(nrow(bs)/nrow(b)*100) # 3.75% of data removed
##################################################################
# Take a look at those subjects behaving badly on control items
# TRUE CONTROL: 
# Sentence T2, "some birds are eagles"
# T4: "All eagles are birds"
t2 <- subset(bs, bs$SentType == "T2" | bs$SentType == "T4")
plot(density(t2$RT))
h<-hist(t2$RT, breaks=100, col="red", xlab="RT (ms)",
        main="Histogram with Normal Curve")
summary(t2$RT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 103     520     864    1127    1363    9970 

# Take a look at all the subjects
t2_agr = bs %>%
  group_by(ID,Value,Verb) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

# it's too big to see even on zoom
ggplot(t2_agr, aes(x=Value, y=meanRT, fill=reorder(ID,meanRT))) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Verb) +
  geom_bar(position=dodge,stat="identity")

quantile(t2$RT)[4] + IQR(t2$RT)*3 #4552  (for T2 only)
quantile(t2$RT)[4] + IQR(t2$RT)*3 #4888  (for T2 & T4)
# but look at the graph of subject RT and 5000 seems like a better cut-off
t2_sub <- subset(t2, t2$RT > 4552  )
View(t2_sub) 
length(unique(t2_sub$ID)) #18 subjects!!!!

t2_sub <- subset(t2, t2$RT > 4888)
View(unique(t2_sub$ID)) #21
# 16753: really long on T6, over 10s, T4 is close to 5s
# 2	17128: really long on T2(none), T3(non), T2(say)
# 3	17233: T5 (know) also long
# 4	17410: just T4
# 5	18169: long on T2, T4...looks like really long for at least 1/3 appearances of a controls
# 6	18265: Long on T2(none,say)m T4 (say)

# 7	18271***** Disagreeing on True controls....Rejection bias??

# 8	18277: just long on T2 (know), will be removed with cutoff; 
      # maybe long on T4 (say), RT> 5s
# 9	18664: long on  T2 (none), T5 (none and say)
# 10	18670: long on T1 (know), T4 (say)
# 11	18730: julst long on T2(say)
# 12	18781: long on T4 (say)

# 13	19087 *** missing several trials for this participant, 
            # took over 15m on T2 (know), T4 (say)

# 14	19252: long on T3 (know), T4 (non)
# 15	19441: T4 (know)
# 16	19474: T4 (know), T3 (none),  
# 17	19654: T1 (say), T2 (say), T4 (say)

# 18	19723:**** maybe remove for controls?
      # T1(know), T2 (say), T3 (none), T6 (none)

# 19	19750: a little long but consistent (T2 say; T5 know)
# 20	19819: some long responses will be removed with cutoff
          # T1 (say), T2 (know)
# 21	19822

# Use the code below to just substitiute in the relevant subject ID
a <- subset(b, b$ID == "19822")
a_agr = a %>%
  group_by(SentType,Value,Verb) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(a_agr, aes(x=SentType, y=meanRT, fill=Value)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Verb) +
  geom_bar(position=dodge,stat="identity")

# Look at the individual items
ggplot(a, aes(x=as.factor(SentNumber), y=RT, fill=Value)) +
  facet_grid(Verb~SentType) +
  geom_bar(position=dodge,stat="identity")


at1 <- subset(a, a$SentType == "T1")
summary(at1$RT)

hist(t1$RT, breaks=20, col="red", xlab="RT (ms)",
     main="Histogram with Normal Curve")
View(at1)
ggplot(at1, aes(x=as.factor(SentNumber), y=RT, fill=Value)) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Verb) +
  geom_bar(position=dodge,stat="identity")

####################################
####################################
# STDV after extremes:
####################################
# remove responses RT > 2*STDV: 
mean(bs$RT) + 2*sd(bs$RT) # 3629.416
ggdensity(subset(b, b$RT < 3629.416)$RT)
c1 <- subset(b, RT < 3629.416)
100 -(nrow(c1)/nrow(b)*100) # removes about 6% of the data

# remove responses RT > 3*STDV: 
mean(bs$RT) + 3*sd(bs$RT) # 4817.922
ggdensity(subset(b, b$RT < 4817.922)$RT)
c2 <- subset(b, RT < 4817.922)
100 -(nrow(c2)/nrow(b)*100) # removes about 3% of the data

####################################
# IQR before extremes:
####################################
# Remove RT > 2*IQR
quantile(b$RT)[4] + IQR(b$RT)*2 #3732.75 
ggdensity(subset(b, b$RT < 3732.75 )$RT)
c3 <- subset(b, RT < 3732.75 )
100 -(nrow(c3)/nrow(b)*100) # Removes about 5.7%

# Remove RT > 3*IQR
quantile(b$RT)[4] + IQR(b$RT)*3 # 4804 
ggdensity(subset(b, b$RT < 4804 )$RT)
c4 <- subset(b, RT < 4804 )
100 -(nrow(c4)/nrow(b)*100) # Removes about 3%

########################################################################
########################################################################
# REMOVE OUTLIERS

# set the upperbound according to BN
bb <- subset(b, RT < 6000)
# set the lower bound:
# Removing the really fast responses makes order significant...
# don't remove them for now
bb <- subset(bb, RT > 200)
bb["logRT"] <- log(bb$RT)
100-(nrow(bb)/nrow(b)*100) # removed 5.86% of the data

####################################
# Choice Proportions
# Better not to use the df with error_trials removed
bn_agr = bb %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(SentType) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)

ggplot(bn_agr, aes(SentType,y=Proportion_Agree)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # facet_wrap(~WhichQuestion) +
  # scale_fill_grey() +
  ggtitle(label = "Bott & Noveck") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

library(ggpubr)
library(tidyverse)
library(ggpubr)
library(rstatix)


kruskal.test(Value ~ SentType, data = bb) # ***
# Kruskal-Wallis chi-squared = 4666.8, df = 5, p-value < 2.2e-16

summary(aov(as.numeric(Value)~SentType, data = bb)) # ***

kruskal.test(Value ~ Verb, data = bb) # NS
summary(aov(as.numeric(Value) ~ Verb, data = bb)) # NS

bb_t1 <- subset(bb, bb$SentType == "T1")
kruskal.test(Value ~ Verb, data = bb_t1) # NS
kruskal.test(Value ~ RT, data = bb_t1) # *


controls <- subset(bb, bb$SentType != "T1")
tc <- subset(bb, bb$SentType=="T2" | bb$SentType=="T4")
tc <- tc %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0))
fc <- subset(bb, bb$SentType=="T3" | bb$SentType=="T5" | bb$SentType=="T6")
fc <- fc %>%
  mutate(., Value = recode(Value, Agree = 0, Disagree = 1))

conts <- rbind(tc,fc)
cont_agr = conts %>%
  group_by(ID) %>%
  summarise(Proportion_correct = mean(Value))
View(cont_agr)

bads <- subset(cont_agr,cont_agr$Proportion_correct <.775)

nrow(bads) # 28
length(unique(bads$ID))
# 17410 18271 18493 18736

nrow(bads_tc) #17
nrow(bads_fc) # 30
# fc <- anti_join(tc,bb)

View(bads_tc)
ggplot(cont_agr, aes(SentType,y=Proportion_Agree)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  ggtitle(label = "Bott & Noveck Controls") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


####################################
# Remove Error Trials:
# Agree on T3, T5, T6
# Disagree on T2, T4
remT3 = subset(bb, (bb$SentType=="T3" & bb$Value=="Agree")) #44 rows
remT5 = subset(bb, (bb$SentType=="T5" & bb$Value=="Agree")) #55 rows
remT6 = subset(bb, (bb$SentType=="T6" & bb$Value=="Agree")) #37 rows
remT2 = subset(bb, (bb$SentType=="T2" & bb$Value=="Disagree")) #46
remT4 = subset(bb, (bb$SentType=="T4" & bb$Value=="Disagree")) #41

error_trials <- rbind(remT3,remT5,remT6,remT4,remT2)
(nrow(error_trials)/nrow(bb)*100) # removes 8%

nrow(bb)-nrow(error_trials) # = 7469
bv = anti_join(bb,error_trials)
# sanity check
nrow(bv) # 7469

write.csv(bv,"bv.csv")

h<-hist(bv$RT, breaks=100, col="red", xlab="RT (ms)",
        main="Histogram with Normal Curve")


#######3#######3#######3#######3#######3#######3#######3#######3
#######3#######3#######3#######3#######3#######3#######3#######3
# Looking at individual subjsect with long RTs
a <- subset(b, b$ID == "19822")
a_agr = bb %>%
  group_by(SentType,Value,Verb) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(a_agr, aes(x=SentType, y=meanRT, fill=Value)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Verb) +
  geom_bar(position=dodge,stat="identity")

# Look at the individual items
ggplot(bb, aes(x=as.factor(SentNumber), y=RT, fill=Value)) +
  facet_grid(Verb~SentType) +
  geom_bar(position=dodge,stat="identity")



# BN remove anything RT> 6s; RT< 200ms (df bb)
# BN also remove error trials for RT analysis (df bv)
# make sure that the analyses below are conducted on the df bv
#######3#######3#######3#######3#######3#######3#######3#######3
# CHECK FOR ORDER EFFECTS
# THIS IS SIGNIFICANT :()
summary(aov(RT~order, data = bv)) # ***
  #               Df    Sum Sq  Mean Sq F value   Pr(>F)    
  # order          1 3.179e+07 31793485   46.05 1.24e-11 ***
  # Residuals   7467 5.155e+09   690425 

kruskal.test(Value ~ order, data = bv) #NS

# After removing outliers?
# (Note that each of the cut-offs change sig. esp, cut off c2...)
summary(aov(RT~order, data = c2)) #*****????
summary(aov(RT~order, data = c4)) # *
kruskal.test(Value ~ order, data = c4) # NS on either one...

#######3#######3#######3#######3#######3#######3#######3#######3
#######3#######3#######3#######3#######3#######3#######3#######3

# REACTION TIME
# AGGREGATE FOR BAR GRAPH
bn_RT_agr = bv %>%
  mutate(., Value = recode(Value, "1" = "Agree", "0" = "Disagree"))%>%
  group_by(SentType,Value,Verb) %>%
  summarise(mean_RT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = mean_RT - CILow, YMax = mean_RT + CIHigh)
dodge = position_dodge(.9)
# View(bn_RT_agr)
ggplot(bn_RT_agr, aes(SentType,y=mean_RT,fill=Value)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Verb) +
  # scale_fill_grey() +
  ggtitle(label = "Bott & Noveck") +
  theme(plot.title = element_text(hjust = 0.5))

# DONT AGGREGATE DATA BEFORE PLOTTING WITH BOXPLOT
ggplot(bv, aes(x=SentType, y=RT, fill=Value)) +
  facet_wrap(~Verb) +
  stat_boxplot(geom = 'errorbar')+
  geom_boxplot()

# LOOK AT LOG_RT
ggplot(bv, aes(x=SentType, y=logRT, fill=Value)) +
  facet_wrap(~Verb) +
  stat_boxplot(geom = 'errorbar')+
  geom_boxplot()




#################################################################################
wilcox.test(RT~Value, data = bv)
summary(aov(RT ~ Value, data = bv))
  #             Df    Sum Sq Mean Sq F value   Pr(>F)    
  # Value          1 9.482e+06 9482323   13.68 0.000219 ***
  # Residuals   7467 5.178e+09  693413    

summary(aov(RT ~ Verb, data = bv)) #NS

summary(aov(RT ~ Value*Verb, data = bv)) #NS

summary(aov(RT ~ SentType, data = bv))
  #               Df    Sum Sq  Mean Sq F value Pr(>F)    
  # SentType       5 1.368e+08 27368951   40.44 <2e-16 ***
  # Residuals   7463 5.050e+09   676719  

# Can't look at interaction if we remove the error trials
summary(aov(RT ~ SentType*Value, data = bv))

##################
# T1 only

# LOOK JUST AT T1
t1_good <- subset(bv, bv$SentType == "T1")
ggplot(t1_good, aes(x=SentType, y=RT, fill=Value)) +
  facet_wrap(~Verb) +
  stat_boxplot(geom = 'errorbar')+
  geom_boxplot()

t1_agr = t1_good %>%
  mutate(., Value = recode(Value, "1" = "Agree", "0" = "Disagree"))%>%
  group_by(Value,Verb) %>%
  summarise(mean_RT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = mean_RT - CILow, YMax = mean_RT + CIHigh)
dodge = position_dodge(.9)
# View(bn_RT_agr)
ggplot(t1_agr, aes(Value,y=mean_RT,fill=Verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  ggtitle(label = "Bott & Noveck T1 SentType") +
  theme(plot.title = element_text(hjust = 0.5))



summary(aov(RT ~ Value, data = t1_good))
  #             Df    Sum Sq  Mean Sq F value Pr(>F)    
  # Value          1 6.929e+07 69288231   74.52 <2e-16 ***
  # Residuals   1316 1.224e+09   929805      

summary(aov(RT ~ Verb*Value, data = t1_good)) # NS

t.test(RT~Value, data = t1_good)
# Welch Two Sample t-test
# 
# data:  RT by Value
# t = -5.0017, df = 390.49, p-value = 8.605e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -675.1526 -294.1440
# sample estimates:
#   mean in group Agree mean in group Disagree 
# 1154.356               1639.005 



#################################################################################
# https://www.s tatmethods.net/stats/anova.html
# These are equiivalent
# res.aov <- aov(RT ~ SentType + Value + SentType:Value, data = bb)
res.aov <- aov(RT ~ SentType*Value, data = bb)
plot(res.aov)
summary(res.aov)
  #                 Df    Sum Sq  Mean Sq F value   Pr(>F)    
  # SentType          5 5.762e+07 11523408  13.769 2.48e-13 ***
  # Value             1 3.660e+06  3659880   4.373   0.0366 *  
  # SentType:Value    5 3.816e+07  7632208   9.119 1.26e-08 ***
  # Residuals      3232 2.705e+09   836919

res.aov <- aov(RT ~ Verb*SentType*Value, data = bb)
summary(res.aov)



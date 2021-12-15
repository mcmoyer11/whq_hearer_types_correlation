#################################################################################
#################################################################################
# CHEMLA AND BOTT (2013) REPLICATION
#################################################################################
#################################################################################
library(ggplot2)
library(ggpubr) # need this for ggboxplot()
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(lme4)

setwd("/Users/morganmoyer/Dropbox/Moyer_research/Embedded_Questions/Dissertation/Experiments/Reaction_time/results/")
source("../../helpers.R")

cb <- read.csv("chembott.csv")
nrow(cb) # 17388
table(cb$ID,cb$SentType)
# Subjects who participanted twice:
# 19723, G< E, 54
# 19489, G < H, 54
# 18670, A < I, 54
# Remove the second participation

# Make a subset for each participant/group pair 
rem1 = subset(cb, (cb$ID=="19723" & cb$Group=="E"))
rem2 = subset(cb, (cb$ID=="19489" & cb$Group=="H"))
rem3 = subset(cb, (cb$ID=="18670" & cb$Group=="I"))
# combine them all together
to_remove = rbind(rem1, rem2, rem3)
# remove these with anti_join()
cvv = anti_join(cb,to_remove)
nrow(cvv)# 17226
# Length before removing = 17388
# Length after removing should be 5022 - (3*54) = 4860
# double check to make sure
table(cvv$ID,cv$SentType)
table(cvv$ID,cv$Group)


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




cb_rt = cvv %>%  
  filter((Parameter == "Selection" | Value == "Start")) %>%
  # mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  select(-c("Time.results.were.received","MD5.hash.of.participant.s.IP.address","Controller.name","Group.Ibex","PennElementType","PennElementName")) %>%
  group_by(ID,SentType,Token,SentNumber,Group) %>%
  summarise( RT = mean(EventTime[Parameter=="Selection"] - EventTime[Value=="Start"]), N = length(Value)/2 ) #Value = Value[Parameter=="Selection"], 
# OK this for some reason won't allow that extra step so we have to create a second DF to get the value of Value for each subject and trial
# View(cb_rt)
cb_vals = cvv %>%
  filter(Type %in% c("test") & (Parameter == "Selection")) %>%
  select(-c("Time.results.were.received","MD5.hash.of.participant.s.IP.address","Controller.name","Group.Ibex","PennElementType","PennElementName")) %>%
  select(ID,SentType,SubStat,Negation,SentNumber,CompTruth,Matrix,order,Group,goal,study,Value)

c = inner_join(cb_rt, cb_vals)
# View(c)
c["logRT"] = log(c$RT)

nrow(c) # 8568

#################################################################################
#################################################################################
# Choice Proportions, wihtout removing any subjects

cb_agr = c %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(SentType) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)

ggplot(cb_agr, aes(SentType,y=Proportion_Agree)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # scale_fill_grey() +
  ggtitle(label = "Chemla & Bott") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


#################################################################################
#################################################################################
# C&B removed 3 participants who failedscored under 75% correct on controls
# Lok at accuracy on uncontroversial controls
# controls <- subset(c, c$SentType != "T1")
# for tcs, Agree = correct (1)
tc <- subset(c, c$SentType=="T4" | c$SentType=="T6")
tc <- tc %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) 
# for tcs, Disagree = correct (1)
fc <- subset(c, c$SentType=="T3" | c$SentType=="T8")
fc <- fc %>%
  mutate(., Value = recode(Value, Agree = 0, Disagree = 1)) 

conts <- rbind(fc, tc)
cont_agr = conts %>%
  group_by(ID) %>%
  summarise(Proportion_correct = mean(Value)) # , CILow = ci.low(Value), CIHigh = ci.high(Value)
  # mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)
View(cont_agr)


# there are 2 observations of each sentence type
# 2*4 = 8 controls
# 75\% of 8
bads <- subset(cont_agr,cont_agr$Proportion_correct <.75)
nrow(bads) #36
bads$ID
# [1]  19663         16657          16753          173004007      17374         
# [6] 17404          19573          19735          19819          A11Q8U6QTT8KGF
# [11] A18T7E73TNGOKP A1DIKLERUZ5WS0 A1EXO9HRNQYHWC A1FB5AD91CD1W0 A1FHS282JP487T
# [16] A23KAJRDVCVGOE A2DN53DIE3049T A2EZNZ6X58RTNR A2GJK2MDTHNQ6Q A2QD7QFGCUNF5N
# [21] A32JEH06T23HDF A377LTGWJKY2IW A3EQO5ZIDBTSVK A3O7X46E3REM7I A4E818Z7A5FST 
# [26] A51U5BEIC5XVR  A598UDLZZZAJ3  A7ERZELTAMWL5  A9LSEP71DNP4O  AB8KNX3LINNX4 
# [31] ACXP8KHFX06KR  AK4WZEW584BR9  AL7TCWU718E5W  ASFPEARDPRFL5  AU5BD5NSSES9H 
# [36] vm407 


length(unique(c$ID)) # 238 subjects
length(unique(bads$ID)) # 36 subjects
nrow(bads)/nrow(cont_agr)*100 # 15%

cv <- c %>%
  filter(!ID %in% c("19663", "17404","A18T7E73TNGOKP","A23KAJRDVCVGOE","A32JEH06T23HDF",
                    "A51U5BEIC5XVR","ACXP8KHFX06KR","vm407","16657","19573","A1DIKLERUZ5WS0",
                    "A2DN53DIE3049T","A377LTGWJKY2IW","A598UDLZZZAJ3","AK4WZEW584BR9",
                    "16753","19735","A1EXO9HRNQYHWC","A2EZNZ6X58RTNR","A3EQO5ZIDBTSVK",
                    "A7ERZELTAMWL5","AL7TCWU718E5W","173004007","19819","A1FB5AD91CD1W0",
                    "A2GJK2MDTHNQ6Q","A3O7X46E3REM7I","A9LSEP71DNP4O","ASFPEARDPRFL5","17374",
                    "A11Q8U6QTT8KGF","A1FHS282JP487T","A2QD7QFGCUNF5N","A4E818Z7A5FST",
                    "AB8KNX3LINNX4","AU5BD5NSSES9H") )
100-(nrow(cv)/nrow(c)*100) # removes 14% of all the data

# save to CSV in order to load into Correllations.R
write.csv(cv,"cv.csv")

#################################################################################
#################################################################################
# Cleaning up the RT data

# Take a look at summary stats
plot(density(cv$RT))
h<-hist(cv$RT, breaks=100, col="red", xlab="RT (ms)",
        main="Histogram with Normal Curve")
mean(cv$RT) #2340.844
sd(cv$RT) #3140.069

summary(cv$RT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29     655    1181    1804    2124   59951 

# First, look at the range of RT
range(c$RT) 
# 1]    32 59951

quantile(c$RT)[4] + IQR(c$RT)*3 # 8998.25
#################################################################################

t1 <- subset(cv, cv$SentType == "T1")
h<-hist(t1$RT, breaks=20, col="red", xlab="RT (ms)",
        main="Histogram with Normal Curve")

summary(t1$RT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 96    1086    1976    2824    3499   39077  
quantile(t1$RT)[4] + IQR(t1$RT)*3 # 9268.125
# SDs after removing everything RT> 10s
bad <- subset(t1, t1$RT > 10000)
# remove all subjects RT> 10s
nrow(bad) #20 rows
View(bad)
# 18184 (31/32)
# 18445 (37)
# 18649(32)
# 19441 (37)
# 19699 (37)
# 19723 (32) took twice
# 19795 (30)

t1good <- subset(t1, t1$RT < 10645)
summary(t1good$RT)
plot(density(t1good$logRT))

# Take a look at all the subjects
t1_agr = t1good %>%
  group_by(ID,Value, Verb) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(t1_agr, aes(x=Value, y=meanRT, fill=reorder(ID,meanRT))) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Verb) +
  geom_bar(position=dodge,stat="identity")


plot(m)
#################################################################################
# Looking at control items
# TRUE CONTROL
# Sentence T4, "Zoologists know that cats are mammals"
t4 <- subset(c, c$SentType == "T4")
plot(density(t4$RT))
h<-hist(t4$RT, breaks=20, col="red", xlab="RT (ms)",
        main="Histogram with Normal Curve")

summary(t4$RT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 61     614    1043    1605    1944   17453

# Take a look at all the subjects
t4_agr = t4 %>%
  group_by(ID,Value,Verb) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(t4_agr, aes(x=Value, y=meanRT, fill=reorder(ID,meanRT))) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Verb) +
  geom_bar(position=dodge,stat="identity")

quantile(t4$RT)[4] + IQR(t4$RT)*3 #5934
# but look at the graph of subject RT and 5000 seems like a better cut-off
t4_sub <- subset(t4, t4$RT > 5000)
View(t4_sub) 
unique(t4_sub$ID)
# 19252 19441 19573 19705 19819
# These trials are also doubled...
# subjects (Item.number): 
# 19441 (25): 
# 19573 (19), 
# 19705 (19), 
# 19819 (25, 26)

# Look at each of these persons responses
# Subject 19441 responded over 10s for Control T4 and T5, 
# but don't think we should remove all their points
# 19573: only responded long on T4 and not even outside one SD
# 19705: responded really slow on T6 (RT> 6s)
# 19819: responded slow on T4 (RT> 10s), and T6 (RT> 7s)

# Conclusion: While these subjs have long RTs on controls (T4 and T6), 
# I don't think it warrants excluding all their data.

# FALSE CONTROL: 
# Sentence T2, "Zoologists know that cats are insects"
t2 <- subset(c, c$SentType == "T2")
plot(density(t2$RT))
h<-hist(t2$RT, breaks=20, col="red", xlab="RT (ms)",
        main="Histogram with Normal Curve")

summary(t2$RT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 309.5   775.5  1336.5  1840.9  2332.5 19725.5

# Take a look at all the subjects
t2_agr = t2 %>%
  group_by(ID,Value,Verb) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(t2_agr, aes(x=Value, y=meanRT, fill=reorder(ID,meanRT))) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Verb) +
  geom_bar(position=dodge,stat="identity")

quantile(t2$RT)[4] + IQR(t2$RT)*3 #7003.5 
# but look at the graph of subject RT and 5000 seems like a better cut-off
t2_sub <- subset(t2, t2$RT > 7003.5 )
View(t2_sub) 
unique(t2_sub$ID)
# Just one subject
# 191006034: Their longest response was for T2 at over 20s!
# They also took a little under 7.5s for T1

# LOOK AT SUBJECTS RESPONDING TOO QUICKLY
fast<- subset(c, c$RT < 100)
nrow(fast)
fast$ID
# These items will be removed...
# 17266: really fast on T3 and T4...but that's OK
# 17404: really fast for T1 'say'
# 18874: really fast on T1....under 100ms
# 19783: really fast on T1...


# Use the code below to just substititue in the relevant subject ID
a <- subset(c, c$ID == "19783")
# View(a)
summary(a$RT)

a_agr = a %>%
  group_by(SentType,Value,Verb) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(a_agr, aes(x=SentType, y=meanRT, fill=Value)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Verb) +
  geom_bar(position=dodge,stat="identity")

ggplot(a, aes(x=as.factor(SentNumber), y=RT, fill=Value)) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
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
  # stat_boxplot(geom = 'errorbar')+
  # geom_boxplot()

# something is wrong with these. why is it showing the exact same time signature?

#################################################################################
# Subjects to remove:
#   For having double: 18670, 19489, 19723


#################################################################################
#################################################################################
# REMOVING OUTLIERS
quantile(c$RT)[4] + IQR(c$RT)*3 # 8998.25 

# STDV without removing extremes
mean(c$RT) + 2*sd(c$RT) # 8422.37
# remove extremes
# CB only removed this much
cs <- subset(cv, cv$RT < 10000)
nrow(c) #3240
# removing failed controls
nrow(cv) #2880
# removing outliter RT points
nrow(cs) # 2828 data points

100-(nrow(cs)/nrow(cv)*100) #1.8%

ggdensity(cs$RT)
####################################
# STDV after extremes:
####################################

# remove responses RT > 2STDV: 
mean(cs$RT) + 2*sd(cs$RT) # 5479.747
ggdensity(subset(c, c$RT < 5479.747)$RT)
c1 <- subset(c, RT < 5479.747)
100 -(nrow(c1)/nrow(c)*100) # removes 7% of the data

####################################
# remove responses RT > 3 STDV:
mean(cs$RT) + 3*sd(cs$RT) # 7195.625
ggdensity(subset(c, c$RT < 7195.625)$RT)
c2 <- subset(c, RT < 7195.625)
100 -(nrow(c2)/nrow(c)*100) # removes 3% of the data
####################################
# remove responses RT > 2 * IQR:
quantile(c$RT)[4] + IQR(c$RT)*2 # 6956
c3 <- subset(c, RT < 6956)
100 -(nrow(c3)/nrow(c)*100) # Removes a little more than 4% of the data
####################################

# remove responses RT > 3*IQR:
quantile(c$RT)[4] + IQR(c$RT)*3 # 8998.25
c4 <- subset(c, RT < 8998.25)
100 -(nrow(c4)/nrow(c)*100) # Removes a little more than 2% of the data
####################################

too_long <- subset(c, RT > 7045.827)
View(too_long)
length(too_long$ID)

####################################
# Make a choice about which cut-off to use
cc <- subset(c, RT < 10000)
# Remove responses faster than 100ms
cc <- subset(cc, RT > 100)

# c2 is sig...and c4???
summary(aov(RT~order, data = c2))

cc["logRT"] = log(cc$RT)
ggdensity(cc$RT)
ggqqplot(cc$RT)

h<-hist(cc$RT, breaks=20, col="red", xlab="RT (ms)",
        main="Histogram with Normal Curve")
##################3##################3##################3##################3
# BOX AND WHISKERS
ggplot(cc, aes(x=SentType, y=RT, fill=Value)) +
  # facet_wrap(~Verb) +
  stat_boxplot(geom = 'errorbar')+
  geom_boxplot()

ggplot(cc, aes(x=SentType, y=logRT, fill=Value)) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # facet_wrap(~Verb) +
  # geom_bar(position=dodge,stat="identity") +
  stat_boxplot(geom = 'errorbar')+
  geom_boxplot()

# BARPLOT
cc_agr = cc %>%
  group_by(SentType,Value) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(cc_agr, aes(x=SentType, y=meanRT, fill=Value)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # facet_wrap(~Verb) +
  scale_y_continuous(breaks=seq(0,3000,500)) +
  # ylim(0,3000)+
  geom_bar(position=dodge,stat="identity")



##################3##################3##################3##################3

oneway.test(RT~SentType, data =cc)
# F = 53.608, num df = 8, denom df = 3469, p-value < 2.2e-16

res.aov <- aov(RT ~ SentType, data = cc)
summary(res.aov)
#                 Df    Sum Sq   Mean Sq F value Pr(>F)    
#   SentType       8 8.356e+08 104452385   52.08 <2e-16 ***
#   Residuals   8351 1.675e+10   2005734 

# Look at homogeneity of variances
plot(res.aov,1)
# no evident relationship between residuals and fitted values
library(car)
leveneTest(RT ~ SentType, data = cc)
# This is significant....which means a violation of homogeneity of variances

# Test for normality - VIOLATED
plot(res.aov,2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) # this can't be run because there's too much data....???

#############################################
# Non-parametric test for violation of normality
kruskal.test(RT~SentType, data = cc)
# Kruskal-Wallis chi-squared = 445.92, df = 8, p-value < 2.2e-16

kruskal.test(RT~Value, data = cc)
# Kruskal-Wallis chi-squared = 6.3474, df = 1, p-value = 0.01176
# 
# Residual analysis shows that a linear model is not right for the data
resids <- resid(res.aov)
plot(cc$RT, resids, xlab="RT", ylab="Residuals")
abline(0,0)
qplot(cc$RT, resids, colour = cc$SentType, 
      shape = cc$SentType, size=I(3.9), 
      xlab="Measurement Values", ylab="Residuals") + 
  labs(colour="Treatment Categories", shape = "Treatment Categories")

summary.lm(res.aov)

oneway.test(RT~Value, data =cc)
# F = 5.8445, num df = 1.0, denom df = 7645.2, p-value = 0.01565
plot(res.aov)

kruskal.test(RT~Value, data = cc)
# Kruskal-Wallis chi-squared = 6.3474, df = 1, p-value = 0.01176


summary(aov(RT ~ Value*SentType, data = cc))
  #                   Df    Sum Sq   Mean Sq F value   Pr(>F)    
  # Value             1 1.244e+07  12441141   6.240   0.0125 *  
  # SentType          8 8.416e+08 105203250  52.766  < 2e-16 ***
  # Value:SentType    8 9.955e+07  12444253   6.242 4.46e-08 ***
  # Residuals      8342 1.663e+10   1993752     

kruskal.test(RT ~ interaction(Value,SentType), data = cc)
# Kruskal-Wallis chi-squared = 526.19, df = 17, p-value < 2.2e-16


goods <- subset(cc, SentType=="T1" | SentType == "T5" | SentType == "T7")
ggplot(goods, aes(x=SentType, y=RT, fill=Value)) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # facet_wrap(~Verb) +
  # geom_bar(position=dodge,stat="identity") +
  stat_boxplot(geom = 'errorbar')+
  geom_boxplot()

t1_agr = goods %>%
  group_by(SentType,Value) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(t1_agr, aes(x=SentType, y=meanRT, fill=Value)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks=seq(0,3000,500)) +
  # ylim(0,3000)+
  geom_bar(position=dodge,stat="identity")

kruskal.test(RT~SentType, data = goods)
# Kruskal-Wallis chi-squared = 1.3821, df = 2, p-value = 0.5011
kruskal.test(RT ~ Value, data = goods)
# Kruskal-Wallis chi-squared = 19.071, df = 1, p-value = 1.259e-05

kruskal.test(RT~interaction(SentType,Value), data = goods)
# Kruskal-Wallis chi-squared = 39.479, df = 5, p-value = 1.902e-07
pairwise.wilcox.test(goods$RT,interaction(goods$SentType,goods$Value), p.adjust.method = "BH")
  #             T1.Agree T5.Agree T7.Agree T1.Disagree T5.Disagree
  # T5.Agree    0.82298  -        -        -           -          
  # T7.Agree    1.00000  0.92118  -        -           -          
  # T1.Disagree 0.23721  0.25025  0.09104  -           -          
  # T5.Disagree 0.23311  0.25025  0.07228  0.97796     -          
  # T7.Disagree 8.9e-05  4.0e-05  3.0e-09  0.00016     0.00043    

goods_T1 <- subset(goods, SentType == "T1")

# Agree vs. Disagree not sD in RT
m <- aov(RT ~ Value, data = goods_T1)
summary(aov(RT ~ Value, data = goods_T1)) #p = 0.05
#               Df    Sum Sq  Mean Sq F value Pr(>F)  
# Value         1 1.315e+07 13146427   3.751 0.0531 .
# Residuals   922 3.231e+09  3504392 

TukeyHSD(m1)

m1 <- oneway.test(RT~Value, data = goods_T1)
# F = 3.1296, num df = 1.00, denom df = 265.19, p-value = 0.07803

pairwise.t.test(cc$RT, cc$Value, p.adjust.method = "BH")
#           Agree
# Disagree 0.015

summary(aov(RT~Verb, data = goods))
  #             Df    Sum Sq  Mean Sq F value  Pr(>F)   
  # Verb          1 3.412e+07 34124447   8.776 0.00317 **
  # Residuals   618 2.403e+09  3888232  

# Interaction with verb? YES
# replicated with Say but not with know
res.aov <- aov(RT ~ Value*Verb, data = goods)
summary(res.aov)
#               Df    Sum Sq  Mean Sq F value Pr(>F)  
# Value         1 8.839e+06  8839412   3.844 0.0504 .
# Verb          1 3.563e+06  3562563   1.549 0.2137  
# Value:Verb    1 1.161e+07 11610138   5.049 0.0250 *
#   Residuals   662 1.522e+09  2299700 
pairwise.wilcox.test(goods$RT, interaction(goods$Value,goods$Verb), p.adjust.method = "BH")
  #             Agree.know Disagree.know Agree.say
  # Disagree.know 0.5613     -             -        
  # Agree.say     0.4598     0.7911        -        
  # Disagree.say  0.3500     0.0061        0.0020 


# outliers RT> 10s 
  #             Df    Sum Sq  Mean Sq F value   Pr(>F)    
  # Value         1 9.217e+03     9217   0.002 0.961081    
  # Verb          1 4.281e+07 42805665  11.067 0.000931 ***
  # Value:Verb    1 1.171e+07 11707754   3.027 0.082387 .  
  # Residuals   616 2.383e+09  3867742

#################################################################################
#################################################################################

# PROPORTION GRAPHS AFTeER
#################################################################################
# Proportions

cb_agr = cc %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(SentType, Verb) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)

ggplot(cb_agr, aes(SentType,y=Proportion_Agree, fill=Verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # scale_fill_grey() +
  ggtitle(label = "Chemla & Bott") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)





#################################################################################

#################################################################################
#################################################################################
# Correllations between Questions and CB/BN
#################################################################################
#################################################################################
#################################################################################
library(ggplot2)
library(ggpubr) # need this for ggboxplot()
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(lme4)
library(ordinal)
library(corrgram)

setwd("/Users/morganmoyer/Dropbox/Moyer_research/Embedded_Questions/Dissertation/Experiments/Reaction_time/results/")
source("../../helpers.R")

# The file is the one saved from the Jupyter Notebook
q <- read.csv("q_withscores.csv")
# head(q)
q = q%>%
  mutate(., Value.n = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(WhichAnswer, goal) %>%
  mutate(meanValue = mean(Value.n))
# https://www.google.com/search?client=firefox-b-1-d&q=r+correllation+analysis+how+to+set+up+data#kpvalbx=_ZAbNXrfHEMvAytMP-_qd8A035


##################################################################
##################################################################
# Try to plot on a scatter the 
plot(q$meanValue, q$logical, main = "Scatterplot", las=1)
abline(lm(q$meanValue~q$logical), col="red")
# View(q)

kruskal.test(Value ~ logical, data = q)
# Kruskal-Wallis chi-squared = 107.36, df = 6, p-value < 2.2e-16

# We know previously that the data fail the assumptions of ANOVA
# So we should use the non-parametric test
cor(q$Value.n, q$logical, method = "spearman")
# R = 0.07055966
# Using the parametric test increases the rho slightly
cor(q$Value.n, q$logical, method = "pearson")
# 0.09416141

# This one just gives you a more complete test with p value
cor.test(q$Value.n, q$logical, method = "spearman", exact=F) # ***
# data:  q$Value.n and q$logical
# S = 1.9271e+10, p-value = 6.029e-07
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.07055966 

# https://terpconnect.umd.edu/~egurarie/teaching/Biol709/Topic3/Lab11_GLMandModelSelection.html
glm <- clmm(formula = Value ~ WhichAnswer + goal + verb + (1|ID) + (1|ImageName),
    data = q,
    family = binomial)
summary(glm) # AIC: 4309.2
glm1 <- clmm(formula = Value ~ WhichAnswer + goal + verb + logical + (1|ID) + (1|ImageName),
                                 data = q,
                                 family = binomial)

anova(glm,glm1)

clm1 <- clm(Value ~ WhichAnswer + goal + verb + logical, link="logit", data = q)
clm <- clm(Value ~ WhichAnswer + goal + verb, link="logit", data = q)
anova(clm,clm1)
#     no.par    AIC  logLik LR.stat df Pr(>Chisq)    
# clm       7 4309.2 -2147.6                          
# clm1      8 4223.5 -2103.7  87.736  1  < 2.2e-16 ***


##################################################################
# Q vs. BN Scatterplot TOTAL
q_bn_agr = q %>%
  mutate(., Value.n = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(ID,WhichAnswer,verb,goal,logical) %>%
  summarise(Proportion_Agree = mean(Value.n), CILow = ci.low(Value.n), CIHigh = ci.high(Value.n)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh) %>%
  drop_na()
dodge = position_dodge(.9)

# nrow(q_agr) # 1248
ggplot(q_bn_agr, aes(logical,y=Proportion_Agree,color=WhichAnswer,shape=WhichAnswer)) +
  geom_point(size=2,alpha=.8,position=position_jitter(width=1,height=.1))+
  facet_grid(verb~goal) +
  scale_x_continuous(limits = c(0, 1)) +
  # geom_smooth(method="lm",formula = y ~ splines::bs(x, 3),se=FALSE) +
  geom_smooth(method=lm,se=FALSE,full_range=TRUE) + # formula = y~poly(x,2)
  ggtitle(label = "Prop_Agree X prop_logical") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

msmo_agr = q %>%
  filter(WhichAnswer %in% c("MS","MO")) %>%
  mutate(., Value.n = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(ID,WhichAnswer,verb,goal,logical) %>%
  summarise(Proportion_Agree = mean(Value.n), CILow = ci.low(Value.n), CIHigh = ci.high(Value.n)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh) %>%
  drop_na()
dodge = position_dodge(.9)

# nrow(msmo_agr) #624
ggplot(msmo_agr, aes(Proportion_Agree,y=logical,color=WhichAnswer,shape=WhichAnswer)) +
  geom_point(size=2,alpha=.8,position=position_jitter(width=1,height=.1))+
  facet_grid(verb~goal) +
  scale_x_continuous(limits = c(0, 1)) +
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE) + # formula = y~poly(x,2)
  ggtitle(label = "MS/MO Prop_Agree X prop_logical") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


pairs(~Value+WhichAnswer+goal+logical,data=q,
      main="Simple Scatterplot Matrix")

##################################################################


##################################################################
# SUBSET THE DATA 
nrow(q) #4992
log_subs = subset(q, q$logical > .5)
nrow(log_subs) # 3424
nrow(log_subs)/nrow(q)*100 #70%

prag_subs = subset(q, q$logical < .5)

nrow(prag_subs) # 1280
nrow(prag_subs)/nrow(q)*100 # 26%

# 192 subjects at .5 
neither_subs = subset(q, q$logical == .5)
nrow(neither_subs)# 192
nrow(neither_subs)/nrow(q)*100 #3.9%
##################################################################
# Is MS/MO significantly different between pragmatic and logical subs?
# YES
log_subs <- log_subs %>%
  mutate(BN_val = "L")
prag_subs <- prag_subs %>%
  mutate(BN_val = "P")
bntot <- rbind(log_subs,prag_subs)
msmobn <- subset(bntot, bntot$WhichAnswer == "MS" | bntot$WhichAnswer == "MO")
kruskal.test(Value ~ BN_val, data = bntot)
# Kruskal-Wallis chi-squared = 60.205, df = 1, p-value = 8.546e-15
kruskal.test(Value ~ interaction(BN_val, WhichQuestion), data = bntot)
# Kruskal-Wallis chi-squared = 60.286, df = 3, p-value = 5.106e-13

##############################################################
# Logical Subjs
View(log_subs)
log_agr = log_subs %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(WhichAnswer,verb,WhichQuestion,goal) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)
# View(log_agr)
ggplot(log_agr, aes(WhichAnswer,y=Proportion_Agree,fill=verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(WhichQuestion ~goal) +
  # scale_fill_grey() +
  ggtitle(label = "Logical Responders") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

kruskal.test(Value ~ goal, data = log_subs )
# Kruskal-Wallis chi-squared = 4.394, df = 2, p-value = 0.1111
kruskal.test(Value~WhichQuestion, data = log_subs)
# Kruskal-Wallis chi-squared = 0.080452, df = 1, p-value = 0.7767

log_subs_rt <- subset(log_subs, RT < 15000)
log_subs_rt <- subset(log_subs_rt, RT > 300)

kruskal.test(RT ~ Value, data = log_subs_rt)
# Kruskal-Wallis chi-squared = 69.284, df = 1, p-value < 2.2e-16

# WHY ARE PEOPLE DISAGREEING WITH MA?

log_RT_agr = log_subs_rt %>%
  group_by(Value, WhichAnswer,goal) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(log_RT_agr, aes(x=WhichAnswer, y=meanRT, fill=Value)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~goal) +
  # facet_grid(goal~verb) +
  geom_bar(position=dodge,stat="identity")


log_msmoma <- subset(log_subs, log_subs$WhichAnswer == "MS" |
                       log_subs$WhichAnswer == "MO" |
                       log_subs$WhichAnswer == "MA" )

kruskal.test(Value ~ interaction(WhichAnswer,goal), data = log_msmoma)
# Kruskal-Wallis chi-squared = 175.64, df = 8, p-value < 2.2e-16
kruskal.test(Value ~ goal, data = log_msmoma)
# Kruskal-Wallis chi-squared = 8.5605, df = 2, p-value = 0.01384
kruskal.test(Value ~ WhichQuestion, data = log_msmoma)
# Kruskal-Wallis chi-squared = 0.12777, df = 1, p-value = 0.7208

interWAG = interaction(log_msmoma$WhichAnswer,log_msmoma$goal)
pairwise.wilcox.test(log_msmoma$Value.n, interWAG, p.adjust.method = "BH")
  #         MA.MA   MO.MA   MS.MA   MA.MS   MO.MS   MS.MS   MA.none MO.none
  # MO.MA   < 2e-16 -       -       -       -       -       -       -      
  # MS.MA   < 2e-16 0.85455 -       -       -       -       -       -      
  # MA.MS   0.16436 < 2e-16 < 2e-16 -       -       -       -       -      
  # MO.MS   1.6e-11 0.02786 0.01681 1.9e-10 -       -       -       -      
  # MS.MS   1.7e-14 0.26339 0.21255 5.9e-14 0.24076 -       -       -      
  # MA.none 0.02858 6.5e-09 2.9e-09 0.32469 2.9e-05 3.4e-07 -       -      
  # MO.none 8.8e-08 0.00339 0.00205 5.4e-06 0.25739 0.03489 0.00456 -      
  # MS.none 5.9e-10 0.03785 0.02786 2.3e-08 0.84611 0.24076 0.00024 0.39772



log_msmoma_none <- subset(log_msmoma,  log_msmoma$goal == "none")

kruskal.test(Value~WhichAnswer, data = log_msmoma_none)
# Kruskal-Wallis chi-squared = 15.098, df = 2, p-value = 0.0005266
pairwise.wilcox.test(log_msmoma_none$Value.n,log_msmoma_none$WhichAnswer, p.adjust.method = "BH")
  #       MA      MO     
  # MO 0.00380 -      
  # MS 0.00035 0.37562
kruskal.test(Value~WhichQuestion, data = log_msmoma_none)
# Kruskal-Wallis chi-squared = 1.8456, df = 1, p-value = 0.1743



##################################################################
# Pragmatic Subjs
View(prag_subs)
prag_agr = prag_subs %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(WhichAnswer,verb,WhichQuestion,goal) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)
# View(log_agr)
ggplot(prag_agr, aes(WhichAnswer,y=Proportion_Agree,fill=verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(WhichQuestion ~goal) +
  # scale_fill_grey() +
  ggtitle(label = "Pragmatic Responders") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


kruskal.test(Value ~ goal, data = prag_subs)
# Kruskal-Wallis chi-squared = 19.734, df = 2, p-value = 5.185e-05
kruskal.test(Value~WhichQuestion, data = prag_subs)
# Kruskal-Wallis chi-squared = 0.0031235, df = 1, p-value = 0.9554

prag_msmoma <- subset(prag_subs, prag_subs$WhichAnswer == "MS" |
                       prag_subs$WhichAnswer == "MO" |
                       prag_subs$WhichAnswer == "MA" )

kruskal.test(Value ~ interaction(WhichAnswer,goal), data = prag_msmoma)
kruskal.test(Value ~ goal, data = prag_msmoma)
# Kruskal-Wallis chi-squared = 30.592, df = 2, p-value = 2.275e-07
kruskal.test(Value ~ WhichQuestion, data = prag_msmoma)
# Kruskal-Wallis chi-squared = 0.16466, df = 1, p-value = 0.6849

interWAG = interaction(prag_msmoma$WhichAnswer,prag_msmoma$goal)
pairwise.wilcox.test(prag_msmoma$Value.n, interWAG, p.adjust.method = "BH")
  #         MA.MA   MO.MA   MS.MA   MA.MS   MO.MS   MS.MS   MA.none MO.none
  # MO.MA   3.3e-11 -       -       -       -       -       -       -      
  # MS.MA   1.0e-11 0.8366  -       -       -       -       -       -      
  # MA.MS   0.2936  5.5e-12 1.4e-12 -       -       -       -       -      
  # MO.MS   1.1e-06 0.0083  0.0040  9.5e-07 -       -       -       -      
  # MS.MS   2.0e-07 0.0229  0.0119  1.1e-07 0.6750  -       -       -      
  # MA.none 0.4606  < 2e-16 < 2e-16 0.0088  < 2e-16 < 2e-16 -       -      
  # MO.none 1.1e-05 4.8e-05 1.7e-05 9.9e-06 0.2172  0.0738  < 2e-16 -      
  # MS.none 7.2e-06 8.1e-05 2.9e-05 5.7e-06 0.2772  0.1015  < 2e-16 0.8366 


##################################################################
# Neither Subjs

neither_agr = neither_subs %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(WhichAnswer,verb,WhichQuestion,goal) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)
View(neither_agr)
ggplot(neither_agr, aes(WhichAnswer,y=Proportion_Agree,fill=verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(WhichQuestion ~goal) +
  # scale_fill_grey() +
  ggtitle(label = "neitherical Responders") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


#################################################################
##################################################################
##################################################################
##################################################################
# CHEMLA AND BOTT

cor(q$Value.n, q$global, method = "spearman")
# 0.02225079
cor.test(q$Value.n, q$global, method = "spearman", exact=F)
# data:  q$Value.n and q$global
# S = 2.0272e+10, p-value = 0.116
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.02225079 

kruskal.test(Value ~ global, data = q)
# Kruskal-Wallis chi-squared = 4.4873, df = 2, p-value = 0.1061


glm2 <- clmm(formula = Value ~ WhichAnswer + goal + verb + (1|ID) + (1|ImageName),
            data = q,
            family = binomial)
summary(glm) # AIC: 4309.2
glm3 <- clmm(formula = Value ~ WhichAnswer + goal + verb + global + (1|ID) + (1|ImageName),
             data = q,
             family = binomial)

anova(glm2,glm3) # NO SD

clm1 <- clm(Value ~ WhichAnswer + goal + verb + logical, link="logit", data = q)
clm <- clm(Value ~ WhichAnswer + goal + verb, link="logit", data = q)
anova(clm,clm1)

##################################################################
# Q vs. CB Scatterplot TOTAL
q_agr = q %>%
  mutate(., Value.n = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(ID,WhichAnswer,verb,goal,global) %>%
  summarise(Proportion_Agree = mean(Value.n), CILow = ci.low(Value.n), CIHigh = ci.high(Value.n)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh) %>%
  drop_na()
dodge = position_dodge(.9)

ggplot(q_agr, aes(global,y=Proportion_Agree,color=WhichAnswer,shape=WhichAnswer)) +
  geom_point(size=2,alpha=.8,position=position_jitter(width=1,height=.1))+
  facet_grid(verb~goal) +
  scale_x_continuous(limits = c(0, 1)) +
  # geom_smooth(method="lm",formula = y ~ splines::bs(x, 3),se=FALSE) +
  geom_smooth(method=lm,se=FALSE,full_range=TRUE) + # formula = y~poly(x,2)
  ggtitle(label = "Prop_agree X prop_global") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

msmo_agr = q %>%
  filter(WhichAnswer %in% c("MS","MO")) %>%
  mutate(., Value.n = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(ID,WhichAnswer,verb,goal,global,logical) %>%
  summarise(Proportion_Agree = mean(Value.n), CILow = ci.low(Value.n), CIHigh = ci.high(Value.n)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh) %>%
  drop_na()
dodge = position_dodge(.9)

# nrow(msmo_agr) #624
ggplot(msmo_agr, aes(Proportion_Agree,y=global,color=WhichAnswer,shape=WhichAnswer)) +
  geom_point(size=2,alpha=.8,position=position_jitter(width=1,height=.1))+
  facet_grid(verb~goal) +
  scale_x_continuous(limits = c(0, 1)) + # wihtout this, it centers the axis on the mean
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE) +
  ggtitle(label = "MS/MO Prop_Agree X prop_logical") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


ggplot(msmo_agr, aes(logical,y=global,color=WhichAnswer,shape=WhichAnswer)) +
  geom_point(size=2,alpha=.8,position=position_jitter(width=1,height=.1))+
  facet_grid(verb~goal) +
  geom_jitter(position=position_jitter(0.2)) +
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE) +
  ggtitle(label = "MS/MO Prop_Global x Prop_Logical") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

##################################################################
# SUBSET THE DATA 
unique(q$global)

nrow(q) #4992
loc_subs = subset(q, q$global < .5)
nrow(loc_subs) # 480
nrow(loc_subs)/nrow(q)*100 #9.8%%

glob_subs = subset(q, q$global > .5)
nrow(glob_subs) # 3264
nrow(glob_subs)/nrow(q)*100 # 66%

# 192 subjects at .5 
neither_subs_cb = subset(q, q$global == .5)
nrow(neither_subs_cb)# 1152
nrow(neither_subs_cb)/nrow(q)*100 #23.5%

##################################################################
# Is MS/MO significantly different between global and local subs?
# YES
loc_subs <- loc_subs %>%
  mutate(CB_val = "L")
glob_subs <- glob_subs %>%
  mutate(CB_val = "G")
cbtot <- rbind(loc_subs,glob_subs)
msmocb <- subset(cbtot, cbtot$WhichAnswer == "MS" | cbtot$WhichAnswer == "MO")
kruskal.test(Value ~ CB_val, data = cbtot)
# Kruskal-Wallis chi-squared = 4.2778, df = 1, p-value = 0.03861


kruskal.test(Value ~ interaction(CB_val, WhichQuestion), data = cbtot)
# Kruskal-Wallis chi-squared = 4.4827, df = 3, p-value = 0.2138

##################################################################
# LOCAL Subjs
loc_agr = loc_subs %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(WhichAnswer,verb,WhichQuestion,goal) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)
# View(log_agr)
ggplot(loc_agr, aes(WhichAnswer,y=Proportion_Agree,fill=verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(WhichQuestion ~goal) +
  # scale_fill_grey() +
  ggtitle(label = "Local Responders") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

kruskal.test(Value ~ goal, data = loc_subs )
# Kruskal-Wallis chi-squared = 4.8919, df = 2, p-value = 0.08664
kruskal.test(Value~WhichQuestion, data = loc_subs)
# ruskal-Wallis chi-squared = 0.059578, df = 1, p-value = 0.8072




##################################################################
# GLOBAL Subjs
glob_agr = glob_subs %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(WhichAnswer,verb,WhichQuestion,goal) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)
# View(log_agr)
ggplot(glob_agr, aes(WhichAnswer,y=Proportion_Agree,fill=verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(WhichQuestion ~goal) +
  # scale_fill_grey() +
  ggtitle(label = "Global Responders") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


kruskal.test(Value ~ goal, data = glob_subs)
# Kruskal-Wallis chi-squared = 3.0536, df = 2, p-value = 0.2172
kruskal.test(Value~WhichQuestion, data = glob_subs)
# Kruskal-Wallis chi-squared = 0.1278, df = 1, p-value = 0.7207





##################################################################
##################################################################
# OVERALL
kruskal.test(Value ~ logical, data = q)
# ruskal-Wallis chi-squared = 96.814, df = 6, p-value < 2.2e-16

kruskal.test(Value ~ global, data = q)
# Kruskal-Wallis chi-squared = 3.6129, df = 2, p-value = 0.1642
kruskal.test(Value ~ ID, data = q)

# MSMO ONLY

msmo <- subset(q, WhichAnswer == "MS" | WhichAnswer == "MO")
kruskal.test(Value ~ logical, data = msmo)
# Kruskal-Wallis chi-squared = 202.24, df = 6, p-value < 2.2e-16
kruskal.test(Value ~ interaction(logical,goal), data = msmo)
# Kruskal-Wallis chi-squared = 351.59, df = 19, p-value < 2.2e-16


kruskal.test(Value ~ global, data = msmo)
# Kruskal-Wallis chi-squared = 8.7707, df = 2, p-value = 0.01246

kruskal.test(Value ~ interaction(global,goal), data = msmo)
# Kruskal-Wallis chi-squared = 51.007, df = 8, p-value = 2.615e-08



##################################################################
##################################################################
# BN vs. CB Scatterplot TOTAL

cor.test(q$global, q$logical, method = "spearman", exact=F)
# S = 2.0834e+10, p-value = 5.116e-06
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# -0.06511627

clm4 <- clmm(Value ~ WhichAnswer + goal + verb + global + logical + (1|ID) + (1|ImageName), link="logit", data = q)
clm5 <- clmm(Value ~ WhichAnswer + goal + verb + global + logical + (1|ID) + (1|ImageName), link="logit", data = q)



# nrow(q_agr) # 1248
ggplot(q, aes(global,y=logical,color=WhichAnswer,shape=WhichAnswer)) +
  geom_point(size=2,alpha=.8,position=position_jitter(width=1,height=.1))+
  facet_grid(verb~goal) +
  scale_x_continuous(limits = c(0, 1)) +
  # geom_smooth(method="lm",formula = y ~ splines::bs(x, 3),se=FALSE) +
  geom_smooth(method=lm,se=FALSE,full_range=TRUE) + # formula = y~poly(x,2)
  ggtitle(label = "prop_logical X prop_global") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


##################################################################
##################################################################

clm2 <- clm(Value ~ global + logical, link="logit", data = q)
summary(clm2)

clm3 <- clm(Value ~ WhichAnswer + goal + verb + global + logical, link="logit", data = q)
summary(clm3)

clm4 <- clm(Value ~ WhichAnswer + goal + verb, link="logit", data = q)
summary(clm4)

clm5 <- clm(Value ~ WhichAnswer + goal + verb + logical, link="logit", data = q)
anova(clm5,clm3)
clm6 <- clm(Value ~ WhichAnswer + goal + verb + global, link="logit", data = q)
anova(clm6,clm3)

anova(clm3,clm4)

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(lme4)

setwd("/Users/morganmoyer/Dropbox/Moyer_research/Embedded_Questions/Dissertation/Experiments/Reaction_time/results/")
source("../../helpers.R")

q <- read.csv("quest.csv")
nrow(q) #23426

#################################################################################
#################################################################################

# Questions Task

#################################################################################
#################################################################################
# subjects to remove FOR PARTICIPATING TWICE
table(q$ID,q$Group)
# Subjects who participanted twice (from CB):
# 19723, O < J, 54
# 19489, M < C, 54
# 18670, M < A, 54 ****Same group twice
# Remove the second participation

# Make a subset for each participant/group pair 
rem1 = subset(q, (q$ID=="19723") & (q$Group == "J"))
rem2 = subset(q, (q$ID=="19489") & (q$Group == "C"))
rem3 = subset(q, (q$ID=="18670") & (q$Group == "A"))

# combine them all together
to_remove = rbind(rem1, rem2, rem3)
nrow(to_remove) #288
nrow(q) #30929... 23426
# remove these with anti_join()
qv = anti_join(q,to_remove)
nrow(qv) #30641...23138
# Length before removing = 30929
# double check to make sure
# View(qv)


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







q_rt = qv %>%  
  filter((Parameter == "Selection" | Value == "Start")) %>%
  # mutate(Value = recode_factor(Value, Agree = 1, Disagree = 0)) %>%
  select(-c("Time.results.were.received","MD5.hash.of.participant.s.IP.address","Controller.name","Group.Ibex","PennElementType","PennElementName")) %>%
  group_by(ID,QuestType,verb,QuestType,WhichQuestion,WhichAnswer,Group,goal) %>%
  summarise( RT = mean(EventTime[Parameter=="Selection"] - EventTime[Value=="Start"]), N = length(Value)/2 ) #Value = Value[Parameter=="Selection"], 
# OK this for some reason won't allow that extra step so we have to create a second DF to get the value of Value for each subject and trial
# View(q_rt)

q_vals = qv %>%
  filter(Type %in% c("trial") & (Parameter == "Selection")) %>%
  select(-c("Time.results.were.received","MD5.hash.of.participant.s.IP.address","Controller.name","Group.Ibex","PennElementType","PennElementName")) %>%
  select(ID,QuestType,verb,QuestType,WhichQuestion,WhichAnswer,ImageName,order,Group,goal,study,Value)

# View(q_vals)
qq = inner_join(q_rt, q_vals)

# write.csv(qq,"qv.csv")
# Subjects to remove for other reasons???

# View(qq)
#################################################################################
# Take a look at the participants who are accepting MF

q_mf = subset(qq, qq$WhichAnswer=="MF") 
q_mf_agr = q_mf %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  filter(verb %in% c("know")) %>%
  group_by(ID) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh) %>%
  filter(Proportion_Agree > .25)
dodge = position_dodge(.9)
# View(q_mf_agr)
ggplot(q_mf_agr, aes(reorder(ID,Proportion_Agree),y=Proportion_Agree,fill=ID)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # facet_wrap(~WhichQuestion) +
  # scale_fill_grey() +
  ggtitle(label = "MF Answers") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)



unique(q_mf_agr$ID)
# [1] 18736          19288          A18T7E73TNGOKP A1FHS282JP487T A2EZNZ6X58RTNR A3EQO5ZIDBTSVK
# [7] A4E818Z7A5FST  A51U5BEIC5XVR  ACXP8KHFX06KR  AK4WZEW584BR9  ASFPEARDPRFL5  AU5BD5NSSES9H 
# [13] AX3N0BBAJ39GG


# Which of these failed 2 MF know trials???
bad_subs <- qq %>%
  filter(ID %in% c("18736","19288","A18T7E73TNGOKP","A1FHS282JP487T","A2EZNZ6X58RTNR",
                   "A3EQO5ZIDBTSVK","A4E818Z7A5FST" ,"A51U5BEIC5XVR","ACXP8KHFX06KR",
                   "AK4WZEW584BR9","ASFPEARDPRFL5","AU5BD5NSSES9H","AX3N0BBAJ39GG"))


q_good <- anti_join(qq,bad_subs)
length(unique(q_good$ID)) #227
length(unique(qq$ID))
length(unique(q$ID)) #240


#################################################################################
# Take a look at the participants who are accepting MF

q_ma = subset(q_good, q_good$WhichAnswer=="MA") 
q_ma_agr = q_ma %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  # filter(verb %in% c("know")) %>%
  group_by(ID) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh) %>%
  filter(Proportion_Agree < .75)
dodge = position_dodge(.9)
# View(q_ma_agr)
ggplot(q_ma_agr, aes(reorder(ID,Proportion_Agree),y=Proportion_Agree,fill=ID)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # facet_wrap(~WhichQuestion) +
  # scale_fill_grey() +
  ggtitle(label = "ma Answers") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
# View(q_ma_agr)

unique(q_ma_agr$ID)
# [1] 17263           17404            18274           182777                    
# [7] A1VR1XQEQQXYUE  A25YZ7RE911DPQ  A2QD7QFGCUNF5N    AD14EQ9O9JKRI     
# "Gabriela Cuevas"
# Which of these failed 2 ma know trials???
bad_subs2 <- qq %>%
  filter(ID %in% c("17263","17404","18274","182777","A1VR1XQEQQXYUE",
                   "A25YZ7RE911DPQ","A2QD7QFGCUNF5N","AD14EQ9O9JKRI"))


q_good2 <- anti_join(q_good,bad_subs2)
length(unique(q_good2$ID)) #219
nrow(q_good2)/nrow(qq)*100 # 90%

length(unique(qq$ID))
length(unique(q$ID)) #240


write.csv(q_good,"q_good.csv")
#################################################################################
#################################################################################
q_ma = subset(q_good2, q_good2$goal=="MA")
q_ms = subset(q_good2, q_good2$goal=="MS")
q_none = subset(q_good2, q_good2$goal=="none")

q_tot = q_good2 %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0))
str(q_ms)

# MS GOAL
q_ms_agr = q_ms %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(WhichAnswer,verb,WhichQuestion) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)
# View(q_ms_agr)
ggplot(q_ms_agr, aes(WhichAnswer,y=Proportion_Agree,fill=verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~WhichQuestion) +
  # scale_fill_grey() +
  ggtitle(label = "MS Goal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

kruskal.test(Value ~ WhichQuestion, data = q_ms)
# Kruskal-Wallis chi-squared = 0.014732, df = 1, p-value = 0.9034
kruskal.test(Value ~ WhichAnswer, data = q_ms)
# Kruskal-Wallis chi-squared = 1101.6, df = 3, p-value < 2.2e-16
kruskal.test(Value ~ verb, data = q_ms)
# Kruskal-Wallis chi-squared = 9.2074, df = 1, p-value = 0.00241


# MA GOAL
q_ma_agr = q_ma %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(WhichAnswer,verb,WhichQuestion) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)

ggplot(q_ma_agr, aes(WhichAnswer,y=Proportion_Agree,fill=verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~WhichQuestion) +
  # scale_fill_grey() +
  ggtitle(label = "MA Goal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


kruskal.test(Value ~ WhichQuestion, data = q_ma)
# Kruskal-Wallis chi-squared = 0.055437, df = 1, p-value = 0.8139
kruskal.test(Value ~ WhichAnswer, data = q_ma)
# Kruskal-Wallis chi-squared = 723.15, df = 3, p-value < 2.2e-16
kruskal.test(Value ~ verb, data = q_ma)
# Kruskal-Wallis chi-squared = 11.817, df = 1, p-value = 0.0005869




# NO GOAL
q_none_agr = q_none %>%
  mutate(., Value = recode(Value, Agree = 1, Disagree = 0)) %>%
  group_by(WhichAnswer,verb,WhichQuestion) %>%
  summarise(Proportion_Agree = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Proportion_Agree - CILow, YMax = Proportion_Agree + CIHigh)
dodge = position_dodge(.9)

ggplot(q_none_agr, aes(WhichAnswer,y=Proportion_Agree,fill=verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~WhichQuestion) +
  # scale_fill_grey() +
  ggtitle(label = "No Goal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

# NOT EVEN IN THIS CONDITION IS MODAL SIGNIFICANT
kruskal.test(Value ~ WhichQuestion, data = q_none)
# Kruskal-Wallis chi-squared = 0.052281, df = 1, p-value = 0.8191

kruskal.test(Value ~ WhichAnswer, data = q_none)
# Kruskal-Wallis chi-squared = 1075.3, df = 3, p-value < 2.2e-16
kruskal.test(Value ~ verb, data = q_none)
# Kruskal-Wallis chi-squared = 10.247, df = 1, p-value = 0.001369


###############################################################
#################################################################################
# Take a look at reaction time

plot(density(q_good2$RT))
summary(q_good$RT)

qq <- subset(q_good2, RT <20000)
qq <- subset(qq, RT > 200)
plot(density(qq$RT))

RT_agr = qq %>%
  group_by(Value,verb, WhichAnswer,goal) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(RT_agr, aes(x=WhichAnswer, y=meanRT, fill=Value)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(goal~verb) +
  geom_bar(position=dodge,stat="identity")


kruskal.test(RT ~ WhichAnswer, data = qq)
# Kruskal-Wallis chi-squared = 634.61, df = 3, p-value < 2.2e-16
pairwise.wilcox.test(qq$RT,qq$WhichAnswer, p.adjust.method = "BH")

kruskal.test(RT ~ interaction(WhichAnswer,Value), data = qq)
# Kruskal-Wallis chi-squared = 689.46, df = 7, p-value < 2.2e-16
pairwise.wilcox.test(qq$RT,qq$Value, p.adjust.method = "BH")


qq_msmo <- subset(qq, WhichAnswer == "MS" | WhichAnswer == "MO")
msmo_RT_agr = qq_msmo %>%
  group_by(Value,verb, WhichAnswer,goal) %>%
  summarise(meanRT = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = meanRT - CILow, YMax = meanRT + CIHigh)
dodge = position_dodge(.9)

ggplot(msmo_RT_agr, aes(x=WhichAnswer, y=meanRT, fill=Value)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(goal~verb) +
  geom_bar(position=dodge,stat="identity")

kruskal.test(RT ~ WhichAnswer, data = qq_msmo)
# Kruskal-Wallis chi-squared = 105.41, df = 1, p-value < 2.2e-16 ***

kruskal.test(RT ~ verb, data = qq_msmo)
# Kruskal-Wallis chi-squared = 5.9983, df = 1, p-value = 0.01432 *

kruskal.test(RT ~ interaction(WhichAnswer, Value), data = qq_msmo)
# Kruskal-Wallis chi-squared = 105.41, df = 1, p-value < 2.2e-16
interwav <- interaction(qq_msmo$WhichAnswer, qq_msmo$Value)
pairwise.wilcox.test(qq_msmo$RT, interwav, p.adjust.method = "BH")


###############################################################
# OVERALL

# NO EFFECT OF MODAL
kruskal.test(Value ~ WhichQuestion, data = q_tot)
# Kruskal-Wallis chi-squared = 0.03651, df = 1, p-value = 0.8485
# SMALL INTERACTION WITH GOAL
kruskal.test(Value ~ interaction(goal, WhichQuestion), data = q_tot)
# Kruskal-Wallis chi-squared = 12.447, df = 5, p-value = 0.02915
interGM <- interaction(q_tot$goal, q_tot$WhichQuestion)
pairwise.wilcox.test(q_tot$Value, interGM, p.adjust.method = "BH")
#           MA.Mod MS.Mod none.Mod MA.NoMod MS.NoMod
# MS.Mod     0.250  -      -        -        -       
# none.Mod   0.878  0.096  -        -        -       
# MA.NoMod   0.878  0.198  0.878    -        -       
# MS.NoMod   0.229  0.903  0.093    0.186    -       
# none.NoMod 0.796  0.093  0.878    0.878    0.093

kruskal.test(Value ~ WhichAnswer, data = q_tot)
# Kruskal-Wallis chi-squared = 2878.7, df = 3, p-value < 2.2e-16

pairwise.wilcox.test(q_tot$Value, q_tot$WhichAnswer, p.adjust.method = "BH")
  #     MA     MF     MO  
  # MF <2e-16 -      -   
  # MO <2e-16 <2e-16 -   
  # MS <2e-16 <2e-16 0.17


# EFFECT OF GOAL
kruskal.test(Value ~ goal, data = q_tot)
# Kruskal-Wallis chi-squared = 12.324, df = 2, p-value = 0.002109

pairwise.wilcox.test(q_tot$Value, q_tot$goal, p.adjust.method = "BH")
  #       MA    MS   
  # MS   0.026 -    
  # none 0.498 0.002

interGA = interaction(q_tot$goal,q_tot$WhichAnswer)
kruskal.test(Value ~ interaction(goal,WhichAnswer), data = q_tot)
# Kruskal-Wallis chi-squared = 2905.9, df = 11, p-value < 2.2e-16

pairwise.wilcox.test(q_tot$Value, interGA, p.adjust.method = "BH")


# VERB IS SIGNIFICANT AND INTERACTS WITH ANSWER
kruskal.test(Value ~ verb, data = q_tot)
# Kruskal-Wallis chi-squared = 30.705, df = 1, p-value = 3.004e-08

kruskal.test(Value ~ interaction(verb,WhichAnswer), data = q_tot)
# Kruskal-Wallis chi-squared = 2925.5, df = 7, p-value < 2.2e-16

kruskal.test(Value ~ interaction(verb,goal), data = q_tot)
# Kruskal-Wallis chi-squared = 43.597, df = 5, p-value = 2.796e-08

kruskal.test(Value ~ interaction(verb,goal,WhichAnswer), data = q_tot)
# Kruskal-Wallis chi-squared = 2954.3, df = 23, p-value < 2.2e-16



###############################################################
# Looking just at MS/MO
msmo = subset(q_tot, q_tot$WhichAnswer=="MS" | q_tot$WhichAnswer == "MO")

# GOAL
kruskal.test(Value ~ goal, data = msmo)
# Kruskal-Wallis chi-squared = 27.671, df = 2, p-value = 9.801e-07
pairwise.wilcox.test(msmo$Value, msmo$goal, p.adjust.method = "BH")
  #       MA     MS    
  # MS   0.0034 -     
  # none 0.4651 7.6e-05
# Acceptance of MS/MO answers in MS goal significantly higher than either None or MA Goal

# MODAL
kruskal.test(Value ~ WhichQuestion, data = msmo)
# Kruskal-Wallis chi-squared = 0.28249, df = 1, p-value = 0.5951
interGM <- interaction(msmo$WhichQuestion,msmo$goal)
kruskal.test(Value ~ interGM, data = msmo)
# Kruskal-Wallis chi-squared = 29.025, df = 5, p-value = 2.293e-05

# ANSWER
kruskal.test(Value ~ WhichAnswer, data = msmo)
# Kruskal-Wallis chi-squared = 1.9096, df = 1, p-value = 0.167

# The interaction is driven by the 
interGA = interaction(msmo$goal,msmo$WhichAnswer)
kruskal.test(Value ~ interaction(goal,WhichAnswer), data = msmo)
# Kruskal-Wallis chi-squared = 31.642, df = 5, p-value = 6.993e-06
pairwise.wilcox.test(msmo$Value, interGA, p.adjust.method = "BH")
  #         MA.MO   MS.MO   none.MO MA.MS   MS.MS  
  # MS.MO   0.00062 -       -       -       -      
  # none.MO 0.97273 0.00014 -       -       -      
  # MA.MS   0.89616 0.00113 0.89616 -       -      
  # MS.MS   0.08637 0.08637 0.06026 0.12768 -      
  # none.MS 0.74640 2.2e-05 0.74640 0.67552 0.01551


#################################################################################
# MA GOAL VS NO GOAL

noma <- subset(q_tot, q_tot$goal=="MA" | q_tot$goal=="none")

kruskal.test(Value ~ goal, data = noma)
# Kruskal-Wallis chi-squared = 0.46023, df = 1, p-value = 0.4975

kruskal.test(Value ~ WhichAnswer, data = noma)
# Kruskal-Wallis chi-squared = 1798.4, df = 3, p-value < 2.2e-16

kruskal.test(Value ~ WhichQuestion, data = noma)
# Kruskal-Wallis chi-squared = 0.10619, df = 1, p-value = 0.7445


noma_msmo <- subset(noma, noma$WhichAnswer=="MS" | noma$WhichAnswer=="MO")
kruskal.test(Value ~ goal, data = noma_msmo)
# Kruskal-Wallis chi-squared = 0.31265, df = 1, p-value = 0.5761




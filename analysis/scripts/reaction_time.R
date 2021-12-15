#  Stats for Con_Prob
# Date: June 2, 2019
############################
# Stats for Reaction Time Study
############################
require(dplyr)

setwd("/Users/morganmoyer/Dropbox/Moyer_research/Embedded_Questions/Dissertation/Experiments/Reaction_time/results/")
# source("helpers.R")

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (index < length(cols)){
          cols <- c()
        }
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

# REad in the results
# BOTT AND NOVECK
bna <- read.pcibex("q_none/results_bna")
# write.csv(bna,"bna.csv")
levels(bna$Quantifier)

bnb <- read.pcibex("q_none/results_bnb")
# write.csv(bnb,"bnb.csv")
levels(bnb$Quantifier)

bnama <- read.pcibex("q_ma/results_bna_ma")
# write.csv(bnama,"bnama.csv")
bnbma <- read.pcibex("q_ma/results_bnb_ma")
# write.csv(bnbma,"bnbma.csv")

bnams <- read.pcibex("q_ms/results_bna_ms")
# write.csv(bnams,"bnams.csv")
# View(bnams)
bnbms <- read.pcibex("q_ms/results_bnb_ms")
# write.csv(bnbms,"bnbms.csv")
levels(bnbms$Item.number)


# CHEMLA AND BOTT
cba <- read.pcibex("q_none/results_cba")
# write.csv(cba,"cba.csv")
cbb <- read.pcibex("q_none/results_cbb")
write.csv(cbb,"cbb.csv")

cbama <- read.pcibex("q_ma/results_cba_ma")
# write.csv(cbama,"cbama.csv")
cbbma <- read.pcibex("q_ma/results_cbb_ma")
# write.csv(cbbma,"cbbma.csv")

cbams <- read.pcibex("q_ms/results_cba_ms")
# write.csv(cbams,"cbams.csv")
cbbms <- read.pcibex("q_ms/results_cbb_ms")
# write.csv(cbbms,"cbbms.csv")



qn <- read.pcibex("q_none/results_q-none")
# write.csv(qn,"qn.csv")
qma <- read.pcibex("q_ma/results_q_ma")
# write.csv(qma,"qma.csv")
qms <- read.pcibex("q_ms/results_q_ms")
# write.csv(qms,"qms.csv")
qms2 <- read.pcibex("q_ms/results_q_ms_cba")
# write.csv(qms2,"qms2.csv")

# Add new columns for study_type and order
bna = bna %>%
  mutate(study = "bn") %>%
  mutate(order = "1") %>%
  mutate(goal = "none")
bnb = bnb %>%
  mutate(study = "bn") %>%
  mutate(order = "2") %>%
  mutate(goal = "none")
cba = cba %>%
  mutate(study = "cb") %>%
  mutate(order = "2") %>%
  mutate(goal = "none")
cbb = cbb %>%
  mutate(study = "cb") %>%
  mutate(order = "1") %>%
  mutate(goal = "none")
qn = qn %>%
  mutate(study = "qn") %>%
  mutate(order = "NA") %>%
  mutate(goal = "none")


# Add new columns for study_type and order
bnama = bnama %>%
  mutate(study = "bn") %>%
  mutate(order = "1") %>%
  mutate(goal = "MA")
bnbma = bnbma %>%
  mutate(study = "bn") %>%
  mutate(order = "2") %>%
  mutate(goal = "MA")
cbama = cbama %>%
  mutate(study = "cb") %>%
  mutate(order = "2") %>%
  mutate(goal = "MA")
cbbma = cbbma %>%
  mutate(study = "cb") %>%
  mutate(order = "1") %>%
  mutate(goal = "MA")
qma = qma %>%
  mutate(study = "q_ma") %>%
  mutate(order = "NA") %>%
  mutate(goal = "MA")


# Add new columns for study_type and order
bnams = bnams %>%
  mutate(study = "bn") %>%
  mutate(order = "1") %>%
  mutate(goal = "MS")
bnbms = bnbms %>%
  mutate(study = "bn") %>%
  mutate(order = "2")%>%
  mutate(goal = "MS")
cbams = cbams %>%
  mutate(study = "cb") %>%
  mutate(order = "2")%>%
  mutate(goal = "MS")
cbbms = cbbms %>%
  mutate(study = "cb") %>%
  mutate(order = "1")%>%
  mutate(goal = "MS")
qms = qms %>%
  mutate(study = "q_ms") %>%
  mutate(order = "1")%>%
  mutate(goal = "MS")
qms2 = qms2 %>%
  mutate(study = "q_ms") %>%
  mutate(order = "2")%>%
  mutate(goal = "MS")

bn = rbind(bna,bnb,bnams,bnbms,bnama,bnbma)
levels(bn$ID)

View(bn)

# Take a look at the non-native speakers
non_nat <- bn %>%
  filter(PennElementName %in% c("ID","NativeLang") & (Parameter == "Final")) %>%
  select(Time.results.were.received, PennElementName,Value) %>%
  spread(PennElementName,Value) %>%# make PennElementName into two columns
  filter(NativeLang != "English") %>%
  filter(NativeLang != "english" ) %>%
  filter(NativeLang != "English ") %>%
  filter(NativeLang != "ENGLISH")

View(non_nat)
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




write.csv(bn,"bn_tot.csv")
q = rbind(qn,qma,qms,qms2)
write.csv(q,"q_tot.csv")
# why are there different colums?
cb = rbind(cba,cbb,cbama,cbbma,cbams)
# something's going on with cbbms
# looking at the column names tells us that cbbms doesn't have the "Study" column name.
# TO DO: Figure out why this is. for now, add the column
names(cb)
names(cbbms)
# add the missing column to be filled in or dropped later.
cbbms["Study"] <- NA
cb = rbind(cb, cbbms)
write.csv(cb,"cb_tot.csv")



# Get subject IDs simply
id = unique(r$ID)
write.csv(id,"id.csv")

# Check out the distribution over Groups
agr = qn %>%
  group_by(Group, ID) %>%
  summarise(n = nrow(unique(ID)))

table(qn$Group)
table(cbb$Group)
table(cba$Group)
table(bna$Group)
table(bnb$Group)

View(agr)

# make all the dfs have the same column names so that can rbind() to make one big df
# is there no way to make this faster?

qn[setdiff(names(cba), names(qn))] <- NA
qn[setdiff(names(bna), names(qn))] <- NA


cba[setdiff(names(qn), names(cba))] <- NA
cba[setdiff(names(bna), names(cba))] <- NA

cbb[setdiff(names(qn), names(cbb))] <- NA
cbb[setdiff(names(bna), names(cbb))] <- NA

bnb[setdiff(names(cba), names(bnb))] <- NA
bnb[setdiff(names(qn), names(bnb))] <- NA

bna[setdiff(names(qn), names(bna))] <- NA
bna[setdiff(names(cba), names(bna))] <- NA


tot = rbind(qn, bna, bnb, cba, cbb)

write.csv(tot, "tot.csv")
View(tot)
nrow(tot)
# results %>%
#   filter(Ending %in% c("No-s","-s") & (Parameter == "Selection" | Value == "Start")) %>%
#   mutate(Accurate = rep(Value[Parameter=="Selection"]==gsub("No-s","two", gsub("-s", "one", Ending[Parameter=="Selection"])), each=2)) %>%
#   group_by(Accurate, Ending, Group, ID) %>%
#   summarise( RT = mean(EventTime[Parameter=="Selection"] - EventTime[Value=="Start"]) , N = length(Value)/2 )

bn = r %>%
  filter(Type %in% c("test") & (Parameter == "Selection" | Value == "Start")) %>%
  # mutate(Accurate = rep(Value[Parameter=="Selection"]==gsub("No-s","two", gsub("-s", "one", Ending[Parameter=="Selection"])), each=2)) %>%
  group_by(ID, Group, SentType, Item.number) %>%
  summarise( RT = mean(EventTime[Parameter=="Selection"] - EventTime[Value=="Start"]) , N = length(Value)/2, Value = Value[Parameter=="Selection"] )

View(bn)

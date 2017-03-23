# Yu Huang
# MA415
# 2017/3/22

library(foreign)
library(stringr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# clear the previous results
rm(list = ls())
gc()

#read tables
osha <- read.dbf("osha.DBF")
info <- read.dbf("optinfo.DBF")
accid <- read.dbf("accid.DBF")
admpay <- read.dbf("admpay.DBF")
debt <- read.dbf("debt.DBF")
hazsub <- read.dbf("hazsub.DBF")
history <- read.dbf("history.DBF")
prog <- read.dbf("prog.DBF")
relact <- read.dbf("relact.DBF")
optinfo <- read.dbf("optinfo.DBF")

# The main file: osha, main table with company name, address, date of inspection, etc. 
head(osha)
dim(osha)

# Hazardous Substances (HAZSUB.DBF): Describes accidents that involve hazardous substances 
# but that do not involve people.
head(hazsub)
dim(hazsub)

# Special Programs (PROG.DBF): A description of any special programs -- local or national -- 
# related to the specific inspection. Also included is documentation to explain further.
head(prog)

# *Related Activity (RELACT.DBF) - If the inspection is related to another inspection, 
# then the ACTIVITYNO of that inspection is listed. Other types of actions that might be 
# related include Accidents, Complaints or Referrals.
head(relact)

# Optional Information (OPTINFO.DBF) - Indicates additional information recorded for the inspection.
head(optinfo)

# Debt (DEBT.DBF) - Information about a debt, including interest, fees and the outcome of the debt.
head(debt)

# Event History (HISTORY.DBF) - The history of penalties or failure to abate, including the amount 
# and reason the amount was changed
head(history)

# Administrative or Pay (ADMPAY.DBF) - A record of payment of administrative fees, penalties or 
# failure-to-abate charges.
head(admpay)

# Check the number of uniqueness of the ACTIVITYNO. 
length(unique(osha$ACTIVITYNO))  # 80444
dim(osha)[1]  # 80445

# There is a repeated data, find that data.
osha %>%
  count(ACTIVITYNO) %>%
  filter(n >= 2)  # 17825978 appears twice 
which(osha$ACTIVITYNO == 17825068) # Apears in 69720 69721 these two lines
osha[which(osha$ACTIVITYNO == 17825068), ]
unique(osha)
dim(osha)
dim(unique(osha))
identical(osha[69720, ], osha[69721, ]) # They are different, CONTFLAG different

# Delete one of the data
osha_unique <- osha[-69721, ]
dim(osha_unique)

# Delete the data that 80% of the information is missing
names(osha_unique)
nas <- sapply(osha_unique, function(x) sum(is.na(x))/length(x))
for (i in names(osha_unique)) {
  if (unname(nas[i]) > 0.8) {
    osha_unique[i] <- NULL
  } 
}
names(osha_unique)

osha_unique %>% 
  select(ACTIVITYNO, SITESTATE, SITEADD, SIC,
         LWDIRATE) %>%
  head(4)

# decode accid
label1 <- read.dbf("lookups/acc.dbf")
if(sum(accid$SITESTATE=="MA") == dim(accid)[1]){accid %<>% select(-SITESTATE)}
dim(label1)

sum(label1$CATEGORY=="PART-BODY")
parts <- label1[(label1$CATEGORY== "PART-BODY"),]
dim(parts)

parts <- select(parts, CODE, VALUE)
head(parts)

colnames(parts) <- c("BODYPART", "VALUE")
str(parts)
accid_1 <- left_join(accid, parts, by="BODYPART")
head(accid_1)


# Import scc data set
# Choose data of MA
scc <- read.dbf("./lookups/scc.dbf")
scc %>%
  filter(STATE == "MA") %>%
  rename(SITECOUNTY = COUNTY, SITECITY = CITY) -> scc_MA

# Combine the osha_unqiue data with the scc_MA data
osha_unique %>%
  left_join(scc_MA, by = "SITECITY") -> osha_unique_1

# Combine the osha_unqiue_1 data with the accid_1 data
osha_unique_1 %>%
  left_join(accid_1[, names(accid_1) != "NAME"], by = "ACTIVITYNO") %>% 
  select(ACTIVITYNO, SITECNTY, SITECITY, 
         NATURE, EVENT, HAZSUB, OCC_CODE, NAME) -> osha_unique_new

head(osha_unique_new)

# "NAME" is the city address 
# ggplot2 plot
# install.packages(("forcats"))


# count the number of times each city appears
osha_unique_new %>%
  group_by(NAME) %>%
  count() -> osha_result
summary(osha_result)  


# barplot
# Since the numeber of cities is too large, I only choose a small amount (n > 500)
osha_result %>% 
  filter(n > 500) %>%
  ggplot() +
  geom_bar(aes(x = fct_reorder(NAME, n), y = log(n),  color = NAME), stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "", y = "")

# Draw barplot jobtitle and acitivityno in terms of hitsflag
plot <- ggplot(osha_unique_1, aes(JOBTITLE, ACTIVITYNO))
plot + geom_bar(stat = "identity", aes(fill = HISTFLAG))



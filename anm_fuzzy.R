#---------- HEADER ----------
# Script name: pilot_id_match.R
# Purpose: Data quality assessment for codebook and exploratory data analysis
# Author(s): Sahil Lalwani
# Date created: 06-11-2020



#---------- SET UP ----------

# Clearing workspace

rm(list = ls(all = TRUE))

# Installing packages

#install.packages(c("haven", "dplyr", "fuzzyjoin"))

setwd("/Volumes/NO NAME")
library(haven)
library(dplyr)
library(fuzzyjoin)
library(tidyverse)

# MASTER Merged child dataset (tracking + registration)
dat<- read_dta("bid_merged_child.dta")

#install.packages("readxl")
setwd("~/Downloads")
library(readxl)

# Latest available pilot data for Gurgaon
pilot_data_g<- read_excel("pilot_activity_report_hamilton_latest.xlsx")

# Latest available pilot data for Mewat
pilot_data_m<- read_excel("pilot_activity_report_syracuse_latest.xlsx")

# Latest available data (appended) for Mewat and Gurgaon
pilot_data<- rbind(pilot_data_g, pilot_data_m)

# Produces the number of unique pilots for a particular SC based on pilot data
unique_pilot_sc_pilotdat <- function(sc){
  counter<-unique(pilot_data$pilot_id[pilot_data$sub_loc==sc])
  counter<- counter[!counter=="" & !is.na(counter)]
  length(counter)
}


# Produces the number of unique pilots for a particular SC based on merged child data
# Note: Variations in spelling names may be an issue here because unique pilots are identified by pilot name
unique_pilot_sc_mergedat <- function(sc){
  counter<-unique(dat$pilot_name[dat$sc==sc])
  counter<- counter[!counter=="" & !is.na(counter)]
  length(counter)
}

# All entries in master merged child dataset for which pilot names are missing
# Contains total 4447 observations (entries)
datMissing<- subset(dat, is.na(pilot_name)|pilot_name=="")
dat<- subset(dat, !(is.na(pilot_name)|pilot_name==""))

# Add number of unique pilots for each SC in master dataset based on pilot data
datMissing$unique_pilot_pilotdat <- as.numeric(lapply(datMissing$sc, function(sc) sapply(sc, unique_pilot_sc_pilotdat)))

# Add number of unique pilots for each SC in master dataset based on merged child (master) data
datMissing$unique_pilot_mergedat <- as.numeric(lapply(datMissing$sc, function(sc) sapply(sc, unique_pilot_sc_mergedat)))

# Checking all unique combinations

unique(datMissing[c("unique_pilot_pilotdat", "unique_pilot_mergedat")])

# Include pilot names in master dataset for all SCs which had 1 unique pilot name in both pilot data and master
# (merged) child data
# 1679 such observations
# 16 unique SCs
# BALIAWAS, Hayatpur, SC Daula, Rethoj, Tikri Dhani, Laxman Vihar ph-1, Tain, Kankerkheri, Marora, Kherla,
# Freozpur Namak, Adbar, Rethora, Hasanpur Nuh, Indana, Bissar

# 15 unique pilots (for corresponding SCs in that order) [SC "Bissar" has different unique pilot name
# recorded under the merged dataset and the pilot data excel sheet]
# Indu, Sunita Rch, Poonam, Smt. Rajwati, Sunita, Lekha, Sahida, Asma Bano, Rekha Rani, Babli Rani,
# Indra Devi, Maya Devi, Sunita Devi, Anju Lata, Harwati, (Anju Lata, MEERA BAI in pilot data for this SC)
cond<- datMissing$unique_pilot_pilotdat==1 & datMissing$unique_pilot_mergedat==1 & !datMissing$sc=="Bissar"
nrow(subset(datMissing, cond))

find_pilot_name <- function(sc){
  counter<- unique(dat$pilot_name[dat$sc==sc])
  counter<- counter[!counter=="" & !is.na(counter)]
}

# Included names in the step below
# Also checked manually that the 1 unique pilot for an SC using merged data and pilot data were in fact the same
# for 15 unique SCs 
datMissing$pilot_name<- ifelse(cond, as.character(lapply(datMissing$sc, function(sc) sapply(sc, find_pilot_name))), 
                             datMissing$pilot_name)

# Checking obs. w/ pilot names in master dataset for all SCs which had 1 unique pilot name in pilot data and 2 in
# master (merged) child data
# 46 such observations

cond<- datMissing$unique_pilot_pilotdat==1 & datMissing$unique_pilot_mergedat==2
nrow(subset(datMissing, cond))

# SC Ghangola and Jiwant are the 2 unique SCs with these cases
unique(subset(datMissing, cond)$sc)

# Finding the unique pilot names in "SC Ghangola" according to (master) merged dataset
# Poonam and Anju: Indeed different pilots
find_pilot_name("SC Ghangola")

# Finding unique pilot names in "Jiwant" according to (master) merged dataset
# Manju and MANJU DEVI: Probably the same pilot
find_pilot_name("Jiwant")

datMissing$pilot_name<- ifelse(cond & datMissing$sc=="Jiwant", 
                             "MANJU DEVI", datMissing$pilot_name)


# Checking obs. w/ pilot names in master dataset for all SCs which had 1 unique pilot name in pilot data and 0 in
# master (merged) child data
# 46 such observations

cond<- datMissing$unique_pilot_pilotdat==1 & datMissing$unique_pilot_mergedat==0
nrow(subset(datMissing, cond))

find_pilot_name_pilotdat <- function(sc){
  counter<- unique(pilot_data$pilot_name[pilot_data$sub_loc==sc])
  counter<- counter[!counter=="" & !is.na(counter)]
}

# Gives Rakesh AWC Area as a result
unique(subset(datMissing, cond)$sc)

datMissing$pilot_name<- ifelse(cond, as.character(lapply(datMissing$sc, function(sc) sapply(sc, find_pilot_name_pilotdat))), 
                             datMissing$pilot_name)

# Checking obs. w/ pilot names in master dataset for all SCs which had 0 unique pilot name in pilot data and 1 in
# master (merged) child data
# 100 such observations

cond<- datMissing$unique_pilot_pilotdat==0 & datMissing$unique_pilot_mergedat==1
nrow(subset(datMissing, cond))

# SC entry blank for these entries
# Add no pilot names in this case
unique(subset(datMissing, cond)$sc)

## Combine dat and datMissing

dat<- rbind(dat, datMissing[1:78])

## Find out the number of entries in the new dat merged (master) dataset which are empty or NA
# 2727 observations with missing pilot names (4.38%)
nrow(subset(dat, dat$pilot_name==""|is.na(dat$pilot_name)))

dat_pilot_name_missing<- subset(dat, dat$pilot_name==""|is.na(dat$pilot_name))
dat_pilot_name_missing$pilot_id<- ""
dat_pilot_name_missing$contact_no<- ""
dat<- subset(dat, !(dat$pilot_name==""|is.na(dat$pilot_name)))


# length(datMissing$sc[datMissing$unique_pilot_pilotdat==0])
# length(datMissing$sc[datMissing$unique_pilot_mergedat==3])
# 
# # Returns False
# all(datMissing$unique_pilot_pilotdat==datMissing$unique_pilot_mergedat)
# 
# nrow(subset(datMissing, unique_pilot_pilotdat==1 & unique_pilot_mergedat==2))
# 
# unique(subset(datMissing, unique_pilot_pilotdat==1 & unique_pilot_mergedat==2)$sc)

datg<- subset(dat, district==1)
#& !(is.na(pilot_name)|pilot_name==""))
datm<- subset(dat, district==2)
#& !(is.na(pilot_name)|pilot_name==""))

fuzzy_g<- datg %>%
  stringdist_left_join(pilot_data_g, by= c(pilot_name="pilot_name"), ignore_case=T, method="soundex", distance_col="dist") %>%
  group_by(child_rch_id) %>%
  top_n(-1) %>%
  select(-dist)

fuzzy_m<- datm %>%
  stringdist_left_join(pilot_data_m, by=c(pilot_name="pilot_name"), ignore_case=T, method="jw", distance_col="dist") %>%
  group_by(child_rch_id) %>%
  top_n(-1) %>%
  select(-dist)

#fuzzy_g<-fuzzy_g[, c(1:7, 80:86)]
#fuzzy_g$ordering<- fuzzy_g$facility==fuzzy_g$loc & fuzzy_g$health_block==fuzzy_g$block
fuzzy_g$ordering<- fuzzy_g$sc==fuzzy_g$sub_loc
fuzzy_g$ordering<- as.numeric(fuzzy_g$ordering)
fuzzy_g <- fuzzy_g[order(fuzzy_g$ordering, decreasing=TRUE),]
fuzzy_g<- fuzzy_g[!duplicated(fuzzy_g$child_rch_id),]

#fuzzy_m<-fuzzy_m[, c(1:7, 80:86)]
#fuzzy_m$ordering<- fuzzy_m$facility==fuzzy_m$loc & fuzzy_m$health_block==fuzzy_m$block
fuzzy_m$ordering<- fuzzy_m$sc==fuzzy_m$sub_loc
fuzzy_m$ordering<- as.numeric(fuzzy_m$ordering)
fuzzy_m <- fuzzy_m[order(fuzzy_m$ordering, decreasing=TRUE),]
fuzzy_m<- fuzzy_m[!duplicated(fuzzy_m$child_rch_id),]

# Missing 2 observations here (should be 59480 but I have 59478)

## Checking fuzzy matching

# Checking all observations that have been

# 333
nrow(subset(fuzzy_g, pilot_name.x==pilot_name.y & sc==sub_loc))
fuzzy_g_name_not_same<- subset(fuzzy_g, !(pilot_name.x==pilot_name.y & sc==sub_loc))

nrow(subset(fuzzy_m, pilot_name.x==pilot_name.y & sc==sub_loc))
fuzzy_m_name_not_same<- subset(fuzzy_m, !(pilot_name.x==pilot_name.y & sc==sub_loc))

## Preparing the final dataset

pilot_ok<- c("MANJU DEVI")
sc_corresponding<- c("Jiwant")

fuzzy_all<- rbind(fuzzy_g, fuzzy_m)
cond<- fuzzy_all$pilot_name.x==fuzzy_all$pilot_name.y & fuzzy_all$sc==fuzzy_all$sub_loc

fuzzy_all$pilot_id<- ifelse(!cond, "", fuzzy_all$pilot_id)
fuzzy_all$contact_no<- ifelse(!cond, "", fuzzy_all$contact_no)

for (i in 1:length(pilot_ok)){
  fuzzy_all$pilot_id[fuzzy_all$pilot_name.y==pilot_ok[i]]= pilot_data$pilot_id[pilot_data$pilot_name==pilot_ok[i] & 
                                                                       pilot_data$sub_loc==sc_corresponding[i]]
  fuzzy_all$contact_no[fuzzy_all$pilot_name.y==pilot_ok[i]]= pilot_data$contact_no[pilot_data$pilot_name==pilot_ok[i] & 
                                                                       pilot_data$sub_loc==sc_corresponding[i]]
}

fuzzy_all_final<- fuzzy_all[,c(1:78,83,85)]
colnames(fuzzy_all_final)[6]="pilot_name"
colnames(fuzzy_all_final)<-colnames(dat_pilot_name_missing)
fuzzy_all<- dplyr::bind_rows(fuzzy_all_final, dat_pilot_name_missing)

#write.csv(fuzzy_all,file="bid_merged_child_pilot_id")
tmp <- tempfile(fileext = ".dta")
write_dta(fuzzy_all, tmp)

write_dta(fuzzy_all, "bid_merged_child_pilot_id.dta")

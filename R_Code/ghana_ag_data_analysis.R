# Data translator project

# Ghana Living Standards Survey

#Business Problem ------------------------------------------

# The ACME corporation is considering moving 
# into agricultural inputs in Ghana. 
# To get an idea of whom to target for their sales efforts 
# they have hired you to analyze what determines 
# agricultural profit there. They are especially interested 
# in what effect household educational attainment and 
# the characteristics of the local area has on profit, 
# but if you find other results of interest, they would like
# to hear those as well. They have left the choice of 
# explanatory variables and variable definitions to you, 
# but to be able to compare across regions they want at
# least one specification that examines profit per area unit


# Import lib and data----------------------------------------

library(tidyverse)
library(haven)


#array of all .dta files in dataset (not using this section)----
file_list <- list.files('Raw_Data/glss4_new/', pattern = '\\.dta*', recursive = TRUE)

#read in file from list
test <- read_dta(paste('Raw_Data/glss4_new/' , file_list[1], sep = ''))

#household and location info------------------------------------
id_df         <- read_dta('Raw_Data/glss4_new/sec0a.dta')
education_df  <- read_dta('Raw_Data/glss4_new/sec2a.dta')
land_chars_df <- read_dta('Raw_Data/glss4_new/sec8a1.dta')

#financials-----------------------------------------------------
ag_profit_df       <- read_dta('Raw_Data/glss4_new/aggregates/agg2.dta')
costs_land_df      <- read_dta('Raw_Data/glss4_new/aggregates/exp3.dta')
costs_crops_df     <- read_dta('Raw_Data/glss4_new/aggregates/exp4.dta')
costs_livestock_df <- read_dta('Raw_Data/glss4_new/aggregates/exp5.dta')

#community info-------------------------------------------------
#195 rural, 105 urban (only have comm surveys from rural)
community_df        <- read_dta('Raw_Data/glss4_new/community/cs2.dta')
community_health_df <- read_dta('Raw_Data/glss4_new/community/cs4b.dta')
community_ag_df     <- read_dta('Raw_Data/glss4_new/community/cs5b.dta')


# ---------------------- Organize data --------------------------

#comm doesn't use clust while everything else does.  examine reg/dist/eanum and clust
clust_map_df <- select(id_df, c(region, district, eanum, clust))

#sort by clust and make sure clust doesn't duplicate
clust_map_df <- clust_map_df %>% arrange(clust) %>% distinct()

#looks like clust is just '4' + eanum (reg/dist are unused)
#i.e. eanum 4 is clust 4004 and eanum 930 is clust 4930
test_eanum_mut <- clust_map_df %>% mutate(clust_mut = eanum + 4000)

#above works, do same for all comm dfs
#select(clust,everything()) puts clust in first column
community_ag_df <- community_ag_df %>% mutate(clust = eanum + 4000) %>%
  select(clust, everything())
community_df <- community_df %>% mutate(clust = eanum + 4000) %>%
  select(clust, everything())
community_health_df <- community_health_df %>% mutate(clust = eanum + 4000) %>%
  select(clust, everything())


#want to tidy with clust and nh being one data point in each row
#----------------------- Tidy Cost Data ------------------------

#separate farmcd into columns so cost of farm 1, cost of farm2 etc.
#this allows households with multiple farms to be contained in one row
#fill in NA with 0 (0 cost for that farm because it doesn't exist.  allows for summing household cost)
costs_land_tidy_df <- costs_land_df %>% 
  spread(key = farmcd, value = landexp, fill = 0) %>%
  arrange(clust)

#similarly for crop expense code
costs_crops_tidy_df <- costs_crops_df %>%
  spread(key = crpexpcd, value = cropexp, fill = 0) %>%
  arrange(clust)

#and livestock expense
costs_livestock_tidy_df <- costs_livestock_df %>%
  spread(key = crpexpcd, value = livexp, fill = 0) %>%
  arrange(clust)

#sum up expenses in each set into new column
costs_land_tidy_df <- costs_land_tidy_df %>%
  mutate(land_expsum = rowSums(costs_land_tidy_df[,-(1:2)])) %>% #don't include first two columns in sum
  select(nh,clust,land_expsum, everything()) #reorder with sum in front of individual pieces


costs_crops_tidy_df <- costs_crops_tidy_df %>%
  mutate(crop_expsum = rowSums(costs_crops_tidy_df[,-(1:2)])) %>%
  select(nh,clust,crop_expsum, everything())

costs_livestock_tidy_df <- costs_livestock_tidy_df %>%
  mutate(livestock_expsum = rowSums(costs_livestock_tidy_df[,-(1:2)])) %>%
  select(nh,clust,livestock_expsum, everything())

#ag_profit_df already tidy from the start

#---------------------- Tidy Education Data ---------------------

#similar to cost data, move individual education into columns so the data is
#tidy by household

#only interested in 5+ y/o (s1q23 = 1)
#pivot wider is like spread but seems to handle multiple columns better
education_tidy_df <- education_df %>% filter(s1q23 == 1) %>%
  pivot_wider(names_from = pid, values_from = c(s2aq1:s2aq17))

#for some reason column order is weird.  sort by col name xxx1_1, xxx1_2,...
#found function str_sort which can treat strings as numbers
#normal sort() did alpha i.e. _1, _10, _11 not _1, _2, _3
education_tidy_df <- education_tidy_df %>% select(str_sort(names(.), numeric = TRUE))

#add data about household education.  all data is per individual currently
#first count number of people in household
#just do this based on how many non-NA answers are in ever attended school question
educ_hh_size_df <- education_tidy_df %>% select(clust,nh,matches('s2aq1_')) %>%
  mutate(hh_size = rowSums(educ_hh_size_df[,-(1:2)] > 0, na.rm = TRUE )) %>%
  mutate(hh_num_educ = rowSums(educ_hh_size_df[,-(1:2)] == 1, na.rm = TRUE )) %>%
  mutate(hh_educ_rate = hh_num_educ / hh_size)  %>%
  select(-matches('s2aq1_'))

#add similar rates for completing at least oridinary level school (o level)
#and for some secondary training (after sixth form)

#education level enums
#excluding 'other' enum (==96) and religious (koranic == 17)
o_level      <- 8
some_sec_ed  <- 10
university   <- 16

educ_hh_highest_ed_df <- education_tidy_df %>% select(clust,nh,matches('s2aq2_')) %>%
  mutate(hh_num_at_least_o_lev = rowSums(educ_hh_highest_ed_df[,-(1:2)] >= o_level & educ_hh_highest_ed_df[,-(1:2)] <= university, na.rm = TRUE))%>%
  mutate(hh_num_some_sec_ed    = rowSums(educ_hh_highest_ed_df[,-(1:2)] >= some_sec_ed & educ_hh_highest_ed_df[,-(1:2)] <= university, na.rm = TRUE)) %>%
  select(-matches('s2aq2_'))

educ_hh_max_and_rate <- educ_hh_size_df %>% left_join(educ_hh_highest_ed_df, by = c('clust','nh')) %>%
  mutate(hh_rate_at_least_o_lev = hh_num_at_least_o_lev / hh_size) %>%
  mutate(hh_rate_some_sec_ed = hh_num_some_sec_ed / hh_size )

#----------------------- Tidy Community Health Data --------------------

#see G4QComm for enums (page 18)

#some communities have more than one data point.  filter down to most popular
#add number of occurances of clust
community_health_df <- community_health_df %>%
  add_count(clust) %>% select(clust, n, everything())

community_df <- community_df %>% 
  add_count(clust) %>% select(clust, n, everything())

community_ag_df <- community_ag_df %>% 
  add_count(clust) %>% select(clust, n, everything())

#6 different enums for health questions so 6 rows per community
community_health_df_single_hh_comms <- community_health_df %>% filter(n == 6)

community_health_df_multi_hh_comms <- community_health_df %>% filter(n > 6)

#other two are 1 per comm
community_df_single_hh_comms <- community_df %>% filter(n == 1)

community_df_multi_hh_comms <- community_df %>% filter(n > 1)

community_ag_df_single_hh_comms <- community_ag_df %>% filter(n == 1)

community_ag_df_multi_hh_comms <- community_ag_df %>% filter(n > 1)

#use the mode of a column to find the most popular value in each duplicate set
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ag first -----------------------------------------------------
#init filtered down dfs for multi hh comms
unique_clust_ag <- unique(community_ag_df_multi_hh_comms$clust)
community_ag_df_multi_hh_comms_filtered <- community_ag_df_multi_hh_comms[unique_clust_ag,]

for (i in 1:length(unique_clust_ag)){
  #get the most common value from each column for each clust
  this_df <- community_ag_df_multi_hh_comms[community_ag_df_multi_hh_comms$clust == unique_clust_ag[i],]
  
  for (j in names(this_df)){
    #get each column mode
    community_ag_df_multi_hh_comms_filtered[i,j] <- Mode(this_df[[j]])
    
  }
  
}

community_ag_tidy_df <- rbind(community_ag_df_multi_hh_comms_filtered, community_ag_df_single_hh_comms)

# ---------------------------------------- community df next ------------------------------------------
#do same as above
unique_clust <- unique(community_df_multi_hh_comms$clust)
community_df_multi_hh_comms_filtered <- community_df_multi_hh_comms[unique_clust,]

for (i in 1:length(unique_clust)){
  this_df <- community_df_multi_hh_comms[community_df_multi_hh_comms$clust == unique_clust[i],]
  
  for (j in names(this_df)){
    community_df_multi_hh_comms_filtered[i,j] <- Mode(this_df[[j]])
  }
}

community_tidy_df <- rbind(community_df_multi_hh_comms_filtered, community_df_single_hh_comms)

# -------------------------- hardest one is health due to needing to spread on 's4bq0' ---------------

unique_health_clust <- unique(community_health_df_multi_hh_comms$clust)
#note this will be too small due to 6 rows per clust but seems to work quickly enough
community_health_df_multi_hh_filtered <- community_health_df_multi_hh_comms[unique_health_clust,]

#need to keep track of rows since their are two vars controlling 'this_df'
n_row = 0

for (i in 1:length(unique_health_clust)){
  for (health_type in 10:15){
    this_df <- community_health_df_multi_hh_comms[(community_health_df_multi_hh_comms$clust == unique_health_clust[i] & 
                                                     community_health_df_multi_hh_comms$s4bq0 == health_type),]
    n_row = n_row + 1
    
    for (j in names(this_df)){
      community_health_df_multi_hh_filtered[n_row,j] <- Mode(this_df[[j]])
    }
  }
}

# ---------------- can now tidy community health data (split into one clust per row) -----------------

community_health_tidy_df <- rbind(community_health_df_multi_hh_filtered, community_health_df_single_hh_comms) %>% 
  pivot_wider(names_from = s4bq0, values_from = c(s4bq5:s4bq10))

#all data should be tidy by household  and clust now :)

# ----------------- Profit per consitent unit area ----------------------------

#land data for each household in land_chars_df
# convert all unit area to acre (3 = ropes = 1/9 acre) ignore the other category (count = 6)
#add land owned, rented, and land for share cropping
land_chars_df <- land_chars_df %>% mutate( hh_land_owned_acres = case_when(
  s8aq3 == 3 ~ s8aq4 / 9,
  s8aq3 == 2 ~ s8aq4,
  s8aq3 == 1 ~ s8aq4 )) %>% #others are NA
  mutate(hh_land_rented_acres = case_when(
    s8aq3 == 3 ~ s8aq14 / 9,
    s8aq3 == 2 ~ s8aq14,
    s8aq3 == 1 ~ s8aq14 )) %>% #others are NA
  mutate(hh_land_shared_acres = case_when(
    s8aq3 == 3 ~ s8aq17 / 9,
    s8aq3 == 2 ~ s8aq17,
    s8aq3 == 1 ~ s8aq17 )) %>% #others are NA
  mutate(hh_land_total_acres = rowSums(land_chars_df[,c('hh_land_owned_acres','hh_land_rented_acres','hh_land_shared_acres')],  na.rm = TRUE)) %>%
  select(clust, nh, s8aq3, matches('hh_land'), s8aq4, everything())



# --------------------- Agricultural Profit Models ------------------------------
# data should be in a tidy form by EA and household.  
# consistent unit of area and profit (acre)
# note from doc exchange rate:  exchange rate of about ?2,394 to the US dollar (march 1999)

#first sum up expenses of all types
costs_all_tidy_df <- costs_crops_tidy_df %>% left_join(costs_land_tidy_df, by = c('clust','nh')) %>%
  left_join(costs_livestock_tidy_df, by = c('clust','nh')) %>%
  select(clust, nh, matches('_expsum'))%>%
  mutate(total_exp = rowSums(costs_all_tidy_df[,3:5], na.rm = TRUE))
#add expenses to ag income df 
ag_profit_df <- ag_profit_df %>% left_join(costs_all_tidy_df, by = c('clust','nh')) %>%
  select(-crop_expsum, -land_expsum, -livestock_expsum)

#after reading doc closer, ag_profit_df has profit so expenses aren't needed.  oh well
#join profit and land data together
ag_profit_and_land_char_df <- ag_profit_df  %>% left_join(land_chars_df, by = c('clust','nh')) %>% 
  select(clust, nh, agri1c, matches('hh_land')) %>%
  filter(hh_land_total_acres > 0)
#only keep households with some land in this df

#add education data for first model
ag_profit_land_educ_df <- ag_profit_and_land_char_df %>% left_join(educ_hh_max_and_rate, by = c('clust','nh')) %>%
  mutate(prof_per_acre = agri1c / hh_land_total_acres)

ag_prof_vs_educ_lm <- lm(data = ag_profit_land_educ_df, prof_per_acre ~ hh_educ_rate)
summary(ag_prof_vs_educ_lm)

#plot fit
ggplot(data = ag_prof_vs_educ_lm, aes(x = hh_educ_rate, y = prof_per_acre)) +
  geom_point() +
  geom_abline(slope = ag_prof_vs_educ_lm$coefficients[2],
              intercept = ag_prof_vs_educ_lm$coefficients[1], col = 'blue') +
  ylim(c(-1000000,5e7)) + #6 outliers with this range
  xlab('Percent household members with some education') +
  ylab('Profit per acre of household') +
  ggtitle('Household Agriculture Profit vs Household Education')

#most/least profitable EAs
profit_grouped_by_clust <- ag_profit_land_educ_df %>% group_by(clust) %>% 
  summarise(totalEaProfit = sum(agri1c), n = n()) %>%
  mutate(avg_hh_prof = totalEaProfit / n) %>%
  arrange(desc(totalEaProfit))

#ggplot(data = profit_grouped_by_clust[1:10,], aes(x = clust, y = totalEaProfit)) +
#  geom_bar(stat="identity")


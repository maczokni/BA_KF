library(janitor) # for tidying df names
library(dplyr) # for data cleaning
library(tidyr) # for data cleaning
library(corrplot) # to make correlation plot

# read in dataset of coded variables
coding <- read.csv("data/feature_coding_data/output_irr2.csv") %>% 
  clean_names() %>% 
  select(-c(x,x_1))

# translate test into numeric values
coding[coding == "Negative"] <- -1
coding[coding == "Positive"] <- 1
coding[coding == "Neutral"] <- 0

# reviewers 5 and 6 flipped the coding so reverse their answers: 
coding$r5 <- as.numeric(coding$r5)*-1
coding$r6 <- as.numeric(coding$r6)*-1

#  create score for each item
coding <- coding %>% 
  mutate_at(c('r1',  'r2', 'r3', 'r4', 'r5', 'r6', 'r7'), as.numeric) %>% 
  mutate(coding_score = rowSums(select(.,r1, r2, r3, r4,  r5, r6, r7), na.rm = TRUE))

# create a df for correlations
corr_df <- coding %>% 
  select(-c(feature, agree, agree_no_r5)) %>% 
  mutate_if(is.character,as.numeric) %>% 
  drop_na()

# plot correlation between coders
corrplot(cor(corr_df) , method = "circle")
  

# test <- coding %>% 
#   mutate_at(c('r1',  'r2', 'r3', 'r4', 'r5', 'r6', 'r7'), as.numeric) %>% 
#   mutate(coding_score = r1 + r2 + r3 + r4 + r5 + r6 + r7)
# 

safe_rated_features <- coding %>% filter(coding_score < -4)
unsafe_rated_features <- coding %>% filter(coding_score > 3)


id_by_algo <- coding %>% 
  mutate(algo_coding = case_when(feature %in% c('Sky', 'Tree', 'Asphalt',
                                               'Plant', 'Cloud', 'Nature', 'Infrastructure',
                                               'Road surface', 
                                               "Natural landscape", 'Mode of transport', 
                                               'Ecoregion', 'Thoroughfare', 'Light') ~ -1, 
                                 
                                 feature %in% c('Building', 'Window', 'Property', 
                                                'Urban design',  'Land lot', 'Car', 'House', 
                                                'Vehicle') ~ 1,
                                 TRUE ~ 0))


# create a df for correlations
corr_df <- id_by_algo %>% 
  filter(coding_score < -1 | coding_score == 0 | coding_score > 1) %>% 
  select(-c(feature, agree, agree_no_r5_and_r6, coding_score)) %>% 
  mutate_if(is.character,as.numeric) %>% 
  drop_na()

# plot correlation between coders
corrplot(cor(corr_df) , method = "circle")


# Inter rater reliability tests
# http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/
# https://www.statology.org/intraclass-correlation-coefficient/
library(irr)

# Fleiss’ Kappa is a way to measure the degree of agreement between three or more raters when the raters are assigning categorical ratings to a set of items.
# Fleiss’ Kappa ranges from 0 to 1 where:
# 0 indicates no agreement at all among the raters.
# 1 indicates perfect inter-rater agreement.


kappam.fleiss(corr_df) # 0.249 

# Interpret:
# < 0.20 | Poor
# .21 – .40 | Fair
# .41 – .60 | Moderate
# .61 – .80 | Good
# .81 – 1 | Very Good

kappam.fleiss(corr_df %>% select(-algo_coding)) # 0.29
kappam.fleiss(corr_df %>% select(-r1)) # 0.245
kappam.fleiss(corr_df %>% select(-r2)) # 0.236
kappam.fleiss(corr_df %>% select(-r3)) # 0.221 
kappam.fleiss(corr_df %>% select(-r4)) # 0.286
kappam.fleiss(corr_df %>% select(-r5)) # 0.223
kappam.fleiss(corr_df %>% select(-r6)) # 0.253
kappam.fleiss(corr_df %>% select(-r7)) # 0.217


# an ICC is used to determine if items (or subjects) can be rated reliably by different raters.
# The value of an ICC can range from 0 to 1, with 0 indicating no reliability among raters and 1 indicating perfect reliability among raters.

# Consistency: We are interested in the systematic differences between the ratings of judges (e.g. did the judges rate similar subjects low and high?)
icc(corr_df, model="twoway", type="consistency", unit = "single")

# Absolute Agreement: We are interested in the absolute differences between the ratings of judges (e.g. what is the absolute difference in ratings between judge A and judge B?)
icc(corr_df, model="twoway", type="agreement")

# To interpret: 
# Less than 0.50: Poor reliability
# Between 0.5 and 0.75: Moderate reliability
# Between 0.75 and 0.9: Good reliability
# Greater than 0.9: Excellent reliability


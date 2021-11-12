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



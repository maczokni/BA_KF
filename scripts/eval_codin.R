library(janitor) # for tidying df names
library(dplyr) # for data cleaning
library(tidyr) # for data cleaning
library(corrplot) # to make correlation plot
library(irr)
library(ggplot2)

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
  select(-c(feature, agree, agree_no_r5_and_r6, coding_score)) %>% 
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

# 
# id_by_algo <- coding %>% 
#   mutate(algo_coding = case_when(feature %in% c('Sky', 'Tree', 'Asphalt',
#                                                'Plant', 'Cloud', 'Nature', 'Infrastructure',
#                                                'Road surface', 
#                                                "Natural landscape", 'Mode of transport', 
#                                                'Ecoregion', 'Thoroughfare', 'Light') ~ -1, 
#                                  
#                                  feature %in% c('Building', 'Window', 'Property', 
#                                                 'Urban design',  'Land lot', 'Car', 'House', 
#                                                 'Vehicle') ~ 1,
#                                  TRUE ~ 0))
# 
# 
# # create a df for correlations
# corr_df <- id_by_algo %>% 
#   filter(coding_score < -1 | coding_score == 0 | coding_score > 1) %>% 
#   select(-c(feature, agree, agree_no_r5_and_r6, coding_score)) %>% 
#   mutate_if(is.character,as.numeric) %>% 
#   drop_na()

algo_coding$algo_coding <- case_when(algo_coding$diff > 1 &
                                       algo_coding$safe > algo_coding$unsafe ~ 1, 
                                     algo_coding$diff > 1 &
                                       algo_coding$safe < algo_coding$unsafe ~ -1,
                                     algo_coding$diff <= 1 ~ 0)

# plot correlation between coders
corrplot(cor(corr_df) , method = "circle")




# All data from Simon

algo_coding <- read.csv("data/simon_data/Simon Parkinson - diff scores.csv")
algo_coding <- algo_coding %>% 
  mutate(which = ifelse(safe > unsafe, "safe", "unsafe"), 
         algo_score = ifelse(which == "safe", diff*-1, diff))

alltogether <- left_join(coding %>% 
                           mutate(link = tolower(gsub(" ", "",feature))), 
                                  algo_coding %>%
                           mutate(link = tolower(gsub(" ", "",Features))),
                         by = c("link" = "link"))

# correlation between composit human score and the algo score (diff)
cor.test(alltogether$coding_score, alltogether$algo_score, method =  "pearson")

# summary(lm(algo_score ~ coding_score, data = alltogether))

ggplot(alltogether, aes(x = coding_score, y = algo_score)) + 
  geom_jitter() + 
  theme_minimal() + 
  xlab("Score from coders") + 
  ylab("Score from algorithm")

alltogether <- alltogether %>% 
  mutate(algo_cat = case_when(algo_score < 0 ~ "Negative", 
                              algo_score == 0 ~ "Neutral", 
                              algo_score > 0 ~ "Positive"), 
         human_cat = case_when(coding_score < 0 ~ "Negative", 
                               coding_score == 0 ~ "Neutral", 
                               coding_score > 0 ~ "Positive"), 
         disagreements = paste(human_cat, algo_cat, sep = "-"))


human_neg_algo_pos <- alltogether %>% 
  filter(human_cat == "Negative" & algo_cat == "Positive")

human_pos_algo_neg <- alltogether %>% 
  filter(human_cat == "Positive" & algo_cat == "Negative")

ggplot() + 
  geom_point(data = alltogether, aes(x = coding_score, y = algo_score)) + 
  ggrepel::geom_label_repel(data = human_neg_algo_pos %>% filter(algo_score > 1), 
             aes(x = coding_score, y = algo_score, label = feature), 
             nudge_x = -3, nudge_y = 3, col = '#d73027') + 
  ggrepel::geom_label_repel(data = human_pos_algo_neg %>% filter(algo_score <= -1),
                            aes(x = coding_score, y = algo_score, label = feature),
                            nudge_x = 3, nudge_y = -4, col = '#4575b4') +
  theme_minimal() + 
  xlab("Score from coders") + 
  ylab("Score from algorithm")


human_neg_algo_neg <- alltogether %>% 
  filter(human_cat == "Negative" & algo_cat == "Negative")

human_pos_algo_neg <- alltogether %>% 
  filter(human_cat == "Positive" & algo_cat == "Negative")

ggplot() + 
  geom_point(data = alltogether, aes(x = coding_score, y = algo_score)) + 
  ggrepel::geom_label_repel(data = human_neg_algo_neg %>% filter(algo_score < -1), 
                            aes(x = coding_score, y = algo_score, label = feature), 
                            nudge_x = -3, nudge_y = 3, col = '#1b7837') + 
  ggrepel::geom_label_repel(data = alltogether %>% filter(coding_score > 3),
                            aes(x = coding_score, y = algo_score, label = feature),
                            nudge_x = 3, col = '#762a83') +
  theme_minimal() + 
  xlab("Score from coders") + 
  ylab("Score from algorithm")


# ggplot() + 
#   geom_point(data = alltogether, aes(x = coding_score, y = algo_score)) + 
#   ggrepel::geom_label_repel(data = alltogether %>% filter(coding_score > 3),
#                             aes(x = coding_score, y = algo_score, label = feature),
#                             nudge_x = 3, col = '#762a83') +
#   theme_minimal() + 
#   xlab("Score from coders") + 
#   ylab("Score from algorithm")

# IRR STUFF 

agree(alltogether %>% select(algo_cat, human_cat), tolerance != 0) 

kappa2(alltogether %>% select(algo_cat, human_cat))

# Inter rater reliability tests
# http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/
# https://www.statology.org/intraclass-correlation-coefficient/
library(irr)

alltogether$algo_cat_num <- case_when(alltogether$algo_cat == "Negative" ~ -1, 
                                      alltogether$algo_cat == "Positive" ~ 1, 
                                      alltogether$algo_cat == "Neutral" ~ 0 )

corr_df <- na.omit(alltogether %>% select(c(r1, r2, r3, r4, r5, r6, r7, algo_cat_num)) %>% 
                     rename(SCO = algo_cat_num))

corrplot(cor(corr_df) , method = "circle")


corr_df <- corr_df %>% 
  mutate_all(as.factor)

# Fleiss’ Kappa is a way to measure the degree of agreement between three or more raters when the raters are assigning categorical ratings to a set of items.
# Fleiss’ Kappa ranges from 0 to 1 where:
# 0 indicates no agreement at all among the raters.
# 1 indicates perfect inter-rater agreement.


kappam.fleiss(corr_df) # 0.208 

# Interpret:
# < 0.20 | Poor
# .21 – .40 | Fair
# .41 – .60 | Moderate
# .61 – .80 | Good
# .81 – 1 | Very Good

kappam.fleiss(corr_df %>% select(-algo_cat_num)) # 0.238
kappam.fleiss(corr_df %>% select(-r1)) # 0.199
kappam.fleiss(corr_df %>% select(-r2)) # 0.198
kappam.fleiss(corr_df %>% select(-r3)) # 0.184 
kappam.fleiss(corr_df %>% select(-r4)) # 0.246
kappam.fleiss(corr_df %>% select(-r5)) # 0.2
kappam.fleiss(corr_df %>% select(-r6)) # 0.208
kappam.fleiss(corr_df %>% select(-r7)) # 0.179


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


icc(corr_df %>% select(-SCO), model="twoway", type="consistency", unit = "single")
icc(corr_df %>% select(-SCO), model="twoway", type="agreement")


                                  
library(sf) # package for spatial data
library(ggplot2) # package for visualisations
library(dplyr) # package for data wrangling
library(tidyr) # package for data wrangling
library(readr) # to read csv data
library(janitor) # to tidy variable names

options(scipen = 9999) # turn off scientific notation

# read in data with all variables combined
all_data <- read_csv("data/polys_linked_w_images.csv") %>% clean_names()


all_data <- all_data %>% 
  # filter(poly_area > 0 & poly_area < 5000000) %>% 
  replace(is.na(.), 0)


ggplot(all_data, aes(x = type, y = sky)) +
  geom_boxplot() 


# try chi-square with whether it occurs at all or not
varnameslist <- names(all_data %>% select(-c(id, type, gender, poly_area, data, x1, x, num_images)))
datalist <- list()
i <- 1
for( varname in varnameslist){
  
  mod_data <- all_data %>% 
    select(eval(as.character(varname)), type, poly_area) 
  
  freq_table <- data.frame(A = mod_data %>% filter(type == "safePolygon" & .[[1]] > 0) %>% nrow(), 
             B = mod_data %>% filter(type == "fearPolygon" & .[[1]] > 0) %>% nrow(), 
             C = mod_data %>% filter(type == "safePolygon" & .[[1]] == 0) %>% nrow(), 
             D = mod_data %>% filter(type == "fearPolygon" & .[[1]] == 0) %>% nrow()
             )
  freq_table <- freq_table %>% 
    mutate( N = sum(A, B, C, D))
  
  
  datalist[[i]] <- data.frame(feature = varname,
                              x2 = (freq_table$N*((freq_table$A*freq_table$D)-(freq_table$C*freq_table$B))^2)/
                                ((freq_table$A + freq_table$B)*(freq_table$A + freq_table$C)*
                                   (freq_table$B + freq_table$D)*(freq_table$C + freq_table$D))
                              )
  i <- i + 1
}

chisq_results <- do.call(rbind, datalist)


# try chi-square with num imgs with and without feature
varnameslist <- names(all_data %>% select(-c(id, type, gender, poly_area, data, x1, x, num_images)))
datalist <- list()
i <- 1
for( varname in varnameslist){
  
  mod_data <- all_data %>% 
    select(eval(as.character(varname)), type, num_images) %>% 
    mutate(has = .[[1]], 
           doesnt_have = num_images - .[[1]]) %>% 
    group_by(type) %>% 
    summarise(sum_has = sum(has), 
              sum_doesnt_have = sum(doesnt_have))
  
  freq_table <- data.frame(A = mod_data %>% filter(type == "safePolygon") %>% 
                             pull(sum_has), 
                           B = mod_data %>% filter(type == "fearPolygon") %>% 
                             pull(sum_has), 
                           C = mod_data %>% filter(type == "safePolygon") %>% 
                             pull(sum_doesnt_have), 
                           D = mod_data %>% filter(type == "fearPolygon") %>% 
                             pull(sum_doesnt_have)
  )
  freq_table <- freq_table %>% 
    mutate( N = sum(A, B, C, D))
  
 x2 <-  (freq_table$N*((freq_table$A*freq_table$D)-(freq_table$C*freq_table$B))^2)/
    ((freq_table$A + freq_table$B)*(freq_table$A + freq_table$C)*
       (freq_table$B + freq_table$D)*(freq_table$C + freq_table$D))
  
  datalist[[i]] <- data.frame(feature = varname,
                              odds_of_feature_in_fear_poly = freq_table$B/freq_table$D, 
                              odds_of_feature_in_safe_poly = freq_table$A/freq_table$C,
                              OR = (freq_table$A/freq_table$C)/(freq_table$B/freq_table$D),
                              x2 = x2, 
                             # p-values from lookup table: https://www.mathsisfun.com/data/chi-square-table.html
                               p_value = case_when(
                                x2 >= 0.0000397 & x2 < 0.000157 ~ "0.995",
                                x2 >= 0.000157 & x2 < 0.000982 ~ "0.99",
                                x2 >= 0.000982 & x2 < 0.00393 ~ "0.975",
                                x2 >= 0.00393 & x2 < 0.0158 ~ "0.95",
                                x2 >= 0.0158 & x2 < 0.455 ~ "0.9",
                                x2 >= 0.455 & x2 < 1.642 ~ "0.5",
                                x2 >= 1.642 & x2 < 2.706 ~ "0.2",
                                x2 >= 2.706 & x2 < 3.841 ~ "0.1",
                                x2 >= 3.841 & x2 < 5.024 ~ "0.05",
                                x2 >= 5.024 & x2 < 5.412 ~ "0.025",
                                x2 >= 5.412 & x2 < 6.635 ~ "0.02",
                                x2 >= 6.635 & x2 < 7.879 ~ "0.01",
                                x2 >= 7.879 & x2 < 9.550 ~ "0.005",
                                x2 >= 9.550 & x2 < 10.828 ~ "0.002",
                                x2 >= 10.828 ~ "< 0.001",
                                TRUE ~ NA_character_),
                             safe_has = freq_table$A,
                             safe_doesnt_have = freq_table$C,
                             unsafe_has = freq_table$B,
                             unsafe_doesnt_have = freq_table$D,
                             total_imgs = freq_table$N
                             )
  i <- i + 1
}

chisq_byimg_results <- do.call(rbind, datalist)


at_least_100 <- chisq_byimg_results %>% filter(safe_has > 99 &
                                                 unsafe_has > 99)




# try logistic regression by m2

varnameslist <- names(all_data %>% select(-c(id, type, gender, poly_area, data, x1, x)))
datalist <- list()
i <- 1
for( varname in varnameslist){
  
  mod_data <- all_data %>% 
    select(eval(as.character(varname)), type, poly_area) %>% 
    mutate(per_m2 = .[[1]]/poly_area*100)
  
  mod_1 <- summary(glm(as.factor(type) ~ per_m2, data = mod_data, family = "binomial"))
  
  datalist[[i]] <- data.frame(feature = varname, 
                              estimate = mod_1$coefficients[2], 
                              exp_estimate = exp(mod_1$coefficients[2]),
                              std_error = mod_1$coefficients[4], 
                              z_value = mod_1$coefficients[6], 
                              p_value = mod_1$coefficients[8], 
                              aic = mod_1$aic, 
                              mod_x2 = with(mod_1, 
                                            pchisq(null.deviance - deviance, 
                                                   df.null - df.residual, 
                                                   lower.tail = FALSE)), 
                              likelihood_ratio_r2 = with(mod_1, 
                                                         (null.deviance - deviance)/null.deviance))
  
  i <- i + 1
}

logreg_results <- do.call(rbind, datalist)




# try logistic regression by num imgs

varnameslist <- names(all_data %>% select(-c(id, type, gender, poly_area, data, x1, x)))
datalist <- list()
i <- 1
for( varname in varnameslist){
  
  mod_data <- all_data %>% 
    select(eval(as.character(varname)), type, num_images) %>% 
    mutate(per_m2 = .[[1]]/num_images*100)
  
  mod_1 <- summary(glm(as.factor(type) ~ per_m2, data = mod_data, family = "binomial"))
  
  datalist[[i]] <- data.frame(feature = varname, 
                              estimate = mod_1$coefficients[2], 
                              exp_estimate = exp(mod_1$coefficients[2]),
                              std_error = mod_1$coefficients[4], 
                              z_value = mod_1$coefficients[6], 
                              p_value = mod_1$coefficients[8], 
                              aic = mod_1$aic, 
                              mod_x2 = with(mod_1, 
                                            pchisq(null.deviance - deviance, 
                                                   df.null - df.residual, 
                                                   lower.tail = FALSE)), 
                              likelihood_ratio_r2 = with(mod_1, 
                                                         (null.deviance - deviance)/null.deviance))
  
  i <- i + 1
}

logreg_results <- do.call(rbind, datalist)




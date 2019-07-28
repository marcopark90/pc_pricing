library(tidyverse)
library(purrr)
library(magrittr)
library(vroom)
library(pracma)
library(ROCR)

options(contrasts=c("contr.treatment", "contr.treatment"))

rm(list = ls())

set.seed(999)

# Functions ---------------------------------------------------------------

extract_act_metrics <- function(table, variable){

  table %>% 
    group_by_at(variable) %>% 
    summarise(exposure = sum(exposure),
              amounts = sum(claim_amount),
              numbers = sum(claim_count)) %>% 
    mutate(lc = amounts / exposure,
           freq = numbers / exposure,
           sev = if_else(numbers == 0, 0, amounts / numbers))
  
}


one_way_plot <- function(table){
  
  table %>% 
  select(-c(amounts,exposure,numbers)) %>% 
  gather(key, value, -1 ) %>% 
  ggplot() + 
  geom_line(aes_string(x = colnames(table)[1], y = "value", col = "key", group = "key")) +
  facet_wrap(~key, scales = "free")
  
  
}



# Import Data -------------------------------------------------------------

modeling_data <- readRDS("./external_data/modeling_data.RDS") %>% ungroup() %>% na.omit()

# Exploratory Analysis and One-Way Tables ---------------------------------

map(modeling_data, ~sum(is.na(.))) # Check NAs

map(select(modeling_data, -c(exposure, claim_amount, claim_count)), ~table(.)) # Check levels with low obs

modeling_data %>% extract_act_metrics("age_range") %>% one_way_plot()

# Train / Test Split ------------------------------------------------------

modeling_data <- modeling_data %>% 
                 mutate(sev = if_else(claim_count == 0, 0, claim_amount / claim_count),
                        lc = claim_amount / exposure) %>%
                 mutate(id = row_number())

train_data <- modeling_data %>% sample_frac(.8) 

test_data <- modeling_data %>% anti_join(train_data, by = "id")

# Frequency Model ---------------------------------------------------------

freq_model <- glm(claim_count ~ region + sex + age_range + vehicle_age + make +
                    vehicle_category + offset(log(exposure)),
                  family = poisson(link = "log"),
                  data = train_data
                  )

summary(freq_model)

# Severity Model ----------------------------------------------------------

sev_model <- glm(sev ~ region + age_range + vehicle_age + make +
                    vehicle_category,
                 family = Gamma(link = "log"),
                 weights = claim_count,
                 data = filter(train_data, claim_count > 0)
)

summary(sev_model)

# Combined Model ----------------------------------------------------------

train_data <- train_data %>% 
              mutate(pred_numbers = predict(freq_model, newdata = train_data, type = "response"),
                     pred_sev = predict(sev_model, newdata = train_data, type = "response"),
                     pred_lc = pred_numbers * pred_sev / exposure)

lc_model <- glm(pred_lc ~ region + sex + age_range + vehicle_age + make +
                  vehicle_category,
                family = Gamma(link = "log"),
                weights = exposure,
                data = train_data
)


sum(predict(lc_model, newdata = train_data, type = "response"))

sum(train_data$pred_lc)


# Performance Evaluation --------------------------------------------------

test_data <- test_data %>% 
             mutate(pred_lc = predict(lc_model, newdata = test_data, type = "response"))


# Actual vs. predicted
test_data %>% 
  select(exposure, lc, pred_lc) %>% 
  arrange(pred_lc) %>% 
  mutate(pred_bins = cut_interval(pred_lc, 10)) %>% 
  group_by(pred_bins) %>% 
  summarise(exp = sum(exposure),
            lc_wmean = weighted.mean(lc, exposure),
            pred_lc_wmean = weighted.mean(pred_lc, exposure)) %>% 
  gather(key, value, -c(pred_bins,exp)) %>% 
  ggplot() + 
  geom_bar(aes(x = pred_bins, y = exp/50  ), stat = "identity") +
  geom_line(aes(x = pred_bins, y = value, col = key, group = key)) +
  scale_y_continuous(sec.axis = sec_axis(~.*50, name = "Exposure"), name = "LC")


# Gini Plot
test_data %>% 
  select(exposure, pred_lc) %>% 
  arrange(pred_lc) %>% 
  mutate(cum_exp = cumsum(exposure)/sum(exposure),
         cum_pred_lc = cumsum(pred_lc)/sum(pred_lc)) %>% 
  ggplot()+
  geom_line(aes(x = cum_exp, y = cum_pred_lc))+
  geom_abline(intercept = 0, slope = 1)


# Gini Value
test_data %>% 
  select(exposure, pred_lc) %>% 
  arrange(pred_lc) %>% 
  mutate(cum_exp = cumsum(exposure)/sum(exposure),
         cum_pred_lc = cumsum(pred_lc)/sum(pred_lc)) %$% 
  trapz(cum_exp, cum_pred_lc) %>% add(-1) %>% abs() %>% subtract(.5) %>% multiply_by(2)


# Lift Curve
quant_comp <- test_data %>% 
                mutate(quant = ntile(pred_lc, 10)) %>% 
                group_by(quant) %>%
                summarise(mean_pred = mean(pred_lc)) %>% 
                inner_join(
                test_data %>% 
                mutate(quant = ntile(lc, 10)) %>% 
                group_by(quant) %>%
                summarise(mean_obs = mean(lc)), by = "quant")


quant_comp %>% 
  gather(key, value, -quant) %>% 
  ggplot() +
  geom_line(aes(x = as.factor(quant), y = value, col = key, group = key))+
  geom_point(aes(x = as.factor(quant), y = value, col = key, group = key))+
  xlab("Decile")
  
# NRMSE
test_data %$% rmse(lc, pred_lc) / (max(test_data$lc) - min(test_data$lc))

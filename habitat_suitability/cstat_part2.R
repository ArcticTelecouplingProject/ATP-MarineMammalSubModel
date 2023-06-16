# Sydney Waloven
# Created on: 16Jun2023

# This script was adapted from the final project of IBIO860 Modern Statistical
# Models in Ecology. This script is Part II which uses the data prepared and saved
# to Rds in the Data_Processed folder from Part I to generate habitat suitability
# models. This script has been adapted after consulting with CSTAT.

# Loading preliminaries ---------------------------------------------------

library(sf)
library(tidyverse)
library(dismo)
library(raster)
library(plyr)
library(terra)
library(gstat)
library(maptools)


# Model Fitting, Prediction, Evaluation -----------------------------------


## Model fitting

# Load the data if not in the environment
sdmdata_sum <- readRDS("../Data_Processed/prelim_models/CSTAT/sdmdata_sum_wal.Rds") # summer
sdmdata_fall <- readRDS("../Data_Processed/prelim_models/CSTAT/sdmdata_fall_wal.Rds") # fall

# predictor values at occurrence locations
pvals_sum15 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_sum15.Rds")
pvals_sum16 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_sum16.Rds")
pvals_sum16 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_sum17.Rds")
pvals_sum17 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_sum18.Rds")
pvals_sum18 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_sum19.Rds")

pvals_fall15 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_fall15.Rds")
pvals_fall16 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_fall16.Rds")
pvals_fall17 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_fall17.Rds")
pvals_fall18 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_fall18.Rds")
pvals_fall19 <- readRDS("../Data_Processed/prelim_models/CSTAT/pvals_fall19.Rds")

# predictors
pred_sum15 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_sum15.Rds")
pred_sum16 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_sum16.Rds")
pred_sum17 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_sum17.Rds")
pred_sum18 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_sum18.Rds")
pred_sum19 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_sum19.Rds")

pred_fall15 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_fall15.Rds")
pred_fall16 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_fall16.Rds")
pred_fall17 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_fall17.Rds")
pred_fall18 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_fall18.Rds")
pred_fall19 <- readRDS("../Data_Processed/prelim_models/CSTAT/pred_fall19.Rds")

# Corresponding predictor names
# chloro = layer.1
# sea_ice = layer.2
# sst = layer.3
# dist = layer.4

# Fit reduced models for summer for each year with all predictor variables except shipping
m15s <- glm(pb_sum15 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_sum)
m16s <- glm(pb_sum16 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_sum)
m17s <- glm(pb_sum17 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_sum)
m18s <- glm(pb_sum18 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_sum)
m19s <- glm(pb_sum19 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_sum)

# Fit reduced models for fall for each year with all predictor variables except shipping
m15f <- glm(pb_fall15 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_fall)
m16f <- glm(pb_fall16 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_fall)
m17f <- glm(pb_fall17 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_fall)
m18f <- glm(pb_fall18 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_fall)
m19f <- glm(pb_fall19 ~ layer.1 + layer.2 + layer.3 + layer.4, data = sdmdata_fall)

# Fit full models for summer for each year with shipping included
m15ss <- glm(pb_sum15 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_summer, data = sdmdata_sum)
m16ss <- glm(pb_sum16 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_summer, data = sdmdata_sum)
m17ss <- glm(pb_sum17 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_summer, data = sdmdata_sum)
m18ss <- glm(pb_sum18 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_summer, data = sdmdata_sum)
m19ss <- glm(pb_sum19 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_summer, data = sdmdata_sum)

# Fit full models for fall for each year with shipping included
m15fs <- glm(pb_fall15 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_fall, data = sdmdata_fall)
m16fs <- glm(pb_fall16 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_fall, data = sdmdata_fall)
m17fs <- glm(pb_fall17 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_fall, data = sdmdata_fall)
m18fs <- glm(pb_fall18 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_fall, data = sdmdata_fall)
m19fs <- glm(pb_fall19 ~ layer.1 + layer.2 + layer.3 + layer.4 + all_ships_fall, data = sdmdata_fall)

# Fit null models
# summer
m15s_null <- glm(pb_sum15 ~ 1, data = sdmdata_sum)
m16s_null <- glm(pb_sum16 ~ 1, data = sdmdata_sum)
m17s_null <- glm(pb_sum17 ~ 1, data = sdmdata_sum)
m18s_null <- glm(pb_sum18 ~ 1, data = sdmdata_sum)
m19s_null <- glm(pb_sum19 ~ 1, data = sdmdata_sum)

# fall
m15f_null <- glm(pb_fall15 ~ 1, data = sdmdata_fall)
m16f_null <- glm(pb_fall16 ~ 1, data = sdmdata_fall)
m17f_null <- glm(pb_fall17 ~ 1, data = sdmdata_fall)
m18f_null <- glm(pb_fall18 ~ 1, data = sdmdata_fall)
m19f_null <- glm(pb_fall19 ~ 1, data = sdmdata_fall)

# Create list of model names
m_red_sum <- list(m15s, m16s, m17s, m18s, m19s)
m_red_fall <- list(m15f, m16f, m17f, m18f, m19f)

m_full_sum <- list(m15ss, m16ss, m17ss, m18ss, m19ss)
m_full_fall <- list(m15fs, m16fs, m17fs, m18fs, m19fs)

m_null_sum <- list(m15s_null, m16s_null, m17s_null, m18s_null, m19s_null)
m_null_fall <- list(m15f_null, m16f_null, m17f_null, m18f_null, m19f_null)


# Print summaries
red_sum_sums <- lapply(m_red_sum, summary)
print(red_sum_sums)

red_fall_sums <- lapply(m_red_fall, summary)
print(red_fall_sums)

full_sum_sums <- lapply(m_full_sum, summary)
print(full_sum_sums)

full_fall_sums <- lapply(m_full_fall, summary)
print(full_fall_sums)

null_sum_sums <- lapply(m_null_sum, summary)
print(m_null_sum)

null_fall_sums <- lapply(m_null_fall, summary)
print(m_null_fall)


# Predict suitability scores with raster object ---------------------------

# Summers with and without shipping

# 2015 summer without shipping
p_red_sum15 <- predict(pred_sum15, m15s)
# 2015 summer with shipping
p_full_sum15 <- predict(pred_sum15, m15ss)

plot(p_red_sum15)
plot(p_full_sum15)

# 2016 summer without shipping
p_red_sum16 <- predict(pred_sum16, m16s)
# 2016 summer with shipping
p_full_sum16 <- predict(pred_sum16, m16ss)

plot(p_red_sum16)
plot(p_full_sum16)

# 2017 summer without shipping
p_red_sum17 <- predict(pred_sum17, m17s)
# 2017 summer with shipping
p_full_sum17 <- predict(pred_sum17, m17ss)

plot(p_red_sum17)
plot(p_full_sum17)

# 2018 summer without shipping
p_red_sum18 <- predict(pred_sum18, m18s)
# 2018 summer with shipping
p_full_sum18 <- predict(pred_sum18, m18ss)

plot(p_red_sum18)
plot(p_full_sum18)

# 2019 summer without shipping
p_red_sum19 <- predict(pred_sum19, m19s)
# 2019 summer with shipping
p_full_sum19 <- predict(pred_sum19, m19ss)

plot(p_red_sum19)
plot(p_full_sum19)

# Summaries for summer 2015-2019 with and without shipping
# with shipping
red_sum_sums <- lapply(m_red_sum, summary)
print(red_sum_sums)
# without shipping
full_sum_sums <- lapply(m_full_sum, summary)
print(full_sum_sums)
# null model summer
null_sum_sums <- lapply(m_null_sum, summary)
print(m_null_sum)

# Comparison between 2015 and 2017 summer and fall with and without shipping

# 2015 summer without shipping
p_red_sum15 <- predict(pred_sum15, m15s)
# 2015 summer with shipping
p_full_sum15 <- predict(pred_sum15, m15ss)

plot(p_red_sum15)
plot(p_full_sum15)

# 2015 fall without shipping
p_red_fall15 <- predict(pred_fall15, m15f)
# 2015 fall with shipping
p_full_fall15 <- predict(pred_fall15, m15fs)

plot(p_red_fall15)
plot(p_full_fall15)

# 2017 summer without shipping
p_red_sum17 <- predict(pred_sum17, m17s)
# 2017 summer with shipping
p_full_sum17 <- predict(pred_sum17, m17ss)

plot(p_red_sum17)
plot(p_full_sum17)

# 2017 fall without shipping
p_red_fall17 <- predict(pred_fall17, m17f)
# 2017 fall with shipping
p_full_fall17 <- predict(pred_fall17, m17fs)

plot(p_red_fall17)
plot(p_full_fall17)

# Summaries for both seasons in 2015 and 2017, with and without shipping
# with shipping
m15s %>% summary()
m17s %>% summary()

m15f %>% summary()
m17f %>% summary()

m15s_null %>% summary()
m17s_null %>% summary()

m15f_null %>% summary()
m17f_null %>% summary()



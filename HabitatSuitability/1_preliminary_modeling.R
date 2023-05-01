# Sydney Waloven
# 19Mar2023
# This script was adapted from my final project for IBIO870 Spatial Ecology.
# The purpose of this script is to create preliminary habitat suitability models
# using the MaxEnt code from the final project script as a starting point.
# This script includes distinct sections for the spotted seal, bearded seal, and
# Pacific walrus.



# Part 1 ------------------------------------------------------------------
# Loading preliminaries ---------------------------------------------------

library(sf)
library(tidyverse)
library(dismo)
library(raster)
library(plyr)
library(terra)
library(gstat)
library(maptools)


# Importing and cleaning data ---------------------------------------------

# Import integrated spotted seal data
all_spot <- st_read("../Data_Processed/largha_integrations/largha_integrated20230417.shp")

# Import integrated bearded seal data
all_beard <- st_read("../Data_Processed/barbatus_integrations/barbatus_integrated20230417.shp")

# Import integrated walrus data
all_walrus <- st_read("../Data_Processed/odobenus_integrations/odobenus_integrated20230417.shp")

# Rename columns
all_spot <- all_spot %>% dplyr::rename(
                                scientificName = scntfcN,
                                commonName = commnNm,
                                groupSize = groupSz,
                                decimalLat = decmlLt,
                                decimalLon = decmlLn,
                                methodObservation = mthdObs)

all_beard <- all_beard %>% dplyr::rename(
                                       scientificName = scntfcN,
                                       commonName = commnNm,
                                       groupSize = groupSz,
                                       decimalLat = decmlLt,
                                       decimalLon = decmlLn,
                                       methodObservation = mthdObs)

all_walrus <- all_walrus %>% dplyr::rename(
                                       scientificName = scntfcN,
                                       commonName = commnNm,
                                       groupSize = groupSz,
                                       decimalLat = decmlLt,
                                       decimalLon = decmlLn,
                                       methodObservation = mthdObs)


# Preparing species occurrences for dismo ---------------------------------

# Check to see if there are NAs just in case
# largha.geo <- subset(all_spot, !is.na(decimalLon) & !is.na(decimalLat))
# dim(la.geo) # number of occurrences match the number in original df which means there were no NAs



# Environmental Data ------------------------------------------------------

# Create the folders (directories) "data"
# if they exist already, this command won't over-write them.
# create directory called "data"

dir.create("../../ATP-MarineMammal-DataRaw/prelim_models_data", recursive = TRUE)

datadir <- "../../ATP-MarineMammal-DataRaw/"

## Load the data from Ascii_Environmental_Variables

# Chlorophyll Concentration
chloro_conc <- list.files(paste0(datadir, "Ascii_Environmental_Variables/Chlorophyll_Concentration"),
                                      pattern = "*.asc",
                                      full.names = TRUE)
# Create a raster layer list
# chloro_list <- lapply(chloro_conc, raster)

# Select only the mean files - excluding the standard deviation files - for each season
# aggregated for all years
## Summer: june-aug
chloro_ja <- chloro_conc[grepl(pattern = "ja", x = chloro_conc)] # takes all the files with "ja"
## Fall: sep-nov
chloro_sn <- chloro_conc[grepl(pattern = "sn", x = chloro_conc)] # takes all the files with "sn"

chloro_sum <- chloro_ja[grepl(pattern = "_mn", x = chloro_ja)] # from ja, takes all the files with "_mn"
chloro_fall <- chloro_sn[grepl(pattern = "_mn", x = chloro_sn)] # from sn, takes all the files with "_mn"


# raster(chloro_conc[1]) # check value

# Distance to Land
dist_land <- list.files(paste0(datadir, "Ascii_Environmental_Variables/Distance_to_Land"),
                        pattern = "*.asc",
                        full.names = TRUE)
## This is just one file

# Sea Ice Extent
sea_ice <- list.files(paste0(datadir, "Ascii_Environmental_Variables/Sea_Ice_Extent"),
                      pattern = "*.asc",
                      full.names = TRUE)

# Select files for each season - aggregated for all years
## Summer: june-aug
ice_sum <- sea_ice[grepl(pattern = "ja", x = sea_ice)] # takes all the files with "ja"
## Fall: sep-nov
ice_fall <- sea_ice[grepl(pattern = "sn", x = sea_ice)] # takes all the files with "sn"

# Sea Surface Temperature
sst <- list.files(paste0(datadir, "Ascii_Environmental_Variables/Sea_Surface_Temperature"),
                  pattern = "*.asc",
                  full.names = TRUE)

# Select only the mean files - excluding the standard deviation files - for each season
# aggregated for all years
## Summer: june-aug
sst_ja <- sst[grepl(pattern = "ja", x = sst)] # takes all the files with "ja"
## Fall: sep-nov
sst_sn <- sst[grepl(pattern = "sn", x = sst)] # takes all the files with "sn"

sst_sum <- sst_ja[grepl(pattern = "_mn", x = sst_ja)] # from ja, takes all the files with "_mn"
sst_fall <- sst_sn[grepl(pattern = "_mn", x = sst_sn)] # from sn, takes all the files with "_mn"


# Shipping
# Data is aggregated for the years 2015-2020
shipping <- list.files(paste0(datadir, "Ascii_Environmental_Variables/shipping"),
                       pattern = "*.asc",
                       full.names = TRUE)

# Select files for each season - aggregated for all years
## Summer: june-aug
ship_sum <- shipping[grepl(pattern = "summer", x = shipping)] # takes all the files with "ja"
## Fall: sep-nov
ship_fall <- shipping[grepl(pattern = "fall", x = shipping)] # takes all the files with "sn"


## Separate occurrences by season

# Create vector of unique years
unique_years <- unique(all_spot$year)

# Create a for loop to subset the all_spot df per each unique_years
for (year in unique_years) {
  # select all rows where the "year" column matches the current value of the
  # "year" variable in the loop
  largha <- all_spot[all_spot$year == year, ]
  # add a new column that assigns the value "summer" if the month is in the range
  # 6-8 (inclusive) and "fall" if the month is in the range 9-11 (inclusive)
  largha$season <- ifelse(largha$month %in% c(6, 7, 8), "summer", "fall")
  # assign the resulting subset to a new df labeled with the name and year
  assign(paste0("largha", year), largha)
}

## Take observations from 2015-2020 and create 1 stack per year, only extracting
## observations from that year for that stack. E.g., 2015 observations get 2015
## values, etc. 

# Iterate over the numbers 15-20 to create a raster stack for each predictor and year
# for chlorophyll concentration, sea ice, and sst

## Chlorophyll Concentration
for (num in 15:20) {
  # Construct the pattern to search for files
  pattern1 <- paste0(num, "_ja_mn") # only want the mean files
  pattern2 <- paste0(num, "_sn_mn") 
  
  # Check if the pattern exists in the file names
  if (any(grepl(pattern1, chloro_sum))) {
  # Summer
    chloro <- chloro_sum[grepl(pattern = pattern1, x = chloro_sum)]
    chloro_stack <- stack(chloro) # Create chloro stack with files containing pattern
    assign(paste0("chloro_sum_", num), chloro_stack) # Rename files for summer
    
  }
  # Fall
  if (any(grepl(pattern2, chloro_fall))) {
    chloro <- chloro_fall[grepl(pattern = pattern2, x = chloro_fall)]
    chloro_stack <- stack(chloro) # Create chloro stack with files containing pattern
    assign(paste0("chloro_fall_", num), chloro_stack) # Rename files for fall
  }
}


## Sea Ice Extent
for (num in 15:20) {
  # Construct the pattern to search for files
  # sum (sm) - corresponds to the number of days when ice was present in each pixel
  pattern1 <- paste0(num, "_ja_sm") 
  pattern2 <- paste0(num, "_sn_sm") 
  
  # Check if the pattern exists in the file names
  if (any(grepl(pattern1, ice_sum))) {
    # Summer
    ice <- ice_sum[grepl(pattern = pattern1, x = ice_sum)]
    ice_stack <- stack(ice) # Create chloro stack with files containing pattern
    assign(paste0("ice_sum_", num), ice_stack) # Rename files for summer
    
  }
  # Fall
  if (any(grepl(pattern2, ice_fall))) {
    ice <- ice_fall[grepl(pattern = pattern2, x = ice_fall)]
    ice_stack <- stack(ice) # Create chloro stack with files containing pattern
    assign(paste0("ice_fall_", num), ice_stack) # Rename files for fall
  }
}


## Sea Surface Temperature
for (num in 15:20) {
  # Construct the pattern to search for files
  pattern1 <- paste0(num, "_ja_mn") # only want the mean files
  pattern2 <- paste0(num, "_sn_mn") 
  
  # Check if the pattern exists in the file names
  if (any(grepl(pattern1, sst_sum))) {
    # Summer
    sst <- sst_sum[grepl(pattern = pattern1, x = sst_sum)]
    sst_stack <- stack(sst) # Create chloro stack with files containing pattern
    assign(paste0("sst_sum_", num), sst_stack) # Rename files for summer
    
  }
  # Fall
  if (any(grepl(pattern2, sst_fall))) {
    sst <- sst_fall[grepl(pattern = pattern2, x = sst_fall)]
    sst_stack <- stack(sst) # Create chloro stack with files containing pattern
    assign(paste0("sst_fall_", num), sst_stack) # Rename files for fall
  }
}



 # ****************

# Create a RasterStack out of predictor variables
chlsum_pred <- stack(chloro_sum)
chlfall_pred <- stack(chloro_fall)
dist_pred <- stack(dist_land)
icesum_pred <- stack(ice_sum)
icefall_pred <- stack(ice_fall)
sstsum_pred <- stack(sst_sum)
sstfall_pred <- stack(sst_fall)
shipsum_pred <- stack(ship_sum)
shipfall_pred <- stack(ship_fall)

# Create vector of names for each predictor stack
# chloro_names <- names(chloro_pred)
# dist_names <- names(dist_pred)
# ice_names <- names(ice_pred)
# sst_names <- names(sst_pred)
# ship_names <- names(shipping_pred)

# Create vector of predictors for summer
sum_pred <- stack(c(chlsum_pred, dist_pred, sstsum_pred, shipsum_pred))


# Plot to check
chloro_pred %>% plot()
dist_pred %>% plot()
ice_pred %>% plot()
sst_pred %>% plot()
shipping_pred %>% plot()

predictors %>% plot()


# Create bounding box for extent of BSR using study_area extent
# Can check bbox values with this or using bbox()
study_area <- st_read(paste0(datadir, "study_area/StudyArea.shp")) # Polygon

# Make study_area spatial first
study_area_sp <- as(study_area, "Spatial") # SpatialPolygonsDataFrame

# Transform to Alaska Albers
# predictors %>% st_transform(crs = 3338)

# Now crop
predictors_na <- crop(predictors, study_area_sp)

# Check predictor data - should be the BSR
plot(predictors_na)

# Now we have predictor variables (rasters) and occurrences
# Need to extract the values of the predictors at the locations of the points
# Cast geometry to another type with st_cast()

## Extracting values from rasters - presence values with predictors
presvals_spot <- raster::extract(predictors_na, all_spot) # spotted seal
presvals_beard <- raster::extract(predictors_na, all_beard) # bearded seal
presvals_wal <- raster::extract(predictors_na, all_walrus) # walrus

# Setting random seed to always create the same random set of points for this example
set.seed(0)

# Changed # of random points based on number of species occurrences (e.g., all_spot = 2039)
backgr_spot <- randomPoints(predictors_na, 2000)
backgr_beard <- randomPoints(predictors_na, 5600)
backgr_wal <- randomPoints(predictors_na, 9000)

absvals_spot <- extract(predictors_na, backgr_spot)
absvals_beard <- extract(predictors_na, backgr_beard)
absvals_wal <- extract(predictors_na, backgr_wal)

# pb indicates whether this is a presence/background point
pb_spot <- c(rep(1, nrow(presvals_spot)), rep(0, nrow(absvals_spot)))
pb_beard <- c(rep(1, nrow(presvals_beard)), rep(0, nrow(absvals_beard)))
pb_wal <- c(rep(1, nrow(presvals_wal)), rep(0, nrow(absvals_wal)))

# Create data tibble for sdm for each species

## spotted seal
sdmdata_spot <- data.frame(cbind(pb_spot, rbind(presvals_spot, absvals_spot)))
sdmdata_spot %>% count(pb_spot) # Nan originally had sort = TRUE as an argument but this is throwing an error for me
## bearded seal
sdmdata_beard <- data.frame(cbind(pb_beard, rbind(presvals_beard, absvals_beard)))
sdmdata_beard %>% count(pb_beard)
## walrus
sdmdata_wal <- data.frame(cbind(pb_wal, rbind(presvals_wal, absvals_wal)))
sdmdata_wal %>% count(pb_wal)

head(sdmdata_spot)
head(sdmdata_beard)
head(sdmdata_wal)


# Pair plot for bioclims 1, 5, 12 to investigate collinearity
# Pair plot for sea ice and shipping to investigate collinearity??
# pairs(sdmdata[,c(2,5,16)], cex = 0.1) # saved plot 

# Pairs plot for temp bioclims and precip bioclims
# pairs(sdmdata[,c(1,5,6,8,9,10,11)], cex = 0.1) # temp
# pairs(sdmdata[,c(12,13,14,16,17,18,19)], cex = 0.1) # precip

# Save work
# saveRDS(sdmdata_spot, "../Data_Processed/prelim_models/sdmdata_spot.Rds")
# saveRDS(presvals_spot, "../Data_Processed/prelim_models/presvals_spot.Rds")

# saveRDS(sdmdata_beard, "../Data_Processed/prelim_models/sdmdata_beard.Rds")
# saveRDS(presvals_beard, "../Data_Processed/prelim_models/presvals_beard.Rds")

# saveRDS(sdmdata_wal, "../Data_Processed/prelim_models/sdmdata_wal.Rds")
# saveRDS(presvals_wal, "../Data_Processed/prelim_models/presvals_wal.Rds")


# Part 2 ------------------------------------------------------------------
# Model fitting, prediction, and evaluation -------------------------------

## Model fitting

# Load the data if not in the environment
sdmdata_spot <- readRDS("../Data_Processed/prelim_models/sdmdata_spot.Rds")
sdmdata_beard <- readRDS("../Data_Processed/prelim_models/sdmdata_beard.Rds")
sdmdata_wal <- readRDS("../Data_Processed/prelim_models/sdmdata_wal.Rds")

presvals_spot <- readRDS("../Data_Processed/prelim_models/presvals_spot.Rds")
presvals_beard <- readRDS("../Data_Processed/prelim_models/presvals_beard.Rds")
presvals_wal <- readRDS("../Data_Processed/prelim_models/presvals_wal.Rds")


# Fit reduced models with shipping and sea ice
# 1 = spotted seal
# 2 = bearded seal
# 3 = walrus

# reduced shipping and sea ice model for spotted seal
m1_ship_sea <- glm(pb_spot ~  + ice_pred, data = sdmdata_spot) ## left off here

# reduced precipitation model
m_precip <- glm(pb ~ bio_12 + bio_13 + bio_14 + bio_16 + bio_17 + bio_18 + bio_19, data = sdmdata)

# summaries
m_temp %>% summary()
m_precip %>% summary()


# Fit full model with all bioclim variables
m_full <- glm(pb ~ ., data = sdmdata) # ~ . means all the other variables in the df
m_full %>% class() # glm lm
m_full %>% summary()

# Fit null model
m_null <- glm(pb ~ 1, data = sdmdata)
m_null %>% class() # glm lm
m_null %>% summary()

# Absolute value of the AIC value does not matter - it can be negative or positive
# Lowest AIC value is always best

## Dismo model
# Models that are implemented in dismo do not use a formula (and most models
# only take presence points). 

## Model prediction
# Different modeling methods return diff types of 'model' objects. These objects
# can be used with the predict function to make predictions for any combo of 
# values of the independent variables.

predictors_na %>% 
  st_as_stars() %>% 
  as_tibble() %>% 
  dplyr::rename(value = 4) %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ band, scales = "free") # saved plot

# Predict table
bio_1 = c(-7, -5, -3)
bio_2 = c(5, 6, 7.5)
bio_3 = c(15, 25, 100)
bio_4 = c(900, 1000, 1200)
bio_5 = c(11, 13, 15)
bio_6 = c(-22, -20, -18)
bio_7 = c(30, 35, 40)
bio_8 = c(-3, 3, 8)
bio_9 = c(-12, -10, -2)
bio_10 = c(6, 7, 8)
bio_11 = c(-20, -18, -15)
bio_12 = c(400, 450, 600)
bio_13 = c(60, 75, 90)
bio_14 = c(15, 20, 30)
bio_15 = c(35, 40, 55)
bio_16 = c(150, 200, 225)
bio_17 = c(50, 60, 70)
bio_18 = c(125, 150, 200)
bio_19 = c(65, 90, 120)
pd = data.frame(cbind(bio_1, bio_2, bio_3, bio_4, bio_5, 
                      bio_6, bio_7, bio_8, bio_9, bio_10,
                      bio_11, bio_12, bio_13, bio_14, bio_15,
                      bio_16, bio_17, bio_18, bio_19))
pd

# Predict with different models
predict(m_temp, pd)
predict(m_precip, pd)
predict(m_full, pd)
predict(m_null, pd)

# Response curves
response(bc_temp)
response(bc_precip)
response(m_temp)
response(m_precip)
response(m_full)

# Predict suitability scores with raster object
pt <- predict(predictors_na, m_temp)
pp <- predict(predictors_na, m_precip)
pf <- predict(predictors_na, m_full)
pn <- predict(predictors_na, m_null)
pbct <- predict(predictors_na, bc_temp)
pbcp <- predict(predictors_na, bc_precip)
plot(pt, main = "Reduced Model (Temperature)")
plot(pp, main = "Reduced Model (Precipitation)")
plot(pf, main = "Full Model")
plot(pn, main = "Null Model")
plot(pbct, main = "BioClim Model - Temperature")
plot(pbcp, main = "BioClim Model - Precipitation")


# MaxEnt models
maxent()

set.seed(0)
group <- kfold(largha, 5)
pres_train <- largha[group != 1, ]
pres_test <- largha[group == 1, ]


largha_coords <- data.frame(largha$decmlLn, largha$decmlLt)

maxent_full <- maxent(x = predictors_na, p = largha_coords)
plot(maxent_full)
response(maxent_full)

# Background data for training and testing set
extent(study_area)
ext <- extent(study_area)

set.seed(10)
backg <- randomPoints(predictors_na, n = 1000, ext=ext, extf = 12.5) # restrict bg points to 12.5% of specified extent
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

# Transform pres_train and pres_test to spatial points instead of spatial multipoints
pres_train <- pres_train %>% st_cast("POINT")
pres_test <- pres_test %>% st_cast("POINT")


e <- evaluate(pres_test, backg_test, maxent_full, predictors_na)
e

px <- predict(predictors_na, maxent_full, ext=ext, progress='')
par(mfrow=c(1,2))
plot(px, main='Maxent, raw values')


data(wrld_simpl)
plot(wrld_simpl, xlim=c(-170,-170), ylim=c(60,70), axes=TRUE, border='dark grey')
tr <- threshold(e, 'spec_sens')
par(mfrow=c(1,1))
plot(px > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')

# pres_train as spatial object instead of sf df to get it to plot?
pres_train_sp <- as(pres_train, "Spatial")
points(pres_train_sp, pch='+') # there's no error, but this doesn't add any points?



## Plot BioClim model for BSR

# Load base_map
base_map <- st_read("Final_Project/Data_Raw/base_map")

# Load cropped base map
base_crop <- st_read("Final_Project/Data_Processed/base_crop.shp")

# Save CRS for later
myCRS <- st_crs(base_crop)

# Turn pbc raster into pbc stars - (suitability of bioclimate scores)
pbct_stars <- pbct %>% st_as_stars()
pbcp_stars <- pbcp %>% st_as_stars()

# Crop pbc stars - need same CRS
pbct_stars <- pbct_stars %>% st_transform(crs = myCRS)
pbcp_stars <- pbcp_stars %>% st_transform(crs = myCRS)
pbct_bsr <- st_crop(pbct_stars, base_crop) # crop only crops regular grids
pbcp_bsr <- st_crop(pbcp_stars, base_crop)

# Crop largha to base_crop
largha_crop <- st_crop(largha, base_crop)

# Plot + locations
ggplot() +
  geom_stars(data = pbc_bsr) +
  geom_sf(data = base_crop, fill = NA,
          col = "black") +
  geom_sf(data = largha_crop, alpha = 0.4, color = "orange") +
  scale_fill_viridis_c(name = "Predicted Suitability",
                       option = "D") +
  labs(title = "Spotted Seal",
       subtitle = "BioClim Model")

# tmap interactive
# tm_shape(pbc_bsr) +
#   tm_raster(alpha = 0.9, palette = "viridis", 
#             title = "Predicted Suitability") +
#   tm_shape(largha_crop) +
#   tm_dots(size = 0.02, col = "orange", alpha = 0.3)

## Model Evaluation

p <- rnorm(50, mean=0.7, sd=0.3) # presence
a <- rnorm(50, mean=0.4, sd=0.4) # absence

# density plot
tibble(presence = p, absence = a) %>% 
  pivot_longer(1:2, names_to = "type", values_to ="value") %>% 
  ggplot(aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +
  ggtitle("Presence/Absence Density Plot")

# boxplot
tibble(presence = p, absence = a) %>% 
  pivot_longer(1:2, names_to = "type", values_to ="value") %>% 
  ggplot(aes(x = type, y = value, fill = type)) +
  geom_boxplot(width = 0.25)


# Compute the correlation coefficient and AUC with two random variables
# p (presence) has higher values and represents the predicted value for 50 known
# cases (locations) where the species is present
# a (absence) has lower values and represents the predicted value for 50 known cases
# (locations) where the species is absent

# Below creates two variables with random normally distributed values but with diff means and std dev.
# Variables will have different distributions and the 'presence' values will be higher
# than for 'absence'.

par(mfrow=c(1, 2))
plot(sort(p), col='red', pch=21)
points(sort(a), col='blue', pch=24)
legend(1, 0.95 * max(a,p), c('presence', 'absence'),
       pch=c(21,24), col=c('red', 'blue'))
comb <- c(p,a)
group <- c(rep('presence', length(p)), rep('absence', length(a)))
boxplot(comb~group, col=c('blue', 'red'))

# Compute the correlation coefficient and the AUC
group = c(rep(1, length(p)), rep(0, length(a)))
cor.test(comb, group)$estimate
mv <- wilcox.test(p,a)
auc <- as.numeric(mv$statistic) / (length(p) * length(a))
auc # 0.7072

# Divide data into two random sets, one for training Bioclim model and one for
# evaluating the model
samp <- sample(nrow(sdmdata), round(0.75 * nrow(sdmdata)))
traindata <- sdmdata[samp,]
traindata <- traindata[traindata[,1] == 1, 2:9]
testdata <- sdmdata[-samp,]
bc <- bioclim(traindata)
e <- evaluate(testdata[testdata==1,], testdata[testdata==0,], bc)
e

par(mfrow=c(1, 1)) # reset plotting window
plot(e, 'ROC')

# Create presence and background data - bg data only for model testing, don't need to partition
pres <- sdmdata[sdmdata[,1] == 1, 2:9]
back <- sdmdata[sdmdata[,1] == 0, 2:9]

# Partition data into 5 groups - kfold() partitions data instead of using single random sample
k <- 5
group <- kfold(pres, k)
group[1:10]
unique(group)

# Fit and test our model 5 times. Each run, the records corresponding to one of the groups 
# is used to evaluate the model and the other 4 are used to fit the model. Results stored in 'e'.
e <- list()
for (i in 1:k) {
  train <- pres[group != i,]
  test <- pres[group == i,]
  bc <- bioclim(train)
  e[[i]] <- evaluate(p=test, a=back, bc)
}

# Extract true positive rate, true negative rate, and threshold for setting cells to p/a (spec_sens)
auc <- sapply(e, function(x){x@auc})
auc

mean(auc)

sapply( e, function(x){ threshold(x)['spec_sens'] } )



# Summary Data ------------------------------------------------------------


# Spotted seal ------------------------------------------------------------

# Create new df with occurrence points and no geometry attached
largha_df_drop <- all_spot %>%
  st_drop_geometry() %>%
  group_by(year, month, day) %>%
  dplyr::summarize(n=n()) %>%
  ungroup()

# Check classes to make sure year and month cols are numeric
largha_df_drop$year <- as.numeric(largha_df_drop$year)
largha_df_drop$month <- as.numeric(largha_df_drop$month) 
largha_df_drop$day <- as.numeric(largha_df_drop$day)

class(largha_df_drop$year)
class(largha_df_drop$month)
class(largha_df_drop$day)

## For loop to avoid rewriting code each time to look at the sum of pts since a year

# number of months per year
n.months <- n_distinct(largha_df_drop$month)
# unique years in data set, in order from earliest to latest
my.years <- sort(unique(largha_df_drop$year))
# number of years
# n.years <- n_distinct(largha_df_drop$year) ## with NA
n.years <- n_distinct(my.years)
# my.sums will contain each of the cumulative sums across the years
my.sums <- rep(NA, n.years)

## choose one of the time windows for the loop
# month_selected <- seq(1,12);
# time_window = 'year round' ## year round

month_selected <- seq(6,10);
time_window = 'summer' ## summer - june through october

month_selected <- c(11, 12, seq(1,5));
time_window = 'winter' ## winter - november through may

sum_since <- data.frame()
for (i in 1:n.years) {
  my.sums[i] <- largha_df_drop %>%
    dplyr::filter(year >= my.years[i]) %>%
    dplyr::filter(month %in% month_selected) %>%
    dplyr::select(n) %>%
    sum(na.rm = TRUE)
  
  # print out the result for easy checking
  cat(my.years[i], ':', my.sums[i], '\n')
  
  # put the results into a dataframe
  sum_i <- cbind(year = my.years[i], sum = my.sums[i], time_window = time_window)
  sum_since <- rbind(sum_since, sum_i)
}

# Save summary to a csv in a folder called prelim_models nested within Output
# write.csv(sum_since, "../Output/prelim_models/largha_yearRound20230418.csv")


# Bearded seal ------------------------------------------------------------

# Create new df with occurrence points and no geometry attached
barbatus_df_drop <- all_beard %>%
  st_drop_geometry() %>%
  group_by(year, month, day) %>%
  dplyr::summarize(n=n()) %>%
  ungroup()

# Check classes to make sure year and month cols are numeric
barbatus_df_drop$year <- as.numeric(barbatus_df_drop$year)
barbatus_df_drop$month <- as.numeric(barbatus_df_drop$month) 
barbatus_df_drop$day <- as.numeric(barbatus_df_drop$day)

class(barbatus_df_drop$year)
class(barbatus_df_drop$month)
class(barbatus_df_drop$day)

## For loop to avoid rewriting code each time to look at the sum of pts since a year

# number of months per year
n.months <- n_distinct(barbatus_df_drop$month)
# unique years in data set, in order from earliest to latest
my.years <- sort(unique(barbatus_df_drop$year))
# number of years
# n.years <- n_distinct(barbatus_df_drop$year) ## with NA
n.years <- n_distinct(my.years)
# my.sums will contain each of the cumulative sums across the years
my.sums <- rep(NA, n.years)

## choose one of the time windows for the loop
month_selected <- seq(1,12);
time_window = 'year round' ## year round

# month_selected <- seq(6,10);
# time_window = 'summer' ## summer - june through october

# month_selected <- c(11, 12, seq(1,5));
# time_window = 'winter' ## winter - november through may

sum_since <- data.frame()
for (i in 1:n.years) {
  my.sums[i] <- barbatus_df_drop %>%
    dplyr::filter(year >= my.years[i]) %>%
    dplyr::filter(month %in% month_selected) %>%
    dplyr::select(n) %>%
    sum(na.rm = TRUE)
  
  # print out the result for easy checking
  cat(my.years[i], ':', my.sums[i], '\n')
  
  # put the results into a dataframe
  sum_i <- cbind(year = my.years[i], sum = my.sums[i], time_window = time_window)
  sum_since <- rbind(sum_since, sum_i)
}

# Save summary to a csv in a folder called prelim_models nested within Output
# write.csv(sum_since, "../Output/prelim_models/barbatus_yearRound20230418.csv")

# Walrus ------------------------------------------------------------------

# Create new df with occurrence points and no geometry attached
odobenus_df_drop <- all_walrus %>%
  st_drop_geometry() %>%
  group_by(year, month, day) %>%
  dplyr::summarize(n=n()) %>%
  ungroup()

# Check classes to make sure year and month cols are numeric
odobenus_df_drop$year <- as.numeric(odobenus_df_drop$year)
odobenus_df_drop$month <- as.numeric(odobenus_df_drop$month) 
odobenus_df_drop$day <- as.numeric(odobenus_df_drop$day)

class(odobenus_df_drop$year)
class(odobenus_df_drop$month)
class(odobenus_df_drop$day)

## For loop to avoid rewriting code each time to look at the sum of pts since a year

# number of months per year
n.months <- n_distinct(odobenus_df_drop$month)
# unique years in data set, in order from earliest to latest
my.years <- sort(unique(odobenus_df_drop$year))
# number of years
# n.years <- n_distinct(odobenus_df_drop$year) ## with NA
n.years <- n_distinct(my.years)
# my.sums will contain each of the cumulative sums across the years
my.sums <- rep(NA, n.years)

## choose one of the time windows for the loop
month_selected <- seq(1,12);
time_window = 'year round' ## year round

# month_selected <- seq(6,10);
# time_window = 'summer' ## summer - june through october

# month_selected <- c(11, 12, seq(1,5));
# time_window = 'winter' ## winter - november through may

sum_since <- data.frame()
for (i in 1:n.years) {
  my.sums[i] <- odobenus_df_drop %>%
    dplyr::filter(year >= my.years[i]) %>%
    dplyr::filter(month %in% month_selected) %>%
    dplyr::select(n) %>%
    sum(na.rm = TRUE)
  
  # print out the result for easy checking
  cat(my.years[i], ':', my.sums[i], '\n')
  
  # put the results into a dataframe
  sum_i <- cbind(year = my.years[i], sum = my.sums[i], time_window = time_window)
  sum_since <- rbind(sum_since, sum_i)
}

# Save summary to a csv in a folder called prelim_models nested within Output
# write.csv(sum_since, "../Output/prelim_models/odobenus_yearRound20230418.csv")



# Preliminary data visualization ------------------------------------------

library(pgirmess) # For bounding box
library(RColorBrewer)


# Import base map - this is already projected 
base_map <- st_read("../../ATP-MarineMammal-DataRaw/base_map")
# Import study area polygon
study_area <- st_read("../../ATP-MarineMammal-DataRaw/study_area")

# https://gis.stackexchange.com/questions/230900/crop-simple-features-object-in-r
# Read xmin, ymin, xmax, and ymax values with st_bbox()
# st_bbox(base_map)
# st_bbox(study_area)

# Crop base map to just Bering Strait Region area (based on study_bbox values)
# Crop base map based on base_map and study area bbox values
base_crop <- st_crop(base_map, c(xmin = -2173499, ymin = 159766, xmax = 1100000, ymax = 3000000))
# Save this
# st_write(base_crop, "../Data_Processed/base_map/base_crop.shp")

# Plot all spotted seal occurrence points against cropped base map
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = all_spot, mapping = aes(color = year)) +
  labs(
    title = "Spotted seal occurrence in the Bering Strait Region",
    caption = "Source: Global Biodiversity Information Facility")

# Isolate number of rows after 2000
largha_2000 <- all_spot[all_spot$year > 1999, ]

ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = largha_2000, mapping = aes(color = year)) +
  labs(
    title = "Spotted seal occurrence in the Bering Strait Region",
    caption = "Source: Global Biodiversity Information Facility")



# Load in makeMap function from X_MapFunction.R script
source("X_MapFunction.R")
# Inputs: 
#### basemap: a shapefile containing the outlines of landmasses within the study area
#### studyarea: a shapefile containing a single polygon that defines the boundaries of the study area
#### pts: a shapefile containing the locations of observed marine mammals 
#### maptitle: a text string with the desired title for each map (No title if not specified)
#### colorcol: a text string specifying the column name used to color the points 

datadir <- "../../ATP-MarineMammal-DataRaw/"
# Load in data for function 
bmap <- st_read(paste0(datadir, "base_map/AK_CAN_RUS.shp"))
# bmap <- bmap %>% st_transform(3338)

rangepolys <- st_read("../Output/OdobenusRosmarus_ExtentOfRange/OdobenusRosmarus_ExtentOfRange.shp"))
rangepoly <- rangepolys %>% dplyr::filter(study_desc == "Summer and fall extent of range")

# Run function for summer locations
locs_summer <- st_read() # Figure what .shp output to upload 
# june_mapping_odobenus_integrated20230210.shp")
testseason <- makeSeasonMap(basemap=bmap, 
                            rangepoly = rangepoly,
                            pts=locs_summer, 
                            maptitle = NA, 
                            colorcol = "year", 
                            colorname = "Year")
testseason
ggsave(filename = "../Figures/WalrusLocations_Summer.png", plot = testseason, width=10, height=8, units="in")

# Run function for fall locations
locs_fall <- st_read("../Data_Processed/Odobenus_DataIntegration_20230116/Fall/sep_mapping_odobenus_integrated20230210.shp")
testseason <- makeSeasonMap(basemap=bmap, 
                            rangepoly = rangepoly,
                            pts=locs_fall, 
                            maptitle = NA, 
                            colorcol = "year", 
                            colorname = "Year")
testseason
ggsave(filename = "../Figures/WalrusLocations_Fall.png", plot = testseason, width=10, height=8, units="in")

# Run function with inputs described in "MapFunction.R" script. 
alllocs <- rbind(locs_fall, locs_summer)
testrange <- makeRangeMap(basemap=bmap, 
                          rangepoly = rangepoly,
                          pts=alllocs, 
                          maptitle = NA, 
                          colorcol = "year", 
                          colorname = "Year")
testrange

# Save output

ggsave(filename = "../Figures/WalrusRangetest2.png", plot = testrange, width=10, height=8, units="in")






# Graveyard ---------------------------------------------------------------


# # Spotted seal tidying ----------------------------------------------------
# 
# # Remove NAs from latitude and longitude columns
# all_spot <- all_spot[!is.na(all_spot$decimalLat), ]
# 
# # Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
# all_spot <- st_as_sf(all_spot, coords = c("decimalLon", "decimalLat"), crs = 4326, remove=FALSE) 
# 
# # Transform to Alaska Albers projection
# all_spot <- all_spot %>% st_transform(crs = 3338)
# 
# # Check what projection it's in
# # st_crs(all_spot)
# 
# 
# # Bearded seal tidying ----------------------------------------------------
# 
# # Remove NAs from latitude and longitude columns
# all_beard <- all_beard[!is.na(all_beard$decimalLat), ]
# 
# # Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
# all_beard <- st_as_sf(all_beard, coords = c("decimalLon", "decimalLat"), crs = 4326, remove=FALSE) 
# 
# # Transform to Alaska Albers projection
# all_beard_3338 <- all_beard %>% st_transform(crs = 3338)
# 
# # Check what projection it's in
# # st_crs(all_spot)
# 
# 
# # Walrus tidying ----------------------------------------------------------
# 
# # Remove NAs from latitude and longitude columns
# all_walrus <- all_walrus[!is.na(all_walrus$decimalLat), ]
# 
# # Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
# all_walrus <- st_as_sf(all_walrus, coords = c("decimalLon", "decimalLat"), crs = 4326, remove=FALSE) 
# 
# # Transform to Alaska Albers projection
# all_walrus_3338 <- all_walrus %>% st_transform(crs = 3338)
# 
# # Check what projection it's in
# # st_crs(all_spot)
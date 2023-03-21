# Sydney Waloven
# 19Mar2023
# This script was adapted from my final project for IBIO870 Spatial Ecology.
# The purpose of this script is to create preliminary habitat suitability models
# using the MaxEnt code from the final project script as a starting point.



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
library(tmap)
library(here)
library(dismo)
library(stars)


# Importing and cleaning data ---------------------------------------------

# Import integrated spotted seal data
all_seals <- read.csv("../Data_Processed/largha_integrated20230210.csv")

head(all_seals)

# Save lat/lon cols
# use dplyr::select

# Assign projection using lat/lon cols to WGS84


# Transform projection
# SW19Mar2023
# Transform to Alaska Albers projection
all_seals_3338 <- all_seals %>% st_transform(crs = 3338)

# Check what projection it's in
# st_crs(all_seals)


# Check to see if it's properly projected (NAD83 / Alaska Albers)
st_crs(all_seals_3338)

View(all_seals_merge)


# Summary data ------------------------------------------------------------

# Create new df with occurrence points and no geometry attached
spotted_df_drop <- all_seals_merge %>%
  st_drop_geometry() %>%
  group_by(year, month, day) %>%
  dplyr::summarize(n=n()) %>%
  ungroup()

# Check classes to make sure year and month cols are numeric
spotted_df_drop$year <- as.numeric(spotted_df_drop$year)
spotted_df_drop$month <- as.numeric(spotted_df_drop$month) 
spotted_df_drop$day <- as.numeric(spotted_df_drop$day)

class(spotted_df_drop$year)
class(spotted_df_drop$month)
class(spotted_df_drop$day)

## For loop to avoid rewriting code each time to look at the sum of pts since a year

# number of months per year
n.months <- n_distinct(spotted_df_drop$month)
# unique years in data set, in order from earliest to latest
my.years <- sort(unique(spotted_df_drop$year))
# number of years
# n.years <- n_distinct(spotted_df_drop$year) ## with NA
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
  my.sums[i] <- spotted_df_drop %>%
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

# Save summary to a csv
# write.csv(sum_since, "Final_Project/Output/sum_yearRound.csv")



# Preliminary data visualization ------------------------------------------


# Import base map - this is already projected 
base_map <- st_read("Final_Project/Data_Raw/base_map")

# https://gis.stackexchange.com/questions/230900/crop-simple-features-object-in-r
# Read xmin, ymin, xmax, and ymax values with st_bbox()
# st_bbox(base_map)
study_area <- st_read("Final_Project/Data_Raw/study_area")
# st_bbox(study_area)

# Crop base map to just Bering Strait Region area (based on study_bbox values)
# Crop base map based on base_map and study area bbox values
base_crop <- st_crop(base_map, c(xmin = -2173499, ymin = 159766, xmax = 1661246, ymax = 3000000))
# Save this
# st_write(base_crop, "Final_Project/Data_Processed/base_crop.shp")

# Plot all spotted seal occurrence points against cropped base map
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = all_seals_merge, mapping = aes(color = year)) +
  labs(
    title = "Spotted seal occurrence in the Bering Strait Region",
    caption = "Source: Global Biodiversity Information Facility")

# Isolate number of rows after 2000
all_seals_2000 <- all_seals_merge[all_seals_merge$year > 1999, ]

# Plotting points after 2000
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = all_seals_2000, mapping = aes(color = year)) +
  labs(
    title = "Spotted seal occurrence points since 2000 in the Bering Strait Region",
    caption = "Source: Global Biodiversity Information Facility")



# Preparing species occurrences for dismo ---------------------------------

# Select the records that have decimalLongitude and latitude data
# Can load all_seals_merge data here also instead of running above
all_seals_merge <- st_read("Final_Project/Data_Processed/all_seals_merge.shp")
largha <- all_seals_merge

# la.geo <- subset(largha, !is.na(decimalLon) & !is.na(decimalLat))
# dim(la.geo) # number of occurrences match the number in original df which means there were no NAs


# Dismo -------------------------------------------------------------------

## Environmental Data

# Create the folders (directories) "data"
# if they exist already, this command won't over-write them.
# create directory called "data"
dir.create("Final_Project/data", recursive = TRUE)

# Download WorldClim 2 data and unzip it
# Refer to the URL for your choice of data:

# Download 10 minute res worldclim V 2.1
wc_url_10m <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_10m_bio.zip"

# 10 minute per degree solution data in "data" folder
# Download file
download.file(url = wc_url_10m, destfile = here("Final_Project/data", "wc_10m.zip"),
              mode = "wb")
# Unzip file
unzip(zipfile = here("Final_Project/data", "wc_10m.zip"), exdir = here("Final_Project/data"))
# Load the data 
bioclim_files <- list.files(here("Final_Project/data"), pattern = "wc2.1_10m_bio_",
                            full.names = TRUE)

# Create a RasterStack of predictor variables
predictors <- stack(bioclim_files)
# Plot to check
predictors %>% plot()

# Create bounding box for extent of BSR using study_area extent
study_area <- st_read("Final_Project/Data_Raw/study_area")

# Make study_area spatial first
study_area_sp <- as(study_area, "Spatial")
bbox_sp <- bbox(study_area_sp)

# Now crop
predictors_na <- crop(predictors, bbox_sp)

# Let's also change names for downstream modelling
names(predictors_na)
bioclim_names <- names(predictors_na)
bioclim_names <- str_remove(bioclim_names, "wc2.1_10m_")
names(predictors_na) <- bioclim_names
# Look at names information to see if they changed
predictors_na

# Check bioclim data - should be the BSR
plot(predictors_na)

# Now we have predictor variables (rasters) and occurrences
# Need to extract the values of the predictors at the locations of the points
# Need to convert largha to SpatialPointsDataFrame instead of multipoints df first

# Cast geometry to another type with st_cast()
largha_sp <- largha %>% st_cast("POINT")


## Extracting values from rasters
presvals <- raster::extract(predictors_na, largha_sp)

# Setting random seed to always create the same random set of points for this example
set.seed(0)

# Changed my # of random points to 4500 because I have around 4500 seal presences
backgr <- randomPoints(predictors_na, 4500)
absvals <- extract(predictors_na, backgr)
# pb indicates whether this is a presence/background point
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
# Create data tibble for sdm
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata %>% count(pb) # Nan originally had sort = TRUE as an argument but this is throwing an error for me

head(sdmdata)

# Pair plot for bioclims 1, 5, 12 to investigate collinearity
pairs(sdmdata[,c(2,5,16)], cex = 0.1) # saved plot 

# Pairs plot for temp bioclims and precip bioclims
pairs(sdmdata[,c(1,5,6,8,9,10,11)], cex = 0.1) # temp
pairs(sdmdata[,c(12,13,14,16,17,18,19)], cex = 0.1) # precip

# Save work
# saveRDS(sdmdata, "Final_Project/Data_Processed/sdm.Rds")
# saveRDS(presvals, "Final_Project/Data_Processed/pvals.Rds")



# Part 2 ------------------------------------------------------------------
# Model fitting, prediction, and evaluation -------------------------------

## Model fitting

# Load the data if not in the environment
sdmdata <- readRDS("Final_Project/Data_Processed/sdm.Rds")
presvals <- readRDS("Final_Project/Data_Processed/pvals.Rds")


# Fit reduced model with bioclims 1, 5, 6, 12, and 19
# Bioclims: 1 (annual mean temp), 5 (max temp of warmest month), 6 (min temp of coldest month),
# 12 (annual precip), 19 (precipitation of coldest quarter)

# reduced temperature model
m_temp <- glm(pb ~ bio_1 + bio_5 + bio_6 + bio_8 + bio_9 + bio_10 + bio_11, data = sdmdata)

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
# Models that are implemented in dismo do not use a formula (and most models only take presence points). 
# Bioclim is an example. It only uses presence data, so we use ‘presvals’ instead of ‘sdmdata’.
bc_temp <- bioclim(presvals[,c('bio_1', 'bio_5', 'bio_6', 'bio_8', 'bio_9', 'bio_10', 'bio_11')])
bc_temp %>% class() # dismo

bc_temp

bc_temp %>% pairs()


bc_precip <- bioclim(presvals[,c('bio_12', 'bio_13', 'bio_14', 'bio_16', 'bio_17', 'bio_18', 'bio_19')])
bc_precip %>% class()

bc_precip

bc_precip %>% pairs()



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



# Graveyard ---------------------------------------------------------------

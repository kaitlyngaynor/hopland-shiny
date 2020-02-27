library(here)
library(overlap)
library(maptools)
library(lubridate)
library(tidyverse)

# Import and merge record tables ---------------------------------------------

# just phase 1 for now
records <- read_csv("~/Dropbox/projects/Hopland - Kaitlyn dissertation/R/hopland-camera-traps/data/recordtable_phase1_cleaned30sec.csv")

# Fix deer classifications ------------------------------------------------

# actually going to do this in the server... for now, "Species" column is 'Deer" and there are other columns for sex, age, buck class

# Format and scale times ------------------------------------------------------------

# set spatial coordinates
coords <- matrix(c(-123.07, 39.00), nrow=1) %>%
  sp::SpatialPoints(proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

# specify date format
records$Date <- as.POSIXct(records$Date, 
                           format = "%m/%d/%y",
                           tz = "North_America/Los_Angeles")

# convert time to radians (could be done more efficiently with pipe)
records$Time.Corrected <- hms(records$Time)
records$Time.Decimal <- records$Time.Corrected$hour + records$Time.Corrected$minute/60 + records$Time.Corrected$second/3600
records$Time.Scaled <- records$Time.Decimal / 24
records$Time.Radians <- records$Time.Scaled * 2 * pi
# calculate suntime using function from overlap package, and coordinates and dates as formatted above
records$Time.Sun <- sunTime(records$Time.Radians, records$Date, coords)


# Drop records outside of operation dates ------------------------------------------------------------

metadata <- read_csv("~/Dropbox/projects/Hopland - Kaitlyn dissertation/R/hopland-camera-traps/data/camera_operation_phase1.csv")

# get the dates into date format
#metadata[, 2:ncol(metadata)] <- lapply(metadata[, 2:ncol(metadata)], as.Date, format = "%m/%d/%y")

# hardcoding for now; there are lat/long and notes columns that are making this not work as it did for Gorongosa data
metadata[, 4:7] <- lapply(metadata[, 4:7], as.Date, format = "%m/%d/%y")

records <- left_join(records, metadata) # join by camera

# label records to drop if outside of operation date (either before start, after end, or during problem window)
records$drop <- FALSE  # create default of false
for (i in 1:nrow(records)) {
  if (records$Date[i] < records$Start[i]) {
    records$drop[i] <- TRUE}
  else if (records$Date[i] > records$End[i]) {
    records$drop[i] <- TRUE}
  else if ((is.na(records$Problem1_from[i]) = FALSE) & (records$Date[i] > records$Problem1_from[i]) & (records$Date[i] < records$Problem1_to[i])) {
    records$drop[i] <- TRUE}
  else {
    records$drop[i] <- FALSE}
}

summary(records$drop) # there are 137 (out of 38895)

# exclude records outside of operation dates
records <- records[records$drop == FALSE,]

# take only columns we want/need
records <- select(records, Camera, Species, Classification, Sex, Age, BuckClass, DateTimeOriginal, Date, Time, delta.time.secs, Time.Sun)

# NOT RELEVANT FOR HOPLAND DATA
## Merge with species metadata ---------------------------------------------
#
## bring in species traits
#species <- read_csv(here::here('data', '2018spp_kingdon.csv')) %>%
#  rename(Species = CommName) # rename to match name of column in records
#
## join records and traits
#records <- left_join(records, species)
#
## remove those with NA for common name full (this includes setup, ghost, unknown)
#records <- drop_na(records, CommName_Full)


# Export cleaned file ---------------------------------------------

write_csv(records, here::here('data', 'recordtable_hopland_for_shiny.csv'))

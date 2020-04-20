library(here)
library(overlap)
library(maptools)
library(lubridate)
library(tidyverse)

# Import and merge record tables ---------------------------------------------

phase1 <- read_csv("~/Documents/github-repos/hopland/shared-repositories/camera-traps/data/recordtable_phase1_cleaned30sec.csv")
phase2 <- read_csv("~/Documents/github-repos/hopland/shared-repositories/camera-traps/data/recordtable_phase2_cleaned30sec.csv")
phase3 <- read_csv("~/Documents/github-repos/hopland/shared-repositories/camera-traps/data/recordtable_phase3_cleaned30sec.csv")

records <- bind_rows(phase1, phase2, phase3)


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

# Export cleaned file ---------------------------------------------

write_csv(records, here::here('data', 'recordtable_hopland_for_shiny.csv'))

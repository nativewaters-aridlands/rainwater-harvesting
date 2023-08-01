##########                Rainwater Harvesting Project                  ###########
###    Code to wrangle rainwater harvesting data from PRISM Climate Group       ###
###                     for AquaCrop Model climate input                        ###
##                       By: Brianda Hernandez Rosales                          ###
###                         Native Water on Arid Lands                          ###

# Load in Libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)

# Read in PRISM Climate Group Dataset that was downloaded from the PRISM website for the desired area and timeframe
# Set dataset as variable and use skip to remove the first 10 lines of metadata from the dataset
raw_data <- read_csv("data-raw/2023_prism_Peach_Springs_AZ.csv", skip = 10)

# Separate the Date into 2 columns for year and month
raw_data <- raw_data %>% separate(Date, c("Year", "Month"))

# Function to calculate the water year
calculate_water_year <- function(Year, Month) {
  ifelse(Month >= 10, as.numeric(Year) + 1, as.numeric(Year))
}

# Add the water year as a new column using the mutate()
raw_data <- raw_data %>%
  mutate(water_year = calculate_water_year(Year, Month))

# Group the months by the water year to get the annual total for precipitation depths with the group_by() and summarize()
wy_data <- raw_data %>%
  group_by(water_year) %>%
  summarise(annual_total = sum(`ppt (inches)`, na.rm = TRUE))

# Calculate the long-term average for precipitation
long_term_average <- mean(wy_data$annual_total)

# Calculate the dry and wet years from the long term precipitation average
normal_year <- (100 / 100 * long_term_average)
dry_year <- (80 / 100 * long_term_average)
wet_year <- (120 / 100 * long_term_average)

## Classify the water years into normal, dry and wet precipitation years

# Create new column and use ifelse statements for classification
wy_data$class <-
  ifelse(wy_data$annual_total >= wet_year, "Wet",
    ifelse(wy_data$annual_total <= dry_year, "Dry", "Normal")
  )

# Apply classification to the months by expanding the dataframe and removing the annual_total column
rwh_data <- merge(raw_data, wy_data[-2], by = "water_year")

## Group_by precipitation classification to get the monthly average for the indicated time frame -
## this information will be used in the AquaCrop Model

# Normal
normal_data <- rwh_data %>%
  group_by(Month, class) %>%
  filter(class %in% c("Normal")) %>%
  summarise(across(c(`ppt (inches)`, `tmin (degrees F)`, `tmax (degrees F)`), list(mean = mean)))

# Dry
dry_data <- rwh_data %>%
  group_by(Month, class) %>%
  filter(class %in% c("Dry")) %>%
  summarise(across(c(`ppt (inches)`, `tmin (degrees F)`, `tmax (degrees F)`), list(mean = mean)))

# Wet
wet_data <- rwh_data %>%
  group_by(Month, class) %>%
  filter(class %in% c("Wet")) %>%
  summarise(across(c(`ppt (inches)`, `tmin (degrees F)`, `tmax (degrees F)`), list(mean = mean)))


# Combine to a single dataframe using the rbind() if desired
aquacrop_data <- rbind(normal_data, dry_data, wet_data)

# Export to CSV for easier visualization if desired
write_csv(aquacrop_data, file = "data-derived/Peach_Springs_AZ_aquacrop.csv")

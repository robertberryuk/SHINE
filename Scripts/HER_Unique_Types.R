
# SCRIPT TO GENERATE UNIQUE HER Types (field = "MonType") from the LDNP HER database

# ----------------------------------------------------------------------------------
# Libs ####
library(tidyverse)
library(here)

# ----------------------------------------------------------------------------------
# Import data ####
# HER Points
HER.point <- read_csv(here("In", "vwHER_HE_MONUMENTSPoint.csv"))
HER.line <- read_csv(here("In", "vwHER_HE_MONUMENTSLine.csv"))
HER.poly <- read_csv(here("In", "vwHER_HE_MONUMENTSPolygon.csv"))

# ---------------------------------------------------------------------------------
# Filter out SHINE-only records (i.e HER features that have a "SHINEuid") and remove duplicates
# HER point
HER.point.SHINE <- HER.point %>% 
  filter(!is.na(SHINEuid)) %>% 
  distinct() %>% 
  mutate(Spatial_Type = "Point")
# HER line
HER.line.SHINE <- HER.line %>% 
  filter(!is.na(SHINEuid)) %>% 
  distinct() %>% 
  mutate(Spatial_Type = "Line")
# HER poly
HER.poly.SHINE <- HER.poly %>% 
  filter(!is.na(SHINEuid)) %>% 
  distinct() %>% 
  mutate(Spatial_Type = "Polygon")

# ---------------------------------------------------------------------------------
# Bind points, lines, and polygons together into one table
HER.all <- rbind(HER.point.SHINE, HER.line.SHINE, HER.poly.SHINE)

# ---------------------------------------------------------------------------------
# Get unique MonTypes from combined HER table
HER.type.unique <- as.tibble(unique(HER.all$MonType))

# Export as CSV file
write_csv(HER.type.unique, (here("Out", "HER_MonType_Unique.csv")))





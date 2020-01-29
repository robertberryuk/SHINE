# Script to analyse (spatially) the linked SHINE-HER data

# For each study area (13 valleys plus Derwent Catchment) calculate:
  # 1. Number of SHINE features
  # 2. Number of SHINE features by PeriodStrt date
  # 3. Number of SHINE features by broad type
  # 4. Number of listed buildings - scheduled monuments??


# Libraries ------------------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(plyr)
library(beepr)
library(conflicted)
conflict_prefer("here", "here")
conflict_prefer("summarise", "dplyr")
conflict_prefer("count", "dplyr")


# STEP 1 - Import Data----------------------------------------------------------------------------

# SHINE Data
SHINE <- st_read(here("Out", "SHINE_HER_Linked.shp"))
# Set CRS to BNG
st_crs(SHINE) <- 27700

# 13 valleys
Vals <- st_read(here("In", "Valleys_WHS.shp"))
# Set CRS to BNG
st_crs(Vals) <- 27700

# Upper Derwent Catchment
Derwent <- st_read(here("In", "Upper_Derwent.shp"))
st_crs(Derwent) <- 27700


# STEP 2 - Spatial intersections---------------------------------------------------------------------

# 13 Valleys
int.Vals.sf <- st_intersection(SHINE, Vals)
# Non geo version for stats
int.Vals <- int.Vals.sf
st_geometry(int.Vals) <- NULL

# Upper Derwent
int.Derwent.sf <- st_intersection(SHINE, Derwent)
# Non geo version for stats
int.Derwent <- int.Derwent.sf
st_geometry(int.Derwent) <- NULL


# STEP 3 - Count number of SHINE features per area

# 13 valleys
SHINE.count.Vals <- int.Vals %>% 
  group_by(Valley) %>% 
  summarise(Count_SHINE = n()) %>% 
  dplyr::arrange(Valley) 

# Upper Derwent
SHINE.count.Derwent <- int.Derwent %>% 
  # no need to group as single area polygon
  summarise(Count_SHINE = n())
  # no need to sort as single polygon


# STEP 4 - Summary of PeriodStrt dates by area

# 13 valleys
# https://stackoverflow.com/questions/40454138/spread-columns-by-count-in-r-dplyr
SHINE.Periods.Vals <- int.Vals %>% 
  select(Valley, PrdStrt) %>% 
  count(Valley, PrdStrt) %>% 
  spread(PrdStrt, n, fill = 0)
# Arrange PrdStrt columns in order of age - see Jeremy's table on values

# Upper Derwent
SHINE.Periods.Derwent <- int.Derwent %>% 
  select(PrdStrt) %>% 
  count(PrdStrt) %>% 
  spread(PrdStrt, n, fill = 0)
# Add "Valley" Column and Populate with "Upper Derwent" to allow RBIND to other other 13 valleys table
SHINE.Periods.Derwent$Valley <- "Upper Derwent"
SHINE.Periods.Derwent <- SHINE.Periods.Derwent %>%
  select(Valley, everything())
# Rearrange columns in date order as per 13 valleys summary table


# STEP 5 - Summary of Broad FISH Types by area
SHINE.FISH.Vals <- int.Vals %>% 
  select(Valley, FISH_B_) %>% 
  count(Valley, FISH_B_) %>% 
  spread(FISH_B_, n, fill = 0)

# Do for Upper Derwent

# STEP 6 - Summary of SHINE "significance" by area
SHINE.Signif.Vals <- int.Vals %>% 
  select(Valley, signfcn) %>% 
  count(Valley, signfcn) %>% 
  spread(signfcn, n, fill =  0)

# STEP 7 - Summary of SHINE "Form" by areas
SHINE.Form.Vals <- int.Vals %>% 
  select(Valley, shn_frm) %>% 
  count(Valley, shn_frm) %>% 
  spread(shn_frm, n, fill = 0)

  
  


  




# Audio notification when script has finished running
beepr::beep()


# ----------------------------
# Libs
library(tidyverse)
library(sf)
library(here)

#-----------------------------
# Import data and assign BNG CRS

# 13 valleys
vals.13 <- st_read(here("In", "Valleys_WHS.shp"))
st_crs(vals.13) <- 27700
# SHINE data
shine <- st_read(here("In", "SHINE_LDNPA.shp"))
st_crs(shine) <- 27700
# Upper Upper Derwent T&T area
derwent <- st_read(here("In", "Upper_Derwent.shp"))
st_crs(derwent) <- 27700
# HER
her.poly <- st_read(here("In", "vwHER_HE_MONUMENTSPolygon.shp"))
st_crs(her) <- 27700


#-----------------------------
# Data prep

# Remove geom from her.poly for merging to SHINE data
st_geometry(her.poly) <- NULL
# Remove letters from SHINE ID string in shine to match Shine ID in Her data
shine$shine_uid2 <- gsub("[a-zA-Z ]", "", shine$shine_uid)
#Merge with SHINE data
shine.her.poly <- merge(shine, her.poly, by.x="shine_uid2", by.y="SHINEuid")



#------------------------------
# Spatial intersections

# Extract 3 selected valleys for evaluation
vals.3 <- vals.13 %>% 
  filter(Valley %in% c("Haweswater", "Eskdale", "Langdale"))

# Intersect with SHINE data
int.her.poly <- st_intersection(shine, vals.3)




Haweswater
Eskdale
Great Langdale
Upper Derwent T&T area

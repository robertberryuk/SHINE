#SHINE only analysis of 3 selected valleys + Derwent catchment

# ----------------------------
# Libs
library(tidyverse)
library(sf)
library(here)
library(dplyr)

#-----------------------------
# Import data
shine <- st_read(here("In", "SHINE_LDNPA.shp"), stringsAsFactors = FALSE)
st_crs(shine) <- 27700
# Upper Upper Derwent T&T area
derwent <- st_read(here("In", "Upper_Derwent.shp"))
st_crs(derwent) <- 27700
# 3 valleys
vals <- st_read(here("In", "Three_Valleys.shp"))
st_crs(vals) <- 27700

#------------------------------

# 3 Valleys
# Intersect data
int_vals <- st_intersection(shine, vals)
# Drop unwated columns
int_vals <- int_vals %>% 
  select(-c(shine_name, web_url, last_edit, ID, shine_uid))
# Drop geom
st_geometry(int_vals) <- NULL

# Derwent
# Intersect data
int_der<- st_intersection(shine, derwent)
# Drop unwated columns
int_der <- int_der%>% 
  select(-c(shine_name, web_url, last_edit, ID, shine_uid))
# Drop geom
st_geometry(int_der) <- NULL
  


#---------------------------------------
# 3 Valleys analysis
# Shine_Form 
# Summarise data by count and percentage
vals.form <- int_vals %>% 
  select(-significan) %>% 
  group_by(Valley, shine_form)  %>%
  summarise(tot = n())
#> Spread the data so that years become columns
vals.form.spread <- spread(vals.form, shine_form, tot)


# Significance
vals.signif <- int_vals %>% 
  select(-shine_form) %>% 
  group_by(Valley, significan) %>%
  summarise(tot = n())

#> Spread the data so that years become columns
vals.signifi.spread <- spread(vals.signif, significan, tot)

#----------------------------------------------
# Dewent catchment analysis
# Shine_Form 
# Summarise data by count and percentage
der.form <- int_der %>% 
  select(-significan) %>% 
  group_by(CATCHMENT, shine_form)  %>%
  summarise(tot = n())
#> Spread the data so that years become columns
der.form.spread <- spread(der.form, shine_form, tot)
#Change catchtment col name to "valley"
names(der.form.spread)[names(der.form.spread) == 'CATCHMENT'] <- 'Valley'



# Significance
der.signif <- int_der %>% 
  select(-shine_form) %>% 
  group_by(CATCHMENT, significan) %>%
  summarise(tot = n())

#> Spread the data so that years become columns
der.signif.spread <- spread(der.signif, significan, tot)
#Change catchtment col name to "valley"
names(der.signif.spread)[names(der.signif.spread) == 'CATCHMENT'] <- 'Valley'


#---------------------------------------------
# Bind the results together
# Shine form
shine_form_results <- rbind(vals.form.spread, der.form.spread)
#Export
write_csv(shine_form_results, (here("Out", "Shine_Form_by_Area.csv")))

# Significance
shine_signif_results <- rbind(vals.signifi.spread, der.signif.spread)
#Export
write_csv(shine_signif_results, (here("Out", "Shine_Significance_by_Area.csv")))















#> Change NAs to zeros
vals.form.spread 
#> Add a totals columns
vals.form.spread$Total <- vals.form.spread$`Above-ground feature(s)` +
  vals.form.spread$`Above + below-ground feature(s)` +
  vals.form.spread$`Below-ground feature(s)` +
  vals.form.spread$`Structure(s)`+
  vals.form.spread$`Structure(s) + above-ground feature(s)`+
  vals.form.spread$`Structure(s) + above + below-ground feature(s)`+
  vals.form.spread$`Structure(s) + below-ground feature(s)`+
  vals.form.spread$`Well-preserved below-ground feature(s)`

  


vals.form.spread_pcent <- vals.form.spread %>%
  mutate_at(vars(2:9), funs(paste0(round(100*./sum(.), 1), "%")))

















st_write(int_vals, (here("Out", "int_vals.shp")))





st_write
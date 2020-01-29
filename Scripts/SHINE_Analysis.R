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
library(reshape2)
conflict_prefer("here", "here") # here conflicst with "here" in plyr
conflict_prefer("mutate", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("arrange", "dplyr")




# STEP 1 - Import Data----------------------------------------------------------------------------

# SHINE Data
SHINE <- st_read(here("Out", "SHINE_HER_Linked", "SHINE_HER_Linked.shp"))
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


# STEP 3 - Count number of SHINE features per area--------------------------------------------------------------------------------
# 13 valleys
SHINE.count.Vals <- int.Vals %>% 
  group_by(Valley) %>% 
  summarise(Count_SHINE = n()) %>% 
  dplyr::arrange(Valley) 
# Upper Derwent
SHINE.count.Derwent <- int.Derwent %>% 
  # no need to group as single area polygon
  summarise(Count_SHINE = n()) %>% 
  dplyr::mutate(Valley = "Derwent ELMS") %>% 
  select(Valley, everything())
  # no need to sort as single polygon
# RBind the Derwent data to the 13 valleys data
SHINE.count.all <- bind_rows(SHINE.count.Vals, SHINE.count.Derwent)
# Arrange valleys in alphabetical order
SHINE.count.all <- SHINE.count.all %>% 
  arrange(Valley)

# Bar chart
plot1.SHINE_count <- ggplot(data=SHINE.count.all, aes(x = reorder(Valley, Count_SHINE), y = Count_SHINE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count_SHINE), vjust = 0.5, hjust = -0.2, size = 2) +
  labs(x = "Valley/area", y = "No. of SHINE features") +
  theme_bw()   +
  coord_flip() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) 
#> Show plot
plot1.SHINE_count
#> Export the plot
ggsave(here("Out", "Plots", "plot1.SHINE_count.png"))
  
  

# STEP 4 - Summary of Aggrg_P dates by area ----------------
# 13 valleys
# https://stackoverflow.com/questions/40454138/spread-columns-by-count-in-r-dplyr
SHINE.Periods.Vals <- int.Vals %>% 
  select(Valley, Aggrg_P ) %>% 
  count(Valley, Aggrg_P ) %>% 
  spread(Aggrg_P , n, fill = 0)
# Arrange Aggrg_P  columns in order of age - see Jeremy's table on values
SHINE.Periods.Derwent <- int.Derwent %>%
  select(Aggrg_P ) %>% 
  count(Aggrg_P ) %>% 
  spread(Aggrg_P , n, fill = 0) %>% 
  dplyr::mutate(Valley = "Derwent ELMS") %>% 
  select(Valley, everything())
# Bind the Derwent data to the 13 valleys data
SHINE.Periods.all <- bind_rows(SHINE.Periods.Vals, SHINE.Periods.Derwent)
# Change NAs to zero
SHINE.Periods.all[is.na(SHINE.Periods.all)] <- 0
# Reorder columns in period order
# First get a list of column names in CSV to copy
colnames(SHINE.Periods.all)
SHINE.Periods.all <- SHINE.Periods.all[c("Valley", "PREHISTORIC (pre-43AD)", "ROMANO-BRITISH (43-410 AD)", "EARLY MEDIEVAL (410-1066)", "MEDIEVAL (1066-1540)", "POST-MEDIEVAL (1540-1900)", "20TH CENTURY", "UNCERTAIN")]

# Bar chart
# Melt the data frame
SHINE.Periods.melt <- melt(SHINE.Periods.all, id.vars = "Valley")
plot2.SHINE_Periods <- ggplot(SHINE.Periods.melt, aes(x = reorder(Valley, value), y=value, fill=factor(variable, levels = c("UNCERTAIN", "20TH CENTURY", "POST-MEDIEVAL (1540-1900)", "MEDIEVAL (1066-1540)", "EARLY MEDIEVAL (410-1066)", "ROMANO-BRITISH (43-410 AD)", "PREHISTORIC (pre-43AD)")))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  # scale_color_brewer(palette = "Set1") +
  labs(x = "Valley/area", y = "No. of features") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw()   +
  coord_flip() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") 
  #> Show plot
  plot2.SHINE_Periods
#> Export the plot
ggsave(here("Out", "Plots", "plot2.SHINE_Periods.png"))


# STEP 5 - Summary of Broad Types by area------------------------------------------------------------------
# 13 valleys
SHINE.FISH.Vals <- int.Vals %>% 
  select(Valley, FISH_B_) %>% 
  count(Valley, FISH_B_) %>% 
  spread(FISH_B_, n, fill = 0)
# Arrange FISH_B_ columns in order of age - see Jeremy's table on values
SHINE.FISH.Derwent <- int.Derwent %>%
  select(FISH_B_) %>% 
  count(FISH_B_) %>% 
  spread(FISH_B_, n, fill = 0) %>% 
  dplyr::mutate(Valley = "Derwent ELMS") %>% 
  select(Valley, everything())
# Bind the Derwent data to the 13 valleys data
SHINE.FISH.all <- bind_rows(SHINE.FISH.Vals, SHINE.FISH.Derwent)
# Change NAs to zero
SHINE.FISH.all[is.na(SHINE.FISH.all)] <- 0

# Prior to plotting, melt the data table and add a newe variable - proportation of value relative to group for use in heatmap table
SHINE.FISH.melt <- melt(SHINE.FISH.all, id.vars = "Valley")
SHINE.FISH.melt <- SHINE.FISH.melt %>% 
  group_by(Valley) %>% 
  mutate(rel_value = value / sum(value))


# Heat map table - see https://rpubs.com/melike/heatmapTable
library(scales) # for "muted" function - not used her ebut used in above example
# Create the heatmap table
plot3.SHINE_FISH_Types <- ggplot(SHINE.FISH.melt, aes(Valley, fct_rev(variable))) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = SHINE.FISH.melt$rel_value, label = SHINE.FISH.melt$value), size = 3) + # write the values
  # geom_text(aes(label = 2) +
  #             geom_text(aes(label = Count_SHINE), vjust = 0.5, hjust = -0.2, size = 2) +
  scale_fill_gradient2(low =  "white", 
                       mid = "green", 
                       high = muted("midnightblue"), 
                       midpoint = 330) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 10,face = "bold"),
        # plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 8,face = "bold")) + 
  # ggtitle("Correlation Plot") + 
  # theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="Feature count")
#> Show plot
plot3.SHINE_FISH_Types
#> Export the plot
ggsave(here("Out", "Plots", "plot3.SHINE_FISH_Types.png"))


# STEP 6 - Summary of SHINE Form by area------------------------------------------------------------------
# 13 valleys
SHINE.Form.Vals <- int.Vals %>% 
  select(Valley, shn_frm) %>% 
  count(Valley, shn_frm) %>% 
  spread(shn_frm, n, fill = 0)
# Arrange shn_frm columns in order of age - see Jeremy's table on values
SHINE.Form.Derwent <- int.Derwent %>%
  select(shn_frm) %>% 
  count(shn_frm) %>% 
  spread(shn_frm, n, fill = 0) %>% 
  dplyr::mutate(Valley = "Derwent ELMS") %>% 
  select(Valley, everything())
# Bind the Derwent data to the 13 valleys data
SHINE.Form.all <- bind_rows(SHINE.Form.Vals, SHINE.Form.Derwent)
# Change NAs to zero
SHINE.Form.all[is.na(SHINE.Form.all)] <- 0


# Prior to plotting, melt the data table and add a newe variable - proportation of value relative to group for use in heatmap table
SHINE.Form.melt <- melt(SHINE.Form.all, id.vars = "Valley")
SHINE.Form.melt <- SHINE.Form.melt %>% 
  group_by(Valley) %>% 
  mutate(rel_value = value / sum(value))


# Heat map table - see https://rpubs.com/melike/heatmapTable
library(scales) # for "muted" function - not used her ebut used in above example
# Create the heatmap table
plot4.SHINE_Form_Types <- ggplot(SHINE.Form.melt, aes(Valley, fct_rev(variable))) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = SHINE.Form.melt$rel_value, label = SHINE.Form.melt$value), size = 3) + # write the values
  scale_fill_gradient2(low =  "white", 
                       mid = "green", 
                       high = muted("midnightblue"), 
                       midpoint = 450) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 10,face = "bold"),
        # plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 8,face = "bold")) + 
  # ggtitle("Correlation Plot") + 
  # theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="Feature count")
#> Show plot
plot4.SHINE_Form_Types
#> Export the plot
ggsave(here("Out", "Plots", "plot4.SHINE_Form_Types.png"))



# STEP 7 - Summary of SHINE Significance by area-----------------------------------------------------------------
# 13 valleys
SHINE.Signif.Vals <- int.Vals %>% 
  select(Valley, signfcn) %>% 
  count(Valley, signfcn) %>% 
  spread(signfcn, n, fill = 0)
# Arrange signfcn columns in order of age - see Jeremy's table on values
SHINE.Signif.Derwent <- int.Derwent %>%
  select(signfcn) %>% 
  count(signfcn) %>% 
  spread(signfcn, n, fill = 0) %>% 
  dplyr::mutate(Valley = "Derwent ELMS") %>% 
  select(Valley, everything())
# Bind the Derwent data to the 13 valleys data
SHINE.Signif.all <- bind_rows(SHINE.Signif.Vals, SHINE.Signif.Derwent)
# Change NAs to zero
SHINE.Signif.all[is.na(SHINE.Signif.all)] <- 0


# Bar chart
# Melt the data frame
SHINE.Signif.melt <- melt(SHINE.Signif.all, id.vars = "Valley")
# Add a factor column to sort High, Low , Medium values in stacked bar chart
# SHINE.Signif.melt$Order <- NULL
# SHINE.Signif.melt$Order[SHINE.Signif.melt$variable == "Low"] <- "1" 
# SHINE.Signif.melt$Order[SHINE.Signif.melt$variable == "Medium"] <- "2" 
# SHINE.Signif.melt$Order[SHINE.Signif.melt$variable == "High"] <- "3" 
# 
# levels(SHINE.Signif.melt$variable) <- c("Low", "Medium", "High")

plot5.SHINE_Signif <- ggplot(SHINE.Signif.melt, aes(x = reorder(Valley, value), y=value, fill=factor(variable, levels = c("High", "Medium", "Low")))) +
  geom_bar(stat = "identity") +
  # scale_fill_brewer(palette = "Set1") +
  # scale_color_brewer(palette = "Set1") +
  labs(x = "Valley/area", y = "No. of features") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw()   +
  coord_flip() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") 
#> Show plot
plot5.SHINE_Signif
#> Export the plot
ggsave(here("Out", "Plots", "plot5.SHINE_Signif.png"))




# STEP 8 - Summary of SHINE Status by area-----------------------------------------------------------------
# 13 valleys

SHINE.Status.Vals <- int.Vals %>% 
  select(Valley, Stts_MA) %>% 
  count(Valley, Stts_MA) %>% 
  spread(Stts_MA, n, fill = 0)
# Arrange Stts_MA columns in order of age - see Jeremy's table on values
SHINE.Status.Derwent <- int.Derwent %>%
  select(Stts_MA) %>% 
  count(Stts_MA) %>% 
  spread(Stts_MA, n, fill = 0) %>% 
  dplyr::mutate(Valley = "Derwent ELMS") %>% 
  select(Valley, everything())
# Bind the Derwent data to the 13 valleys data
SHINE.Status.all <- bind_rows(SHINE.Status.Vals, SHINE.Status.Derwent)
# Change NAs to zero
SHINE.Status.all[is.na(SHINE.Status.all)] <- 0

# Bar chart
# Melt the data frame
SHINE.Status.melt <- melt(SHINE.Status.all, id.vars = "Valley")
# Add a factor column to sort High, Low , Medium values in stacked bar chart
# SHINE.Status.melt$Order <- NULL
# SHINE.Status.melt$Order[SHINE.Status.melt$variable == "Low"] <- "1" 
# SHINE.Status.melt$Order[SHINE.Status.melt$variable == "Medium"] <- "2" 
# SHINE.Status.melt$Order[SHINE.Status.melt$variable == "High"] <- "3" 

# levels(SHINE.Status.melt$variable) <- c("Low", "Medium", "High")

plot6.SHINE_Status <- ggplot(SHINE.Status.melt, aes(x = reorder(Valley, value), y=value, fill=factor(variable, levels = c( "<NA>",  "Scheduled Monument", "Register of Parks and Gardens",  "Listed Building")), Order)) +
  geom_bar(stat = "identity") +
  # scale_fill_brewer(palette = "Set1") +
  # scale_color_brewer(palette = "Set1") +
  labs(x = "Valley/area", y = "No. of features") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw()   +
  coord_flip() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") 
#> Show plot
plot6.SHINE_Status
#> Export the plot
ggsave(here("Out", "Plots", "plot6.SHINE_Status.png"))



# STEP 9 - Export stats tables---------------------------------------------------------------------------------------
write_csv(SHINE.count.all, here("Out", "SHINE_Analysis", "SHINE_counts_by_area.csv"))
write_csv(SHINE.Periods.all, here("Out", "SHINE_Analysis", "SHINE_Periods_by_area.csv"))
write_csv(SHINE.FISH.all, here("Out", "SHINE_Analysis", "SHINE_FISH_Types_by_area.csv"))
write_csv(SHINE.Form.all, here("Out", "SHINE_Analysis", "SHINE_Form_Types_by_area.csv"))
write_csv(SHINE.Signif.all, here("Out", "SHINE_Analysis","SHINE_Significance_Types_by_area.csv"))
write_csv(SHINE.Status.all, here("Out", "SHINE_Analysis","SHINE_Protected_Status_by_area.csv"))


# Audio notification when script has finished running
beepr::beep()


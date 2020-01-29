# Script to update SHINE data with key variables (MonType and PeriodStart) from the HER data 

# Pseudocode -----------------------------------------------------------------------------------------------------------

# REMEMBER there are 3 shapefiloes for HER - POINTS, LINES and POLYGONS - will NEED TO PERFORM THIS ANALYSIS FOR EACH OF THESE LAYERS!!!

# STEP 1 (DONE) - a) Create a lookup table (in Excel) to allow merge of textual descriptions of HER monumnent broad types based on SHINE narrow types. Use the "FISH Thesaurus of Monument Types - Class Names and Definitions" document. b) Create a lookup table to allocate values based on period start date (PeriodStrt).

# STEP 2 (DONE) - Import 1) narrow to broad type lookup table (created by Jeremy) and 2) the broad type code to FISH category description lookup table, and 3) SHINE data and the 3 HER layers (points, lines and polygons)

# STEP 3 (DONE) - Identify and remove any duplicates from HER and SHINE (3 layers)

# STEP 4 (DONE) - Bind the 3 HER layers (i.e. point, line, polygon) into a single layer

# STEP 5 (DONE) - Remove HER dupes 

# STEP 6 (DONE) - Remove any HER records without a SHINE ID (i.e "NA")

# STEP 7 (DONE) - Remove HER records where CandStat = "no". See "SHINE_HER_Workflow_Guidelines_2018v1.4.pdf" - Sites whose character is uncertain to the degree that they are not demonstrably substantive or verifiable, and sites whose extent may be implied by description, but whose extent cannot be represented as a GIS polygon, should be excluded. For example, this would include sites which are known solely from limited documentary sources. Many monuments will have insufficient information available to make an informed judgement on whether they merit a SHINE polygon. This in itself means they must not be included in the SHINE dataset.In such cases the record should be flagged with a ‘SHINE candidate status’ of ‘No’.

# STEP 8 - (RECONSIDER) Merge Jeremy's narrow-to-broad onument type lookup table with the HER "super layer"

# STEP 8 - (DONE) Remove records where Keep_YN = N (these are narrow types flagged for removal by Jeremy) - MAYBE RETHINK THIS AS RECORDS WILL BE LOST - NEED TO INCLUDE AS SHINE RECORDS COMPRISED SOLELY OF NARROW TYPES FOR 'REMOVAL' WILL THEN THEMSELVES BE REMOVED


# STEP 10 - (DONE) For one-to-many (i.e. SHINE to HER) relationships retain the most frequently occurring broad type and retain all records to show records where the number of broad types are equal (tied)

# STEP 11 - (DONE) Rank the FISH broad type categories in order of importance and create a lookup table which can be used to refine selection of the SHINE records where number of different FISH broad types are tied.

# STEP 12 - (DONE) Merge the FISH ranking lookup table with the HER data and select the most important records. Review number of HER records that need to be refined, if any.

# STEP 13 - For one-to-many (i.e. SHINE to HER) relationships retain the earliest PeriodStrt date, using a similar refinement methodology to the FISH borad types above (i.e. using lookup tables and ranking)

# 






# STEP 8 - Merge the SHINE and single HER "super layer" based on SHINE ID - retain all HER records



# Step 10 - If one-to-many use earliest PeriodSrt field

# Joining and processing as per below rules


# DATE Yes, we simply take the EARLIEST PeriodStrt field

# TYPE I can run through the Excel table along the lines we agreed yesterday to inform your first sift of the HER data, this will keep Ken’s powder dry for any work that may arise once you have spotted the ‘one to many’: I agree that we go for most frequent. 




# Libraries ----------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(plyr)
library(beepr)
library(conflicted)
conflict_prefer("here", "here") # here conflicst with "here" in plyr
conflict_prefer("mutate", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarise", "dplyr")



### STEPS (see pseudocode above)


# 1. Create lookups------------------------------------------------------------------------------------------------------------
# Create a lookup table CSV (in Excel) to allow merge of textual descriptions of HER monumnent broad types based on SHINE narrow types. Use the "FISH Thesaurus of Monument Types - Class Names and Definitions" document




# 2. Data import --------------------------------------------------------------------------------------------------------------

# 2.1 - Lookup tables (STEP 2)

# Narrow HER mon type to FISH broad type code lookup
LT.Narrow.Broad <- read_csv(here("In", "HER_MonType_BroadType_Link_JL.csv"))

# Mon broad type code to FISH category description lookup
LT.Broad.FISH <- read_csv(here("In", "HER_NarrowTypes_FISH_Lookup.csv"))

# 2.2 - SHINE data (STEP 3)
SHINE <- st_read(here("In", "SHINE_LDNPA.shp"), stringsAsFactors = FALSE)

# 2.3 - HER attribute data (for points, lines and polygons - no need for spatial join as link is SHINE ID) (STEP 3)
HER.Point <- read_csv(here("In", "vwHER_HE_MONUMENTSPoint.csv"))
HER.Line <- read_csv(here("In", "vwHER_HE_MONUMENTSLine.csv"))
HER.Poly <- read_csv(here("In", "vwHER_HE_MONUMENTSPolygon.csv"))



# 3 - Remove SHINE dupes-------------------------------------------------------------------------------------------------------

# SHINE
SHINE <- unique(SHINE)
# Also remove the prefix "LD" from SHINE ID column
SHINE$shine_uid <- str_replace(SHINE$shine_uid, "LD", "")
# Now change SHINE_uid column to numeric
SHINE$shine_uid <- as.numeric(SHINE$shine_uid)
# Remove records without SHINE ID
SHINE <- SHINE %>% drop_na("shine_uid")
# NO DUPES FOUND



# 4 - Bind HER layers-----------------------------------------------------------------------------------------------------------

# Prior to bind, add a "shape" variable to each HER layer to indicate whether feature is a point, line, or polygon
# Points
HER.Point <- HER.Point %>% 
  mutate(Shape = "Point")
# Lines
HER.Line <- HER.Line %>% 
  mutate(Shape = "Line")
# Polygons
HER.Poly <- HER.Poly %>% 
  mutate(Shape = "Polygon")

# Bind the HER layers
HER <- rbind(HER.Point, HER.Line, HER.Poly)

# Remove unwanted columns from HER layer
HER <- HER %>% 
  select(-Easting, -Northing, -Web_URL, -LastEdit)


# 5 - Remove HER dupes-------------------------------------------------------------------------------------------------------------

HER <- distinct(HER)

# # Code to display the duplicate records, as a checker -
# HER.dupes <- HER %>%
#   group_by_all() %>%
#   filter(n()>1) %>%
#   ungroup() %>%
#   distinct()

# Add unique identifier
HER <- HER %>% 
  mutate(ID_CCRI = row_number())
# Move ID_CCRI column to beginning 
HER <- HER %>%
  select(ID_CCRI, everything())





# 6 - Remove HER no SHINE ID--------------------------------------------------------------------------------------------

# HER.Poly
HER <- HER %>% drop_na("SHINEuid")




# 7  - Filter HER "CandStat"---------------------------------------------------------------------------------------------------
# Remove SHINE IDs from HER where CandStatus is "No"
HER <- HER %>% 
  filter(CandStat =="yes")

# Check number of distinct SHINEuids at this stage where CandStat = "yes"
HER.dist.CandYes <- as.data.frame(unique(HER$SHINEuid))
  


# 8 - Merge broad-narrow lookup-------------------------------------------------------------------------------------------------

# Merge using narrow type columns keeping all HER records
HER <- merge(HER, LT.Narrow.Broad, by.x = "MonType", by.y = "Narrow_Mon_Type", all.x = TRUE)


# # Export a table of HER records where MonType = NULL, for Jeremy to update
# HER.NoMonType <- HER %>% 
#   filter(MonType == "")
# write_csv(HER.NoMonType, here("Out", "HER_No_Mon_Types.csv"))

# # First check number of unique SHINEuid records that will be lost from HER where CandStat = "yes" and KeepYN = "no"
# HER.CandYes.KeepNo <- HER %>% 
#   filter(CandStat =="yes" & Keep_YN == "N")
# 
# # Count number of "N" values in each SHINEuid group
# HER.CandYes.KeepNo.Count <- HER.CandYes.KeepNo %>% 
#   group_by(SHINEuid, Keep_YN) %>% 
#   summarise(countNoKeep = n()) 
# # Join this back to HER
# HER <- merge(HER, HER.CandYes.KeepNo.Count, by.x ="SHINEuid", by.y = "SHINEuid", all.x = TRUE)
# 
# # Count number of SHINEuids in HER record
# HER.SHINEuid.Group.Count <- HER %>% 
#   group_by(SHINEuid) %>% 
# summarise(countSHINEuids = n())
# # Join this back to HER
# HER <- merge(HER, HER.SHINEuid.Group.Count, by.x ="SHINEuid", by.y = "SHINEuid", all.x = TRUE)
# # Now show all records where the number of countNoKeep records = number of countSHINEuids - this will tell us the number of SHINE records that will be lost from discarding KEEPYN = N records
# SHINE.KeepN.Lost <- HER %>% 
#   filter(countSHINEuids == countNoKeep) %>% 
#   group_by(SHINEuid) %>% 
#   summarise(records = n())
# # Create a new table to show team the records that would be lost
# HER.KeepYN.Lost.Export <- merge(SHINE.KeepN.Lost, HER, by.x = "SHINEuid", by.y = "SHINEuid")
# # Extract unique SHINEuids
# HER.KeepYN.Unique.Lost <- as.data.frame(unique(HER.KeepYN.Lost.Export$SHINEuid))
# HER.KeepYN.Unique.Lost$SHINEuid <- HER.KeepYN.Unique.Lost$`unique(HER.KeepYN.Lost.Export$SHINEuid)`
# HER.KeepYN.Unique.Lost$`unique(HER.KeepYN.Lost.Export$SHINEuid)` <- NULL
# # Merge back to HER
# HER.KeepYN.Lost.Export <- merge(HER.KeepYN.Unique.Lost, HER, by.x = "SHINEuid", by.y = "SHINEuid", all.x = TRUE)
# # Export
# write_csv(HER.KeepYN.Lost.Export, here("Out", "SHINE_KEEP_NO_Lost.csv"))


# Remove records where Keep_YN = N (these are narrow types flagged for removal by Jeremy)
HER <- HER %>%
  filter(Keep_YN != "N" | is.na(Keep_YN))


# Update missing BroadTypes using Jeremy's update table
# Import table
T.MissingTypes <- read_csv(here("In", "HER_No_Mon_Types_JL_Update.csv"))
T.MissingTypes <- T.MissingTypes %>% 
  select(ID_CCRI, BroadType_Code)
# Merge table with HER
HER <- merge(HER, T.MissingTypes, by = "ID_CCRI", all.x = TRUE)
# Coalesce the tow broad type fields
HER$BroadType_Code <- coalesce(HER$BroadType_Code.x, HER$BroadType_Code.y)
# Remove unwanted broad type fields created in merge
HER$BroadType_Code.x <- NULL
HER$BroadType_Code.y <- NULL

# Update missing PeriodStrt values using Jeremy's update table - PART 1
# Import table
T.MissingPeriods <- read_csv(here("In", "HER_PeriodStart_Updates_JL.csv"))
# Update HER using "MATCH" (base R) on the imported lookup table
HER$PeriodStrt[match(T.MissingPeriods$ID_CCRI, HER$ID_CCRI)] <- T.MissingPeriods$PeriodStrt

# Merge the broad type code to broad type textual description lookup table with the HER table
HER <- merge(HER, LT.Broad.FISH, by.x = "BroadType_Code",by.y = "Mon_BT_Code", all.x = TRUE)

# Check unique HER IDs
HER.uni.HER_IDs <- as.data.frame(unique(HER$`HER Number`))

# Unique SHINE IDS in HER
HER.uni.SHINE_IDs <- as.data.frame(unique(HER$SHINEuid))

# Check SHINE > HER ID link
# Prior to broad type and period start extraction, find out how many unique SHINE UIDs from SHINE link to unique SHINE IDs from HER

# Unique SHINEUids in HER
SHINEUids.HER <- as.data.frame(unique(HER$SHINEuid))
# Create new SHINEuid column
SHINEUids.HER$SHINEuid <- SHINEUids.HER$`unique(HER$SHINEuid)`
# Delete old SHINEuid column
SHINEUids.HER$`unique(HER$SHINEuid)` <- NULL
# Sort
SHINEUids.HER <- SHINEUids.HER %>%
dplyr::arrange(SHINEuid)

# Unique SHINEUids in SHINE
SHINEuids.SHINE <- as.data.frame(unique(SHINE$shine_uid))
# Create new SHINEuid column
SHINEuids.SHINE$SHINEuid <- SHINEuids.SHINE$`unique(SHINE$shine_uid)`
# Delete old SHINEuid column
SHINEuids.SHINE$`unique(SHINE$shine_uid)` <- NULL
# Sort
SHINEuids.SHINE <- SHINEuids.SHINE %>%
dplyr::arrange(SHINEuid)

# Merge the unique SHINEuids from HER and SHINE.
SHINEuids.merge <- merge(SHINEUids.HER, SHINEuids.SHINE, by.x = "SHINEuid", by.y = "SHINEuid", all.x = FALSE)

# Show SHINEuids in SHINE that are not present in HER
SHINEuids.no.merge.SHINE <- anti_join(SHINEuids.SHINE, SHINEUids.HER, by.x = "SHINEuid", by.y = "SHINEuid")
# Sort
SHINEuids.no.merge.SHINE <- SHINEuids.no.merge.SHINE  %>%
dplyr::arrange(SHINEuid)

# # Show SHINEuids in HER that are not present in SHINE
# SHINEuids.no.merge.HER <- anti_join(SHINEUids.HER, SHINEuids.SHINE, by.x = "SHINEuid", by.y = "SHINEuid")
# # Sort
# SHINEuids.no.merge.HER  <- SHINEuids.no.merge.HER  %>%
# dplyr::arrange(SHINEuid)

#  Export unmatched SHINEuids from SHINE for LNDP
# write_csv(SHINEuids.no.merge.SHINE, here("Out", "SHINE_SHINEuids_no_match_in_HER.csv"))


# 8 - Broad type extract---------------------------------------------------------------------
# Extract a single HER broad type record for each SHINE feature (i.e. tackle the one-to-may problem)

# Count the number of FISH broad types per SHINEuid group and add count as new column "count_FISH_types"
# https://stackoverflow.com/questions/17421776/how-to-add-count-of-unique-values-by-group-to-r-data-frame
HER <- ddply(HER, .(SHINEuid), mutate, count_FISH_types = length(unique(FISH_Broad_Type)))


# # # Show number of HER records where "count" (i.e. the number of unique FISH types per SHINEuid) is greater than one - to see if the one-to-many selection can be done manually
# HER.FISH.1plus <- HER %>% 
# filter(count_FISH_types > 1)


# Check number of HER records where FISH_TYPE_BROAD is "NA"
HER.noFISH <- HER %>% 
  filter(is.na(FISH_Broad_Type)) # Now zero


# Summarise records based on "count" value
HER.FISH.Summary <- HER %>% 
    group_by(SHINEuid, ID_CCRI, count_FISH_types) %>% 
  summarise(countFISH = n())

# Select the most frequently occurring FISh types for each SHINE ID 
HER.FISH.Top <- HER %>% 
  group_by(SHINEuid) %>% 
  count(FISH_Broad_Type) %>% top_n(1)


# result <- HER %>% 
#   group_by(SHINEuid, ID_ccri) %>%
#   filter(FISH_max == max(value)) %>%
#   arrange(A,B,C)


# Count number of duplicated SHINE IDs in HER.FISH.Top- this will show the number of HER records where FISH broad types are tied
sum(duplicated(HER.FISH.Top$SHINEuid))

# Double-check unique SHINEuids in HER.FISH.Top
HER.FISH.Top.unique <- as.data.frame(unique(HER.FISH.Top$SHINEuid))

# Load and merge the FISH_Rank_Lookup.csv lookup table and merge with HER.FISH.TOP to refine selection of HER records based on FISH Type (i.e where there are a tied number of records for a SHINE records, use the FISH type ranks to further refine)
LT.FISH.Ranks <- read_csv(here("In", "FISH_Rank_Lookup.csv"))


# What if ranks are also tied?



# Merge lookup table with HER.FISH.Top (using )
HER.FISH.Top <- merge(HER.FISH.Top, LT.FISH.Ranks, by.x = "FISH_Broad_Type", by.y = "FISH_Broad_Type", all.x = TRUE)

# Remove/rename columns and arrange by SHINE ID (ascending)
HER.FISH.Top <- HER.FISH.Top %>% 
  dplyr::rename(FISH_Type_Count = n) %>% 
  dplyr::arrange(SHINEuid)


# # Select the highest FISH rank value within each group (note: for SHINE IDs where there are no multiples then the single value will still be selected and the record retained)
HER.FISH.Top <- HER.FISH.Top %>%
  group_by(SHINEuid) %>%
  filter(FISH_Rank == max(FISH_Rank))

# Check to see if any duplicate SHINEuids left - if not then each SHINEuid has been assigned a single FISH broad type and can be merged with SHINE
sum(duplicated(HER.FISH.Top$SHINEuid))




# 9 - Merge FISH types with SHINE (in HER.FISH.Top) with the main SHINE table--------------------------------------------------------------------------

SHINE <- merge(SHINE, HER.FISH.Top, by.x = "shine_uid", by.y = "SHINEuid", all.x = TRUE)

# Show records that do not match - tally with SHINE.FISH.Unmatched
SHINE.FISH.Unmatched <- SHINE %>% 
  dplyr::filter(is.na(FISH_Broad_Type))

# # SHINE records are incomplete, so count the number of SHINE records where e.g. SHINE_NAME is incomplete in HER table and see if this tallies with SHINE.FISH.Unmatched 
# HER.SHINE.NA <- dplyr::filter(HER, (SHINE_name =="")) 
SHINE.FISH.NA <- SHINE %>% 
  filter(is.na(FISH_Broad_Type))

# # Export the HER.FISG.Counts data table for colleagues to analyse
# write_csv(HER.FISH.Counts, here("Out", "HER_FISH_Counts.csv"))
# 
# # Export the HER data table for colleagues to analyse
# write_csv(HER, here("Out", "HER.csv"))
# 


# 10 - Period start extract---------------------------------------------------------------------
# Load Period_Rank_Lookup table


LT.Period.Rank <- read_csv(here("In", "Period_Rank_Lookup_Updated.csv"))
LT.Period.Rank$Period_Rank <- as.numeric(LT.Period.Rank$Period_Rank)

# # Get unique period starts from HER
# HER.PeriodStrt.Unique <- as.data.frame(unique(HER$PeriodStrt))

# Merge with main HER table
HER <- merge(HER, LT.Period.Rank, by.x = "PeriodStrt", by.y = "Period", all.x = TRUE)

# # Get records where 'PeriodStrt' is blank ("") NA aand export table to Jeremy for updating
# HER.Period.NA <- HER %>%
#   filter(PeriodStrt == "")
# write_csv(HER.Period.NA, here("Out", "HER_Period_Strt_Blank2.csv"))


# Select the highest Period rank value within each group (note: for SHINE IDs where there are no multiples then the single value will still be selected and the record retained)
HER.Period.Top <- HER %>%
  # select(SHINEuid, PeriodStrt, Period_Rank)
  select(SHINEuid, Aggregated_Period, Period_Rank) %>% 
  group_by(SHINEuid, Aggregated_Period) %>% 
  dplyr::summarise(Period_Rank = max(Period_Rank)) %>% 
  dplyr::filter(Period_Rank == max(Period_Rank))

# Merge the HER.Period.Top wth SHINE
SHINE <- merge(SHINE, HER.Period.Top, by.x = "shine_uid", by.y = "SHINEuid", all.x = TRUE)


SHINE.Period.NA <- SHINE %>% 
  filter(is.na(Aggregated_Period))


# 11 - Protected status extract---------------------------------------------------------------------
# Select most frequnetly cocuring in 
HER.Status.Select <- HER %>% 
  select(SHINEuid, PrtctStat) %>% 
  group_by(SHINEuid) %>% 
  summarise(Status_MAx = max(PrtctStat))
# Merge the selected protected status records with SHINE
SHINE <- merge(SHINE, HER.Status.Select, by.x = "shine_uid", by.y = "SHINEuid", all.x = TRUE)


# 12 - SHINE Export ---------------------------------------------------------------------
# Extract dataframe of complete SHINE records where 
SHINE.Export <- SHINE %>% 
  filter(!is.na(Aggregated_Period))
  

# Export updated SHINE shapefile delting any pervious later first
st_write(SHINE.Export, here("Out", "SHINE_HER_Linked", "SHINE_HER_Linked.shp"), delete_layer=TRUE)
# Export non-spatial table
SHINE.Export.NoGeo <- SHINE.Export
st_geometry(SHINE.Export.NoGeo) <- NULL
write_csv(SHINE.Export.NoGeo, here("Out", "SHINE_HER_Linked","SHINE_HER_Linked_Non_Geo.csv"))



# Audio notification when script has finished running
beepr::beep()



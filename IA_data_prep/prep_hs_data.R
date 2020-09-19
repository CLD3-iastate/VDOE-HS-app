# prepare HS data -- ****SET WORKING DIR TO VDOE-App****

library(dplyr)
library(readxl)
library(readr)
library(tidygeocoder)
library(naniar)
library(sf)

#
# HS Lat and Long data ----------------------------------------------------
#

hs_loc <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/2019-2020 Iowa Public School Building Directory (2).xlsx",
                     skip = 6, col_names = TRUE)

# filter for public high schools only and retain necessary columns
hs_loc <- hs_loc %>%
  filter(`School Level*` == "1") %>%
  select(School, `School Name`, Co, `County Name`, AEA, District, `District Name`, `Physical Street`, 
         `Physcial City`, State...15, `Zip Code...16`, `Grades Served`) %>%
  rename(County = Co, State = State...15, `Zip Code` = `Zip Code...16`)

hs_loc <- hs_loc %>% geocode(street = `Physical Street`, city = `Physcial City`, state = State, 
                           method = 'cascade')

miss_var_summary(hs_loc)

#write_rds(hs_loc, "./data/IA/hs_loc.rds")
hs_loc <- readRDS("./data/IA/hs_loc.rds")


# 11 schools did not get automatically geocoded - looking up and entering those in by hand
# Keokuk, East Mills Jr/Sr, Mid-Prairie, Central Springs, Ottumwa, PCM, Sidney, Tri-County, 
# Van Meter, West Lyon, Westwood

t = hs_loc[is.na(hs_loc$lat),]

lats <- c(40.412360, 40.998160, 41.468830, 43.284020, 41.014810,
          41.514560, 40.742460, 41.454440, 41.529420, 43.394260,
          42.229030)
longs <- c(-91.392700, -95.585170, -91.824010, -93.196230, -92.406170,
           -93.099150, -95.660510, -92.333300, -93.952280, -96.428950,
           -96.215900)

hs_loc[is.na(hs_loc$lat), ]$lat <- lats
hs_loc[is.na(hs_loc$long), ]$long <- longs

miss_var_summary(hs_loc)


#
# HS demographic info -------------------------------------------------------------
#

hs_demo <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/gr4byschool2019.xlsx", skip = 3, col_names = TRUE)

# rename columns and remove uneccesary variables

hs_demo <- hs_demo %>%
  rename(seniors = Denominator...9, free_reduced_lunch = Denominator...15, english_learners = Denominator...18,
         black = Denominator...21, white = Denominator...36) %>%
  select(County, AEA, `AEA Name`, District, `District Name`, School, `School Name`, seniors, free_reduced_lunch,
         english_learners, black, white)

# change '****' entries to '<10' in hs_demo variables

hs_demo[hs_demo$seniors == '****', "seniors"] <- "<10"
hs_demo[hs_demo$free_reduced_lunch == '****', "free_reduced_lunch"] <- "<10"
hs_demo[hs_demo$english_learners == '****', "english_learners"] <- "<10"
hs_demo[hs_demo$black == '****', "black"] <- "<10"
hs_demo[hs_demo$white == '****', "white"] <- "<10"


# merge location and demographic dataframes

nrow( unique(hs_loc[ , c("AEA", "District", "School")]) ) # 320 
nrow( unique(hs_demo[ , c("AEA", "District", "School")]) ) # 400 
# ==> this combination of columns uniquely identifies a school in hs_loc and hs_demo

hs_data <- merge(hs_loc, hs_demo, by = c("AEA", "District", "School"), all.x = TRUE)

hs_data <- hs_data[ , c("County.x", "County.y", "County Name", "AEA", "AEA Name", "District", "District Name.x", 
                        "District Name.y", "School", "School Name.x", "School Name.y", "Physical Street",
                        "Physcial City", "State", "Zip Code", "Grades Served", "lat", "long", 
                        "geo_method", "seniors", "free_reduced_lunch", "english_learners", "black",
                        "white")]

miss_var_summary(hs_data)  # schools with missing demo data: valley southwoods - 9th grade only & virtual campus

# remove two schools with missing data

hs_data <- hs_data[complete.cases(hs_data), ]

# Note: Southwest Valley HS used to be Corning HS.  Corning is the name we match on with FAFSA data.
 
# create the minority count column (number of non-white students)

hs_data <- hs_data %>%
  mutate(minority = (as.integer(seniors) - as.integer(white)))

hs_data$minority <- as.character(hs_data$minority)
hs_data[is.na(hs_data$minority), "minority"] <- "<10"


# temporary write

write_rds(hs_data, "./data/IA/hs_data.rds")

hs_data <- readRDS("./data/IA/hs_data.rds")

#
# FAFSA data ---------------------------------------
#


fafsa1 <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/IA FAFSA 19-20.xls", skip = 3, col_names = TRUE)
fafsa2 <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/IA FAFSA 17-18.xls", skip = 3, col_names = TRUE)
fafsa3 <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/IA FAFSA 15-16.xls", skip = 3, col_names = TRUE)

fafsa2 <- fafsa2 %>%
  filter(State == "IA")

fafsa3 <- fafsa3 %>%
  filter(State == "IA")

# rename columns and only keep necessary variables

fafsa1 <- fafsa1 %>%
  rename(SUBMIT_July_20 = `Applications
Submitted
Jul31  2020`, COMPLETE_July_20 = `Applications
Complete
Jul31  2020`, SUBMIT_July_19 = `Applications
Submitted
Jul31  2019`, COMPLETE_July_19 = `Applications
Complete
Jul31  2019`) %>%
  select(Name, City, SUBMIT_July_20, COMPLETE_July_20, SUBMIT_July_19, COMPLETE_July_19)

fafsa2 <- fafsa2 %>%
  rename(SUBMIT_July_18 = `Applications
Submitted
Jul31  2018`, COMPLETE_July_18 = `Applications
Complete
Jul31  2018`, SUBMIT_July_17 = `Applications
Submitted
Jul31  2017`, COMPLETE_July_17 = `Applications
Complete
Jul31  2017`) %>%
  select(Name, City, SUBMIT_July_18, COMPLETE_July_18, SUBMIT_July_17, COMPLETE_July_17)

fafsa3 <- fafsa3 %>%
  rename(SUBMIT_July_16 = `Applications
Submitted
Jul31  2016`, COMPLETE_July_16 = `Applications
Complete
Jul31  2016`, SUBMIT_July_15 = `Applications
Submitted
Jul31  2015`, COMPLETE_July_15 = `Applications
Complete
Jul31  2015`) %>%
  select(Name, City, SUBMIT_July_16, COMPLETE_July_16, SUBMIT_July_15, COMPLETE_July_15)


# merge with hs_data

hs_data$`School Name.y` <- tolower(hs_data$`School Name.y`)
hs_data$`Physcial City` <- tolower(hs_data$`Physcial City`)
fafsa1$Name <- tolower(fafsa1$Name)
fafsa1$City <- tolower(fafsa1$City)
fafsa2$Name <- tolower(fafsa2$Name)
fafsa2$City <- tolower(fafsa2$City)
fafsa3$Name <- tolower(fafsa3$Name)
fafsa3$City <- tolower(fafsa3$City)

temp <- merge(hs_data, fafsa1, by.x = c("School Name.y", "Physcial City"), by.y = c("Name", "City"), 
              all.x = TRUE)

miss_var_summary(temp) # missing 6 matches

# fix missing matches

fafsa1[fafsa1$Name == "melcher-dallas high school", "City"] <- "melcher-dallas"
fafsa1[fafsa1$Name == "mount ayr high school", "City"] <- "mt ayr"
fafsa1[fafsa1$Name == "north-linn senior high school", "City"] <- "coggon"
fafsa1[fafsa1$Name == "battle creek-ida grove senior high school", "Name"] <- "odebolt arthur battle creek ida grove high school"
# south page high school only missing from fafsa1, could not find info about a name change, etc. just low enrollment/budget struggles
fafsa1[fafsa1$Name == "st ansgar high school", "City"] <- "st ansgar"

temp <- merge(hs_data, fafsa1, by.x = c("School Name.y", "Physcial City"), by.y = c("Name", "City"), 
              all.x = TRUE)
miss_var_summary(temp)

# merge with fafsa 2

temp2 <- merge(temp, fafsa2, by.x = c("School Name.y", "Physcial City"), by.y = c("Name", "City"), 
               all.x = TRUE)

miss_var_summary(temp2) # missing 16 matches

# fix missing matches

fafsa2[fafsa2$Name == "ahstw secondary school", "Name"] <- "ahstw high school"
fafsa2[fafsa2$Name == "cardinal middle-senior high school", "Name"] <- "cardinal high school high school"
fafsa2[fafsa2$Name == "central jr-sr high school", "Name"] <- "central middle school/high school"
fafsa2[fafsa2$Name == "gladbrook-reinbeck high school", "Name"] <- "gladbrook-reinbeck jr. high & high school"

fafsa2[fafsa2$Name == "lawton junior-senior high school", "Name"] <- "lawton-bronson junior-senior high school"
fafsa2[fafsa2$Name == "melcher-dallas high school", "City"] <- "melcher-dallas"
fafsa2[fafsa2$Name == "mount ayr high school", "City"] <- "mt ayr"
fafsa2[fafsa2$Name == "mount pleasant high school", "City"] <- "mt pleasant"

fafsa2[fafsa2$Name == "mount vernon high school", "City"] <- "mt vernon"
fafsa2[fafsa2$Name == "north fayette valley h school", "Name"] <- "north fayette valley high school"
fafsa2[fafsa2$Name == "north scott senior high school", "Name"] <- "north scott high school"
fafsa2[fafsa2$Name == "north-linn senior high school", "City"] <- "coggon"

fafsa2[fafsa2$Name == "battle creek-ida grove senior high school", "Name"] <- "odebolt arthur battle creek ida grove high school"
fafsa2[fafsa2$Name == "rock valley jr-sr high school", "Name"] <- "rock valley high school high school"
fafsa2[fafsa2$Name == "st ansgar high school", "City"] <- "st ansgar"
fafsa2[fafsa2$Name == "van meter jr-sr high school", "Name"] <- "van meter high school school"


temp2 <- merge(temp, fafsa2, by.x = c("School Name.y", "Physcial City"), by.y = c("Name", "City"), 
               all.x = TRUE)

miss_var_summary(temp2)

# merge with fafsa 2

temp3 <- merge(temp2, fafsa3, by.x = c("School Name.y", "Physcial City"), by.y = c("Name", "City"), 
               all.x = TRUE)

miss_var_summary(temp3) # missing 31 matches

# fix missing matches

fafsa3[fafsa3$Name == "a-h-s-t high school", "Name"] <- "ahstw high school"
fafsa3[fafsa3$Name == "ballard community senior high school", "Name"] <- "ballard high school"
fafsa3[fafsa3$Name == "cardinal middle-senior high school", "Name"] <- "cardinal high school high school"

fafsa3[fafsa3$Name == "central decatur ms/sr high school", "Name"] <- "central decatur ms/hs high school"
fafsa3[fafsa3$City == "de witt", "Name"] <- "central dewitt high school"
fafsa3[fafsa3$Name == "central community jr-sr high school", "Name"] <- "central middle school/high school"

fafsa3[fafsa3$Name == "colo-nesco  jr./sr. high school", "Name"] <- "colo-nesco  middle/high school"
fafsa3[fafsa3$Name == "southwest valley high school", "Name"] <- "corning high school"
fafsa3[fafsa3$Name == "garner-hayfield high school", "Name"] <- "garner-hayfield-ventura high school"

fafsa3[fafsa3$Name == "gladbrook-reinbeck high school", "Name"] <- "gladbrook-reinbeck jr. high & high school"
fafsa3[fafsa3$Name == "harlan community high school", "Name"] <- "harlan high school"
fafsa3[fafsa3$Name == "john r mott high school", "Name"] <- "john r mott jr/hi school"

fafsa3[fafsa3$Name == "lawton junior-senior high school", "Name"] <- "lawton-bronson junior-senior high school"
# liberty HS opened in 2017, will be missing from this file for 2015-2016 FAFSA
fafsa3[fafsa3$Name == "melcher-dallas high school", "City"] <- "melcher-dallas"

fafsa3[fafsa3$Name == "marcus-meriden-cleghorn  jr/sr high school", "Name"] <- "mmcru  high school"
fafsa3[fafsa3$Name == "mount ayr high school", "City"] <- "mt ayr"
fafsa3[fafsa3$Name == "mount pleasant high school", "City"] <- "mt pleasant"

fafsa3[fafsa3$Name == "mount vernon high school", "City"] <- "mt vernon"
fafsa3[fafsa3$Name == "murray school murray jr/sr high", "Name"] <- "murray jr/sr high"
fafsa3[fafsa3$Name == "north butler high school", "Name"] <- "north butler jr/sr high school"

fafsa3[fafsa3$Name == "north cedar high school", "Name"] <- "north cedar jr/sr high school"
fafsa3[fafsa3$Name == "north cedar jr/sr high school", "City"] <- "clarence"
fafsa3[fafsa3$Name == "north fayette high school", "Name"] <- "north fayette valley high school"
fafsa3[fafsa3$Name == "north scott senior high school", "Name"] <- "north scott high school"

fafsa3[fafsa3$Name == "north-linn senior high school", "City"] <- "coggon"
fafsa3[fafsa3$Name == "battle creek-ida grove senior high school", "Name"] <- "odebolt arthur battle creek ida grove high school"
fafsa3[fafsa3$Name == "rock valley jr-sr high school", "Name"] <- "rock valley high school high school"

fafsa3[fafsa3$Name == "southeast valley", "Name"] <- "southeast valley high school"
fafsa3[fafsa3$Name == "st ansgar high school", "City"] <- "st ansgar"
fafsa3[fafsa3$Name == "van buren community high school", "Name"] <- "van buren high school"

fafsa3[fafsa3$Name == "van meter jr-sr high school", "Name"] <- "van meter high school school"

temp3 <- merge(temp2, fafsa3, by.x = c("School Name.y", "Physcial City"), by.y = c("Name", "City"), 
               all.x = TRUE)

miss_var_summary(temp3)

hs_fafsa <- temp3

# add in the relative difference column from July 2020 to July 2019

# code <5 in FAFSA columns to NA and change type to integer

hs_fafsa[!is.na(hs_fafsa$COMPLETE_July_15) & hs_fafsa$COMPLETE_July_15 == "<5", "COMPLETE_July_15"] <- NA
hs_fafsa[!is.na(hs_fafsa$COMPLETE_July_16) & hs_fafsa$COMPLETE_July_16 == "<5", "COMPLETE_July_16"] <- NA
hs_fafsa[!is.na(hs_fafsa$COMPLETE_July_17) & hs_fafsa$COMPLETE_July_17 == "<5", "COMPLETE_July_17"] <- NA
hs_fafsa[!is.na(hs_fafsa$COMPLETE_July_18) & hs_fafsa$COMPLETE_July_18 == "<5", "COMPLETE_July_18"] <- NA
hs_fafsa[!is.na(hs_fafsa$COMPLETE_July_19) & hs_fafsa$COMPLETE_July_19 == "<5", "COMPLETE_July_19"] <- NA
hs_fafsa[!is.na(hs_fafsa$COMPLETE_July_20) & hs_fafsa$COMPLETE_July_20 == "<5", "COMPLETE_July_20"] <- NA

hs_fafsa$COMPLETE_July_15 <- as.integer(hs_fafsa$COMPLETE_July_15)
hs_fafsa$COMPLETE_July_16 <- as.integer(hs_fafsa$COMPLETE_July_16)
hs_fafsa$COMPLETE_July_17 <- as.integer(hs_fafsa$COMPLETE_July_17)
hs_fafsa$COMPLETE_July_18 <- as.integer(hs_fafsa$COMPLETE_July_18)
hs_fafsa$COMPLETE_July_19 <- as.integer(hs_fafsa$COMPLETE_July_19)
hs_fafsa$COMPLETE_July_20 <- as.integer(hs_fafsa$COMPLETE_July_20)

hs_fafsa$rel_diff_20_19 <- 100*(hs_fafsa$COMPLETE_July_20 - hs_fafsa$COMPLETE_July_19)/hs_fafsa$COMPLETE_July_19  


#
# final dataset for hs and fafsa information
#

write_rds(hs_fafsa, "./data/IA/hs_fafsa.rds")


#
# percent fafsa completed by county ---------------------------------------------
#

fafsa_by_county <- readRDS("data/IA/hs_fafsa.rds")
county_geom <- readRDS("data/IA/IA_grad_plans.rds")

# calculate percent complete fafsa by county

fafsa_by_county[fafsa_by_county$seniors == "<10", "seniors"] <- NA
fafsa_by_county$seniors <- as.integer(fafsa_by_county$seniors)
fafsa_by_county$COMPLETE_July_20 <- as.integer(fafsa_by_county$COMPLETE_July_20)
fafsa_by_county$COMPLETE_July_19 <- as.integer(fafsa_by_county$COMPLETE_July_19)


# Using County.x column for the county - doesn't match with County.y for 5 high schools.  This column 
# from the IA dept of ed building directory. County Name also comes from the same source.

fafsa_by_county <- fafsa_by_county %>%
  group_by(County.x, `County Name`) %>%
  summarise(num_seniors = sum(seniors, na.rm = TRUE),
            num_complete_fafsa_20 = sum(COMPLETE_July_20, na.rm = TRUE),
            num_complete_fafsa_19 = sum(COMPLETE_July_19, na.rm = TRUE))

fafsa_by_county <- fafsa_by_county %>%
  mutate(perc_complete_20 = (num_complete_fafsa_20/num_seniors)*100,
         pct_rel_diff = 100*(num_complete_fafsa_20 - num_complete_fafsa_19)/num_complete_fafsa_19)

# create bins for percent relative difference

fafsa_by_county$prd_bins = "0"

fafsa_by_county[fafsa_by_county$pct_rel_diff < 0, "prd_bins"] <- 
  cut(fafsa_by_county[fafsa_by_county$pct_rel_diff < 0, "pct_rel_diff"]$pct_rel_diff, 
      breaks = c(-Inf, -20, -10, -5, 0), right = FALSE)

fafsa_by_county[fafsa_by_county$pct_rel_diff > 0, "prd_bins"] <- 
  cut(fafsa_by_county[fafsa_by_county$pct_rel_diff > 0, "pct_rel_diff"]$pct_rel_diff, 
      breaks = c(0, 5, 10, 20, Inf), right = TRUE)

fafsa_by_county$prd_bins <- factor(fafsa_by_county$prd_bins, 
                                   levels = c("[-Inf,-20)", "[-20,-10)", "[-10,-5)", "[-5,0)", "0", 
                                              "(0,5]", "(5,10]", "(10,20]", "(20,Inf]")) 
levels(fafsa_by_county$prd_bins)[levels(fafsa_by_county$prd_bins) == "(20,Inf]"] <- ">20"                                  
levels(fafsa_by_county$prd_bins)[levels(fafsa_by_county$prd_bins) == "[-Inf,-20)"] <- "<-20" 


# add county geometries to data set

county_geom <- county_geom %>%
  filter(year == "2018-2019") %>%
  select(County, COUNTYFP, STATEFP, geometry) 

fafsa_by_county <- merge(fafsa_by_county, county_geom, by.x = "County.x", by.y = "County", all = TRUE)

fafsa_by_county <- fafsa_by_county %>%
  st_as_sf() %>%
  st_transform(4326)

write_rds(fafsa_by_county, "./data/IA/fafsa_by_county.rds")

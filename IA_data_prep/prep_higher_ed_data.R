# prepare IOWA higher-ed data -- ****SET WORKING DIR TO VDOE-App****

library(dplyr)
library(readxl)
library(readr)
library(tigris)
library(sf)
library(tidygeocoder)


#
# graduation intentions data ------------------------------------------------------------
#

IA_grad_plans19 <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/2018-2019 Iowa Public High School Graduate Intentions.xlsx",
                            skip = 5, col_names = TRUE)
IA_grad_plans18 <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/2017-2018 Iowa Public High School Graduate Intentions.xls",
                              skip = 5, col_names = TRUE)
IA_grad_plans17 <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/2016-2017 Iowa Public High School Graduate Intentions.xlsx",
                              skip = 5, col_names = TRUE)
IA_grad_plans16 <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/2015-2016 Iowa Public High School Graduate Intentions.xls",
                              skip = 6, col_names = TRUE)



# remove blank columns 
IA_grad_plans19$...6 <- NULL
IA_grad_plans19$...11 <- NULL

IA_grad_plans18$...6 <- NULL
IA_grad_plans18$...11 <- NULL

IA_grad_plans17$...6 <- NULL
IA_grad_plans17$...11 <- NULL



# remove blank row and totals row
n <- nrow(IA_grad_plans19) - 2  # 330
IA_grad_plans19 <- IA_grad_plans19[1:n, ]

n <- nrow(IA_grad_plans18) - 2  # 333
IA_grad_plans18 <- IA_grad_plans18[1:n, ]

n <- nrow(IA_grad_plans17) - 1  # 333
IA_grad_plans17 <- IA_grad_plans17[1:n, ]

n <- nrow(IA_grad_plans16) - 2  # 336
IA_grad_plans16 <- IA_grad_plans16[1:n, ]


#
# add a year column to all datasets ---------------------------------------------------
#

IA_grad_plans19$year <- '2018-2019'
IA_grad_plans18$year <- '2017-2018'
IA_grad_plans17$year <- '2016-2017'
IA_grad_plans16$year <- '2015-2016'



#
# bind datasets together  -----------------------------------------------------------
#

# column names of the 4 datasets are the same

IA_grad_plans <- rbind(IA_grad_plans16, IA_grad_plans17, IA_grad_plans18, IA_grad_plans19)

#
# group-by county and year in the dataset ----------------------------------------------------
#

IA_grad_plans <- IA_grad_plans %>%
  group_by(County, year) %>%
  summarise(priv4 = sum(`Private 4-Year`, na.rm = TRUE),
            pub4 = sum(`Public 4-Year`, na.rm = TRUE),
            cc = sum(`Community College`, na.rm = TRUE))

#
# add a total column for 4-yr public, 4-yr private, and community college ----------------
#

IA_grad_plans$total <- IA_grad_plans$priv4 + IA_grad_plans$pub4 + IA_grad_plans$cc


#
# add in County name and ACS geometry for each county
#

IA_counties <- read_excel("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/2019-2020 Iowa Public School District PreK-12 Enrollments by District, Grade, Race and Gender_0.xlsx",
                              skip = 4, col_names = TRUE)
n <- nrow(IA_counties) - 1  # 332
IA_counties <- IA_counties[1:n, ]

IA_counties <- IA_counties %>% 
  select(COUNTY, `COUNTY NAME`) %>%
  group_by(COUNTY, `COUNTY NAME`) %>%
  summarize(count = n())

IA_counties$COUNTY <- as.character(IA_counties$COUNTY)
IA_counties[1:9,1] <- c("01", "02", "03", "04", "05", "06", "07", "08", "09")

# merge in county names with grad plans

IA_grad_plans <- merge(IA_grad_plans, IA_counties[ ,1:2], by.x = "County", by.y = "COUNTY", all = TRUE)
IA_grad_plans <- IA_grad_plans[ , c(1,7,2,3,4,5,6)]
IA_grad_plans <- IA_grad_plans %>%
  rename(county_name = `COUNTY NAME`)

# get county geometries

county_geom <- counties(state = "Iowa", cb = FALSE) %>%
  st_as_sf() 

county_geom <- county_geom %>%
  select(STATEFP, COUNTYFP, NAME, geometry)

# merge geometries with IA_grad_plans data

IA_grad_plans[IA_grad_plans$county_name == "Pottawattam",]$county_name <- "Pottawattamie"
IA_grad_plans <- merge(IA_grad_plans, county_geom, by.x = "county_name", by.y = "NAME", all = TRUE)

# change num columns to type int

IA_grad_plans$priv4 <- as.integer(IA_grad_plans$priv4)
IA_grad_plans$pub4 <- as.integer(IA_grad_plans$pub4)
IA_grad_plans$cc <- as.integer(IA_grad_plans$cc)
IA_grad_plans$total <- as.integer(IA_grad_plans$total)

IA_grad_plans <- IA_grad_plans %>%
  st_as_sf() %>%
  st_transform(4326)

write_rds(IA_grad_plans, "./data/IA_grad_plans.rds")




#
# Colleges Lat/Long data ------------------------------------------------
#

colleges <- read_csv("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/colleges_info.csv")
cc <- read_csv("~/gates/rivanna_data/working/educ_dash/IOWA_ed_data/community_colleges.csv")

# 4-yr schools

colleges$UnitID = NULL
colleges$`Institution (entity) name (HD2019)` = NULL
colnames(colleges) <- c("Name", "Website", "Longitude", "Latitude", "Sector", "State")

# filter to only include those in sectors 
#     1- Public, 4-year or above; 
#     2 - Private not-for-profit, 4-year or above; 
# we will get sector 4 - community colleges from the cc df

colleges <- colleges %>%
  filter(Sector == 1 | Sector == 2)
colleges$Location <- NA

# community colleges

cc$X9 <- NULL
cc$ID <- NULL
cc$entity_name <- NULL

cc <- cc %>% geocode(address = address, method = 'cascade')  
# missed 5 cc's: 
#  - Indian Hills, North Campus
#  - Iowa Central, East
#  - Iowa Lakes, Algona
#  - Iowa Lakes, Spencer
#  - Northeast Iowa, Peosta

lats <- c(41.107000, 42.643480, 43.087700, 43.163920, 42.445084)
longs <- c(-92.434770, -95.209850, -94.237620, -95.145570, -90.845841)

cc[is.na(cc$lat), ]$lat <- lats
cc[is.na(cc$long), ]$long <- longs

colnames(cc) <- c("Name", "Website", "Sector", "State", "Location", "Address", "Latitude", "Longitude", "geo_method")
cc <- cc %>% select("Name", "Website", "Longitude", "Latitude", "Sector", "State", "Location")


all_coll <- rbind(colleges, cc)

write_rds(all_coll, "./data/IA/colleges.rds")
 

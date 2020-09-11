library(sf)
library(dplyr)
library(stringr)
library(readxl)
library(rvest)


#------------- CHANGE colleges file to correct projection ----------------------#

colleges <- read.csv("VDOE-App/LatLongs.csv")
colleges <- st_as_sf(colleges,  
         coords = c('Longitude', 'Latitude'),
         crs = 4326)

#write_sf(colleges, "VDOE-App/colleges/colleges.shp",  driver="ESRI Shapefile")


#----------------------------- make regions map -------------------------------#
library(patchwork)
library(ggplot2)

# CB Palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)

super <- read_sf("VDOE-App/super_simple/superintendent_simple.shp")
super$color <- c("#E69F00","#F0E442",   "#009E73", "#0072B2", "#CC79A7","#D55E00", "#56B4E9", "#999999"	)
hs <- read.csv("VDOE-App/data_download.csv")

schoolsRegion <- hs %>% select(Region, SCHOOL) %>% group_by(Region) %>% summarize(n = n())


map <- ggplot(super, aes(fill = as.factor(Region))) +
  geom_sf(fill = super$color, color = "black") +
  geom_sf_text(aes(label = ifelse(Region == 1| Region ==3|Region == 4|Region == 5|Region == 7|Region == 8, Region, NA), geometry = geometry), size =8, fun.geometry = st_centroid)+
  geom_label(x = -76.25656, y = 36.76493, label="2", size = 8, fill = "#E69F00", label.size = NA)+
  geom_label(x = -79.93317, y =36.7945, label="6", size = 8, fill = "#0072B2", label.size = NA)+
  theme_void()

bar <- ggplot(schoolsRegion, aes(y = as.factor(-Region), x = n, fill = as.factor(Region))) +
  scale_fill_discrete(cbPalette) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100))+
  scale_y_discrete(labels= c("Region 8", "Region 7", "Region 6", "Region 5", "Region 4", "Region 3", "Region 2", "Region 1")) + 
  geom_bar(stat = "identity", fill = cbPalette)+
  labs(x = "Number of High Schools", y = "")+
  theme(panel.grid.major = element_blank(), 
        axis.text.y = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y   = element_blank(),
        axis.line.x = element_line(colour = "black"))

map + bar




#
# ---------------------------------COUNTY SHAPFILES----------------------------------------------------------------
#

#download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2019/COUNTY/tl_2019_us_county.zip", destfile = "census_shape")
#unzip("census_shape")
#shape <- st_read("census_shapefiles/tl_2019_us_county.shp")
#the US shapefile was deleted. too large to push to github
#va_2019_shape <- shape %>% filter(STATEFP == "51")
#write_sf(va_2019_shape, "va_acs/va_2019.shp")

shape <- st_read("va_acs/va_2019.shp")
shape <- st_transform(shape, 4326) # this crs is needed for leaflet

#write_sf(shape, "va_acs/va_2019.shp",  driver="ESRI Shapefile")
# I got an error for ALAND here...if we use ALAND we might need to revisit this


# 
# ---------------------------------SUPER INTENDENT BORDERS---------------------------------------------------------
#

# superintendent codes are on HS table
hs <- read_xlsx("VDOE-App/VA_HS.xlsx")

# we will need counties
shape <- st_read("va_acs/va_2019.shp")

super <- unique(hs[, c("Region", "Division Name")])

# change superintendent division names so we can merge with the Census shapefiles
super$`Division Name` <- ifelse(
  super$`Division Name`!= "Charles City County" & super$`Division Name`!= "Williamsburg-James City County",
  str_replace(super$`Division Name`, "City", "city"),
  super$`Division Name`)

# a few singular changes to make
  super[super$`Division Name` == "Williamsburg-James City County", "Division Name"]<- "Williamsburg city"
  super[super$`Division Name` == "Manassas", "Division Name"]<- "Manassas city"
  super[super$`Division Name` == "Richmond County", "Region"]<- 3
  super <- super %>% add_row(Region = 4, `Division Name` = "Fairfax city")
  super <- super %>% add_row(Region = 2, `Division Name` = "James City County")
  super <- super %>% add_row(Region = 5, `Division Name` = "Lexington city")
  super <- super %>% add_row(Region = 8, `Division Name` = "Emporia city")

# make sure ever county in the census shapefiles matches a county
shape[!(shape$NAMELSA %in% super$`Division Name`), ]

shape <- merge(shape, super, by.x = "NAMELSA", by.y = "Division Name", all.x = TRUE)

# merge counties together by region group
new.df <-data.frame(Region = numeric(0), geometry = numeric(0))
for(i in unique(shape$Region)){
  new <- shape %>% select(Region) %>% filter(Region== i) 
  new<-as.data.frame(st_union(new))
  new$Region <- i
  
  new.df<-  rbind(new.df,new)
  
}

new.df <-st_as_sf(new.df)
#write_sf(new.df, "super/superintendent.shp",  driver="ESRI Shapefile")


#
# acs-------------------------------------------------------------------------------
#

acsVA <- read.csv("acs_original/ACSva.csv")

acsVA<- acsVA[, c("GEO_ID", "NAME", 
        # total
        "B01001_001E",  
        # male 18-19, 20, 21, 22-24
        "B01001_007E", "B01001_008E", "B01001_009E", "B01001_010E", 
        #female 18-19,20, 21, 22-24
        "B01001_031E", "B01001_032E", "B01001_033E", "B01001_034E")]

acsVA <- acsVA[-1,]

acsVA[,3:11]<- lapply(acsVA[, 3:11], function(x) as.numeric(as.character(x)))

acsVA$total1824county <- rowSums(acsVA[,4:11])

acsVA$propCountyTotal1824<- acsVA$total1824county/sum(acsVA$total1824county)

acsVA$GEO_ID <- str_remove(acsVA$GEO_ID, "0500000US")

shape <- st_read("va_acs/va_2019.shp")

shape<- merge(shape, acsVA[, c("GEO_ID", "total1824county", "propCountyTotal1824")], by.x = "GEOID", by.y = "GEO_ID")


#write_sf(shape, "va_acs/va_2019.shp", driver="ESRI Shapefile")
# I got an error for ALAND here...if we use ALAND we might need to revisit this

# not sure that I need fasfa now


#--------------------------------------COMMUNITY COLLEGES---------------------------------------------------------------
# data from vicki
vccs <- read_xlsx("VCCS.xlsx",sheet = 2)
vccs <- vccs[, c("COMMUNITY COLLEGE  NAME", "LATITUDE", "LONGITUDE")]

# community colleges websites
url <- "https://www.schev.edu/index/students-and-parents/explore/virginia-institutions"

website <- as.data.frame(url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="Contentplaceholder1_T9C53CF90038_Col00"]/div[3]/div/div[2]/div[4]/div/table') %>%
  html_table()
)

website$Institution.Name <- str_remove(website$Institution.Name, " Community College")

vccs <- merge(website[, c("Institution.Name","Website")], vccs, by.x = "Institution.Name", by.y = "COMMUNITY COLLEGE  NAME")

vccs<- st_as_sf(vccs,  
         coords = c('LONGITUDE', 'LATITUDE'),
         crs = 4326)
#write_sf(vccs, "vccs/vccs.shp", driver="ESRI Shapefile")



# ---------------------------------------------- ST SIMPLIFY------------------------------------------------------
virginia <- st_read("VDOE-App/va_acs/va_2019.shp")
virginia <- st_simplify(virginia, dTolerance = 0.0001)

#write_sf(virginia, "va_acs_simple/va_2019_simple.shp", driver="ESRI Shapefile")

superintendent <- st_read("VDOE-App/super/superintendent.shp")
superintendent <- st_simplify(superintendent, dTolerance = 0.001)

#write_sf(superintendent, "VDOE-App/super_simple/superintendent_simple.shp", driver="ESRI Shapefile")



#--------------------------------------------------------------------------------
fafsa_co <- read.csv("VDOE-App/FAFSA_CO.csv")
geo <- get_acs(geography="county", 
               state=51, 
               variables="C02003_001", # we have to define a variable for the function to work, will be removed since we don't need this data
               year=2018, 
               survey="acs5", 
               cache_table=TRUE, 
               output="wide", 
               geometry=TRUE, 
               keep_geo_vars=TRUE)

fafsa_co$new_name <- toupper(fafsa_co$County)
geo$new_name = toupper(sub(", Virginia", "", geo$NAME.y))
geo <- geo[, c("new_name", "GEOID")]

geo <- st_as_sf(
  geo, 
  coords = c('LONG', 'LAT'),
  crs = st_sfc(4326)
)


fafsa_co[fafsa_co$new_name == "MANASSAS", "new_name"] <- "MANASSAS CITY"
fafsa_co[!(fafsa_co$new_name %in% geo$new_name), ]

# west point is a part of king william county
# colonial beach is part of westmoreland county
# williamsburg city and james city county are separeate in geo



place <- get_acs(geography="place", 
               state=51, 
               variables="C02003_001", # we have to define a variable for the function to work, will be removed since we don't need this data
               year=2018, 
               survey="acs5", 
               cache_table=TRUE, 
               output="wide", 
               geometry=TRUE, 
               keep_geo_vars=TRUE
)

place <- st_as_sf(
  place, 
  coords = c('LONG', 'LAT'),
  crs = st_sfc(4326)
)

place <- place %>% filter(NAME.x == "Colonial Beach"|NAME.x == "West Point") %>%
  select(NAME.x, GEOID) %>%
  mutate(new_name = toupper(NAME.x)) %>%
  select(new_name, GEOID)

westpoint <- place %>% filter(new_name == "WEST POINT")
kingwilliam <- geo %>% filter(new_name == "KING WILLIAM COUNTY")
kingwilliam_new = st_difference(kingwilliam, westpoint)
westmoreland <- geo %>% filter(new_name == "WESTMORELAND COUNTY")
colonialbeach <- place %>% filter(new_name == "COLONIAL BEACH")
westmoreland_new <- st_difference(westmoreland, colonialbeach)
westmoreland_new <- westmoreland_new %>% select(new_name, GEOID)
kingwilliam_new <- kingwilliam_new %>% select(new_name, GEOID)

james <- geo %>% filter(new_name == "JAMES CITY COUNTY")
williamsburg <- geo %>% filter(new_name == "WILLIAMSBURG CITY")

james_new <- st_union(james, williamsburg)

james_new[james_new$new_name == "JAMES CITY COUNTY", "new_name"] <- "WILLIAMSBURG-JAMES CITY COUNTY"
james_new <- james_new %>% select(new_name, GEOID)

geo <- geo %>% filter(new_name != "WESTMORELAND COUNTY" & new_name != "KING WILLIAM COUNTY" & new_name != "JAMES CITY COUNTY" & new_name != "WILLIAMSBURG CITY")
geo <- rbind(geo, westmoreland_new, kingwilliam_new, colonialbeach, westpoint, james_new)

data <- merge(geo[, c("new_name", "GEOID", "geometry")], fafsa_co, by = "new_name", all = TRUE)


write_sf(data, "VDOE-App/va_FAFSA_CO/va_FAFSA_CO.shp",driver="ESRI Shapefile")
#------------------------------------------------




hs <- read_xlsx("data_download_2.xlsx")
#write.csv(hs, "VDOE-App/data_download_2.csv")


data_download_2 <- read.csv("VDOE-App/data_download_2.csv")
data_download <- read_xlsx("VA_HS.xlsx")

data_download_2 <-merge(data_download_2, data_download[,c("Region", "Distract Number", "School Number", "RELDIF")], by.y = c("Region", "Distract Number", "School Number"), by.x = c("Region", "Distract.Number", "School.Number"))
#write.csv(data_download_2, "VDOE-App/data_download_2_reldif.csv")

hs <- st_as_sf(
  data_download_2, 
  coords = c('LONG', 'LAT'),
  crs = st_crs(4326)
)


#write_sf(hs, "VDOE-App/hs/hs.shp", driver="ESRI Shapefile")



#--------------- add minority students-----------------#

df <- read.csv("fall_membership_statistics.csv")
hs <- st_read("VDOE-App/hs/hs.shp")

df$School.Name <- toupper(df$School.Name)

hs[!(hs$SCHOOL %in% df$School.Name), "SCHOOL"] 

df[df$School.Name == "CHARLES CITY COUNTY HIGH", "School.Name"] <- "CHARLES CITY CO HIGH"
df[df$School.Name == "LLOYD C. BIRD HIGH", "School.Name"] <- "LLOYD C BIRD HIGH"
df[df$School.Name == "DINWIDDIE COUNTY HIGH", "School.Name"] <- "DINWIDDIE HIGH"
df[df$School.Name == "BOOKER T WASHINGTON HIGH", "School.Name"] <- "BOOKER T. WASHINGTON HIGH"

df[df$School.Name == "OSCAR F. SMITH HIGH", "School.Name"] <- "OSCAR F SMITH HIGH"
df[df$School.Name == "STAFFORD SENIOR HIGH", "School.Name"] <- "STAFFORD SR. HIGH"
df[df$School.Name == "T.C. WILLIAMS HIGH", "School.Name"] <- "TC WILLIAMS HIGH"
df[df$School.Name == "JOHN HANDLEY HIGH SCHOOL", "School.Name"] <- "JOHN HANDLEY HIGH"

df[df$School.Name == "KETTLE RUN HIGH", "School.Name"] <- "KETTLE RUN HIGH SCH"
df[df$School.Name == "JOHN CHAMPE HIGH SCHOOL", "School.Name"] <- "JOHN CHAMPE HIGH"
df[df$School.Name == "ORANGE COUNTY HIGH", "School.Name"] <- "ORANGE CO. HIGH"
df[df$School.Name == "CHARLES J. COLGAN SR. HIGH", "School.Name"] <- "CHARLES J COLGAN SR HIGH"

df[df$School.Name == "RAPPAHANNOCK COUNTY HIGH", "School.Name"] <- "RAPPAHANNOCK CO. HIGH"
df[df$School.Name == "PARRY MCCLUER HIGH SCHOOL", "School.Name"] <- "PARRY MCCLUER HIGH"
df[df$School.Name == "FORT DEFIANCE HIGH", "School.Name"] <- "FORT DEFINACE HIGH"
df[df$School.Name == "PULASKI COUNTY SENIOR HIGH", "School.Name"] <- "PULASKI COUNTY SR. HIGH"
df[df$School.Name == "THOMAS JEFFERSON HIGH FOR SCIENCE AND TECHNOLOGY", "School.Name"] <- "THOMAS JEFFERSON HIGH"



hs[!(hs$SCHOOL %in% df$School.Name) , "SCHOOL"] 

# this has 321
new <- df %>% filter((School.Name %in% hs$SCHOOL)) %>%
  select(Division.Number, Division.Name, School.Number, School.Name, Race, Grade.12) %>%
  spread(Race,Grade.12)

new$MinorityStudents12 <- rowSums(new[, c("American Indian or Alaska Native","Asian", "Black, not of Hispanic origin",
                                          "Hispanic",  "Native Hawaiian  or Pacific Islander", "Non-Hispanic, two or more races")], 
                                  na.rm = T)

hs_new <-merge(hs, new[, c("School.Name", "School.Number", "MinorityStudents12")],  by.y = c("School.Name", "School.Number"), by.x = c("SCHOOL", "Schl_Nm"), all = T) %>%
  select(!X) %>%
  relocate(MinorityStudents12, .after = EL12)
  
hs_new <- st_as_sf(
  hs_new, 
  coords = c('LONG', 'LAT'),
  crs = st_crs(4326)
)


#write_sf(hs_new, "VDOE-App/hs_new/hs_new.shp", driver="ESRI Shapefile")

data_download <- read.csv("VDOE-App/data_download_2_reldif.csv")

hs_no_geo <- hs_new %>% st_drop_geometry()

test <-merge(data_download, hs_no_geo[, c("SCHOOL", "Schl_Nm", "MinorityStudents12")], by.x = c("SCHOOL", "School.Number"), by.y = c("SCHOOL", "Schl_Nm"), all = T)%>%
  relocate(MinorityStudents12, .after = EL12)

write.csv(test, "VDOE-App/data_download_2_reldif_updated.csv")





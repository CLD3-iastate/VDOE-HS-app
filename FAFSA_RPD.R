library(readxl)
library(leaflet)
library(tidycensus)
library(stringr)
library(sf)
library(dplyr)

data <- read_xlsx("FAFSA_Division.xlsx")

data$new_name <- toupper(data$Division)

geo <- get_acs(geography="county", 
                     state=51, 
                     variables="C02003_001", # we have to define a variable for the function to work, will be removed since we don't need this data
                     year=2018, 
                     survey="acs5", 
                     cache_table=TRUE, 
                     output="wide", 
                     geometry=TRUE, 
                     keep_geo_vars=TRUE
              )
geo$new_name = toupper(sub(", Virginia", "", geo$NAME.y))

geo <- geo[, c("new_name", "GEOID")]

geo <- st_as_sf(
  geo, 
  coords = c('LONG', 'LAT'),
  crs = st_sfc(4326)
)

data[!(data$new_name %in% geo$new_name), ]

data[data$new_name == "MANASSAS", "new_name"] <- "MANASSAS CITY"

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

leaflet(kingwilliam)%>%
  addPolygons() %>%
  addPolygons(data = westpoint, color = "red")%>%
  addPolygons(data = kingwilliam_new, color = "green")

westmoreland <- geo %>% filter(new_name == "WESTMORELAND COUNTY")
colonialbeach <- place %>% filter(new_name == "COLONIAL BEACH")

westmoreland_new <- st_difference(westmoreland, colonialbeach)

leaflet(westmoreland)%>%
  addPolygons() %>%
  addPolygons(data = colonialbeach, color = "red") %>%
  addPolygons(data = westmoreland_new, color = "green")


westmoreland_new <- westmoreland_new %>% select(new_name, GEOID)
kingwilliam_new <- kingwilliam_new %>% select(new_name, GEOID)


james <- geo %>% filter(new_name == "JAMES CITY COUNTY")
williamsburg <- geo %>% filter(new_name == "WILLIAMSBURG CITY")

james_new <- st_union(james, williamsburg)

leaflet(james)%>%
  addPolygons() %>%
  addPolygons(data = williamsburg, color = "red") %>%
  addPolygons(data = james_new, color = "green")

james_new[james_new$new_name == "JAMES CITY COUNTY", "new_name"] <- "WILLIAMSBURG-JAMES CITY COUNTY"
james_new <- james_new %>% select(new_name, GEOID)

geo <- geo %>% filter(new_name != "WESTMORELAND COUNTY" & new_name != "KING WILLIAM COUNTY" & new_name != "JAMES CITY COUNTY" & new_name != "WILLIAMSBURG CITY")

geo <- rbind(geo, westmoreland_new, kingwilliam_new, colonialbeach, westpoint, james_new)


division <- merge(geo[, c("new_name", "GEOID", "geometry")], data, by = "new_name", all = TRUE)


library(leaflet.extras)

label_division <- lapply(
  paste("<strong>Division: </strong>",
        division$new_name,
        "<br />",
        "<strong>RPD: </strong>",
        division$RPD,
        "<br />",
        "<strong># of Completed FAFSAs in 2020: </strong>",
        division$COM2020, 
        "<br />",
        "<strong># of Completed FAFSAs in 2019: </strong>",
        division$COM2019),

  htmltools::HTML
)

scale_range <- c(-80, 60)

pal <- colorNumeric("RdBu", domain = scale_range)

pal <- colorBin(palette ="Blues", domain = division$RPD, bins = 7, na.color = "transparent")


division$bin <- ifelse(division$RPD > 0 & division$RPD <= 20, "(0, 20]", 
                       ifelse(division$RPD > 20 & division$RPD <= 40, "(20, 40]", 
                              ifelse(division$RPD > 40 & division$RPD <= 60, "(40, 60]", 
                                     ifelse(division$RPD == 0, "0", 
                                            ifelse(division$RPD < 0 & division$RPD >= -20, "[-20, 0)",
                                                   ifelse(division$RPD < -20 & division$RPD >= -40, "[-40, -20)",
                                                          ifelse(division$RPD < -40 & division$RPD >= -60, "[-60, -40)",
                                                                 ifelse(division$RPD < -60 & division$RPD >= -80, "[-80, -60)", 
                                                                        "NA"))))))))

division$bin  <- factor(division$bin , levels = c("[-80, -60)", "[-60, -40)", "[-40, -20)", "[-20, 0)", "0", "(0, 20]", "(20, 40]", "(40, 60]"))



write_sf(division, "VDOE-App/division.shp", driver="ESRI Shapefile")

test <- colorRampPalette( c("#D94801","#F9F1CB", "#084594"))(9)

test <- test[-9]

pal <- colorFactor(palette = test, domain = division$bin, na.color = "transparent")
show_col(test)


leaflet(division, options = leafletOptions(minZoom = 7, maxZoom = 10)) %>%
  setView(zoom = 7.5, lat = 38.032560, lng = -79.422777) %>%
  addTiles() %>%
  addPolygons(fillColor =  ~pal(bin), color = "black", opacity = 1, weight = 1, fillOpacity = .8,  label = label_division, 
              labelOptions = labelOptions(direction = "bottom",           
                                          style = list(
                                            "font-size" = "12px",
                                            direction = "auto"))) %>%
  setMapWidgetStyle(list(background= "transparent"))  %>%
  addLegend("bottomleft", pal = pal, values = ~bin,
           title = "Relative Percent Difference", opacity = 1)







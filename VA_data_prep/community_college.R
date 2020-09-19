# VA college dataframe

library(readxl)
library(dplyr)
library(tidyr)

data1 <- read.csv("~/gates/rivanna_data/working/educ_dash/VA_ed_data/LatLongs.csv")
data2 <- read_excel("~/gates/rivanna_data/working/educ_dash/VA_ed_data/CollegeLatLong.xlsx", sheet = "Addresses")

data1_cc <- data1 %>%
  filter(Type != 4)

data1_web <- data1 %>%
  filter(Type == 4) %>%
  separate(College.Name, sep = " Community College", into = "College.Name")

data2_cc <- data2 %>%
  mutate(Type = 4) %>%
  filter(`College Type` == "Public Two-Year") %>%
  transmute(College.Name = `COLLEGE  NAME`,
            Address = ADDRESS,
            City = CITY,
            ZIP.code = `ZIP CODE`,
            Latitude = LATITUDE,
            Longitude = LONGITUDE,
            Type = Type)

data2_cc[3,1]<- "Dabney S Lancaster"
data2_cc[c(8:10),1]<- "J Sargeant Reynolds"
data2_cc[c(24:25),1]<- "Paul D Camp"

data2_cc$ZIP.code <- as.factor(data2_cc$ZIP.code)

data2_cc_web <- left_join(data2_cc, data1_web, by = c("College.Name"))

data2_cc_web <- data2_cc_web %>%
  transmute(College.Name = College.Name,
Address = Address.x,
City= City.x,
ZIP.code = ZIP.code.x,
Latitude= Latitude.x,
Longitude= Longitude.x,
Type = Type.x,
Website = Website)

data2_cc_web$College.Name <- paste(data2_cc_web$College.Name, "Community College")

data_cc <- full_join(data1_cc, data2_cc_web, by = c("College.Name", "Type", "Address", "City", "Latitude", "Longitude", "ZIP.code", "Website"))

saveRDS(data_cc, "./data/VA/college_loc.rds")

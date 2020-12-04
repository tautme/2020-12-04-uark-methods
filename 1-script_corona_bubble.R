## Adam Hughes
## count the number of covid cases by radius


## DOWNLOAD ####
 dir.create("raw_data")
 dir.create("data")

download.file("https://liproduction-reportsbucket-bhk8fnhv1s76.s3-us-west-1.amazonaws.com/v1/latest/latest.csv", "raw_data/latest.csv", mode = "wb")
download.file("https://liproduction-reportsbucket-bhk8fnhv1s76.s3-us-west-1.amazonaws.com/v1/latest/timeseries.csv", "raw_data/timeseries.csv", mode = "wb")

## ANALTYSIS ####
library(tidyverse)

## radius from angle then circumference
## for longitude by radius
####
## NOTICE: estimation, assuming earth is a perfect sphere, NOT ELLIPSOID!
####

latlongdeg2mile <- function(lat, long){
  
  ## Latitude 
  R <- 3959 # mean radius of Earth in miles
  circumference <- 2 * pi * R
  
  ## The 1 degree of 360 is
  D_lat_1_deg <- circumference / 360
  round(D_lat_1_deg, 2)
  
  ## Longitude
  ## This is the same for one degree at the equator (Latitude = 0)
  ## But as abs(Latitude) goes to 90 the circumference goes to 0
  
  # lat <- 34
  lat <- abs(lat)
  lat
  ## at lat = 0, then D_lat_1_deg == D_lon_1_deg
  ## https://www.nhc.noaa.gov/gccalc.shtml
  ## at lat = 34, then one degree lon is 50 nautical miles
  ## at lat = 84, then one degree lon is 06 nautical miles
  
  ## calculate radius first
  ## radius is the angle theta (90 - lat) | opposite (R_lat) over hypot (R)., 
  ##         with hypot. = R
  ## sin(theta) = opposite / hypot. = R_lat / R
  radian <- (90 - lat) * 2 * pi / 360
  R_lat <- sin(radian) * R
  R_lat
  circumference_lat <- 2 * pi * R_lat
  ## at set latitude (lat)
  D_lon_1_deg <-  circumference_lat / 360
  # D_lon_1_deg <-  (circumference / (lat * 4)) / 3.6 
  # where x is the angle of latidude
  round(D_lon_1_deg, 2)
  
  # print(paste("one degree latitude =", round(D_lat_1_deg), "miles",
  #             "one degree longitude =", round(D_lon_1_deg), "miles"))
}


latlongdeg2mile(lat = 0, long = -93)

## Latitude 
R <- 3959 # mean radius of Earth in miles
circumference <- 2 * pi * R
circumference / 360


## data_snap ####
## data.csv today match these?


## latest day of data
data_snap <- read_csv("raw_data/latest.csv",
                      col_types = cols(
                        .default = col_double(),
                        locationID = col_character(),
                        slug = col_character(),
                        name = col_character(),
                        level = col_character(),
                        city = col_character(),
                        county = col_character(),
                        state = col_character(),
                        country = col_character(),
                        aggregate = col_character(),
                        tz = col_character()
                      ))
## city should be character


names(data_snap)
unique(data_snap$level)

data_snap %>%
  filter(level == "state", state == "Kansas") %>%
  select(cases)

data_snap %>%
  filter(level == "county", state == "Kansas") %>%
  # group_by(county) %>%
  summarize(total = sum(cases))

data_snap %>%
  filter(level == "country") %>%
  arrange(desc(cases))

## need only county level
data_snap_county <- data_snap %>%
  filter(level == "county")

data_snap %>%
  filter(level == "county", state == test) %>%
  summarise(total = sum(cases))

data_snap %>%
  filter(level == "state", state ==  "Arkansas") %>%
  summarise(total = sum(cases))

## county 

# tidy_data %>% 
#   filter(level == "county", state == test, date > "2020-03-15") %>% 
#   spread(type, value) %>% 
#   group_by(date, state) %>%
#   summarise(cases = sum(cases)) %>% 
#     ggplot(aes(x = date, y = cases)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")

# time_data %>%
#   filter(level == "county", state == test, date > last_month) %>%
#   group_by(date, state) %>%
#   summarise(cases = sum(cases, na.rm = TRUE)) %>%
#     ggplot(aes(x = date, y = cases)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")
# 
# time_data %>%
#   filter(state == state, date > last_month, level == "county") %>% 
#   group_by(county, date) %>%
#   summarise(tested = sum(tested, na.rm = FALSE),
#             cases = sum(cases, na.rm = FALSE)) %>%
#   ggplot(aes(x = date, y = cases, color = county)) +
#     geom_line() #+
#   # facet_grid(~ . county, scales = "free")
#     
#     
# ## try and automate date change each day
# # gather_range <- paste0('2020-01-22":"', today)
# # gather_range <- paste0('2020-01-22":"', as.character(today))
# # replace(gather_range, c(":"), "")
# # gsub("\\", "", gather_range)
# 
# time_jhu %>%
#   gather("2020-01-22":"2020-04-28", key = date, value = cases) %>%
#   mutate(date = as.Date(date, "%Y-%m-%d")) %>%
#   filter(level == "county", date > last_month, state == test) %>%
#   group_by(state, date) %>%
#   summarise(cases = sum(cases, na.rm = TRUE)) %>%
#     ggplot(aes(x = date, y = cases)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")

## state

data_snap %>%
  filter(level == "state", state == "Missouri") %>%
  select(name, cases, deaths)

# max(tidy_data$date)

# tidy_data %>% 
#   filter(level == "state", state == test, type == "cases") %>% 
#   select(value) %>% max()

# tidy_data %>% 
#   filter(level == "state", state == test, date > "2020-03-15", 
#          type == "cases") %>%
#     ggplot(aes(x = date, y = value, color = type)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")

# time_data %>%
#   filter(level == "state", date > "2020-03-19", state == test) %>%
#     ggplot(aes(x = date, y = cases, color = state)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")
# 
# time_jhu %>%
#   gather("2020-01-22":"2020-04-21", key = date, value = cases) %>%
#   mutate(date = as.Date(date, "%Y-%m-%d")) %>%
#   filter(level == "state", date > "2020-03-19", state == test) %>%
#     ggplot(aes(x = date, y = cases)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")
# 
# ## see last date
# time_jhu %>%
#   gather("2020-01-22":"2020-04-12", key = date, value = cases) %>%
#   mutate(date = as.Date(date, "%Y-%m-%d")) %>%
#   filter(level == "state", date > "2020-03-11", state == test) %>%
#   arrange(desc(date)) %>%
#   select(name, date, cases)
#   
# 
# 
# 
# ## South Dakota
# ## the decrease could be accociated with counting active cases?
# time_data %>%
#   filter(state == "South Dakota", date > "2020-04-01") %>% 
#   group_by(county, date) %>%
#   summarise(tested = sum(tested, na.rm = FALSE),
#             cases = sum(cases, na.rm = FALSE)) %>%
#   ggplot(aes(x = date, y = cases, color = county)) +
#   geom_line() #+
#   # facet_grid(~ . county, scales = "free")

## data.csv
data_snap %>%
  filter(state == "South Dakota") %>%
  summarise(total = sum(cases, na.rm = TRUE))

## county level
data_snap %>% 
  filter(state == "South Dakota") %>%
  filter(level == "county") %>%
  select(name, cases, deaths)
 
data_snap %>% 
  mutate(line = rownames(data_snap)) %>%
  filter(state == "South Dakota", county == "Minnehaha County") %>%
  select(line, name, cases, deaths)



## where is Brazil?
data_snap %>%
  filter(country == "Brazil")

## how does the aggregate level work?
data_snap %>%
  filter(country == "United States", 
         # aggregate == "county",
         cases > 90000)

data_snap %>%
  group_by(level) %>%
  summarise(total = sum(cases, na.rm = TRUE))

data_snap$level %>% as_factor() %>% levels()

data_snap %>% arrange(city)

data_snap %>% filter(aggregate == "state") %>% arrange(desc(cases))



## Map People ##########
# install.packages(c("leaflet", "sp"))
library(sp)
library(leaflet)

dfc <- data_snap %>% 
  filter(level == "county", cases >= 1000, !is.na(lat)) %>%
  arrange(desc(cases)) %>% head(20)

names(dfc)
names(dfc)[9] <- "latitude"
names(dfc)[10] <- "longitude"
coordinates(dfc) <- ~longitude+latitude

leaflet(dfc) %>% 
  addMarkers(popup = paste(dfc$name, "has", dfc$cases, "cases of COVID-19,", 
                           dfc$deaths, "deaths,", dfc$tested, "tested")) %>%
  addTiles()

## Explore


data_snap %>% 
  filter(country == "United States", state == "California") %>% 
  arrange(desc(cases))
## CA sum row = 5550 cases
data_snap %>% 
  filter(country == "United States", 
         state == "California", 
         !is.na(county), 
         is.na(city)) %>% 
  summarise(cases = sum(cases, na.rm = TRUE))
## CA sum of county observations cases = 5551

## top 10 countries
data_snap %>% 
  filter(is.na(state), is.na(county), is.na(city)) %>% 
  summarise(cases = sum(cases, na.rm = TRUE)) %>% 
  arrange(desc(cases))

## Earth sum of countries = 654766 cases in 178 countries
earth <- data_snap %>% 
  filter(is.na(state), is.na(county)) %>% 
  # as_factor() %>% 
  levels()

data_snap %>% filter(level == "state")
data_snap$level %>% as_factor() %>% levels()
data_snap %>% filter(level == "state") %>% summarise(cases = sum(cases, na.rm = TRUE))
## some NA in state
data_snap %>% filter(level == "county") %>% summarise(cases = sum(cases, na.rm = TRUE))
data_snap %>% filter(level == "country") %>% summarise(cases = sum(cases, na.rm = TRUE))

## USA only
data_snap %>% 
  filter(country == "United States", aggregate == "state") %>% 
  summarise(cases = sum(cases, na.rm = TRUE))


## County ####
## we only have data down to county level, so which counties are over 1000 cases?
## check that it is only county
## why are some lat lon 0 or missing?
df <- data_snap %>% filter(level == "county", cases >= 1000, !is.na(lat)) %>%
  arrange(desc(cases))
names(df)
names(df)[9] <- "latitude"
names(df)[10] <- "longitude"

## what about fast rate change
# df <- time_data %>% filter(date == "2020-03-29", !is.na(lat), cases > 100)
# names(df)
# names(df)[6] <- "latitude"
# names(df)[7] <- "longitude"

## Map ##########
# install.packages(c("leaflet", "sp"))
library(sp)
library(leaflet)

coordinates(df) <- ~longitude+latitude

leaflet(df) %>% 
  addCircleMarkers(popup = paste(df$county, "--", 
                                 df$state, 
                                 "has", 
                                 df$cases, 
                                 "cases of COVID-19. DATA:coronadatascraper.com", 
                                 df$url)) %>%
  # addCircleMarkers(radius = 10) %>%
  # addRectangles(lng1 = lng - 3,
  #               lng2 = lng + 3,
  #               lat1 = lat - 3,
  #               lat1 = lat + 3) %>%
  # addCircles(radius = df$cases * df$deaths / 10) %>%
  # labelOptions() %>%
  addTiles()


# ## area #######
# ## at state level
# library(datasets)
# 
# state.area # land in square miles
# state.name
# 
# area <- tibble(state.name, state.area)

## My People ########
## degree buffer search area long, lat.
lon_buffer <- 1
lat_buffer <- 1

my_people <- 
  tibble(    c(1:35),
  c("LasVegas", "Crawfordville", "LittleRock", "NewOrleans", "London", "NewYorkCity", 
    "Melissa", "WarnerRobins", 
    "Phoenix", "OverlandPark", "Denver", "Oakland", "Fayetteville", 
    "Leesburg", "Smackover", "RoundTop", 
    "SanDiego", "Ravenna", "Barrington", "Helwan", "PalmDesert", 
    "Melbourne", "Victoria", "LosAngeles", "Poole", 
    "Pittsburgh", "PIT", "ATL", "Annapolis", "Nassau", "Windsor", "Montpelier", "Russelville", 
    "Pittsburg", "BatonRouge" ),
  c("NV", "FL", "AR", "LA", "ENG", "NY", "TX", "GA", "AZ", "KS", "CO", 
    "CA", "AR", "FL", "AR", "TX", "CA", "OH", "RI", "EGY", "CA", "AUS", "CAN", 
    "CA", "ENG", "PA", "PA", "GA", "MD", "BAH", "CT", "VT", "AR", "KS", "LA" ),
  c("Virgina", "Joe", "Andy", "Nathan", "Luther", "Nancy", "Will", "George", 
    "Jess", "David", 
    "Charlies", "Bob", "Adam", "Gary", "Misty",  "Don",  "Cassi", "Orlando", 
    "Rex",  "Harvey", "Arnold", "Bart", "Valarie", 
    "Ruth",    "Ronnie", "Cary", "Pat",    "Al", "Alf", "Onita", 
    "Jerry", "Jonathan", "Harry", "Devin", "Bonny"),
  c(-115.2, -84.39, -92.41, -90.10, -0.11, -73.99, -96.56, -83.60, -112.07, -94.68, 
    -104.94, -122.25, -94.16, -81.87,     -92.73, -96.80, 
    -117.12, -81.24,  -71.32, 31.33,  -116.39,  145.04, -123.37,    
    -118.44,  -2.00, -79.95,-80.25, -84.43,  -76.54,     
    -77.29, -72.64,   -72.57,   -93.13,        -94.70, -91.20),
  c(36.2, 30.19, 34.74, 29.95, 51.52, 40.74, 33.27, 32.61, 33.49, 
    38.98, 39.60, 37.81, 36.07,  28.73,      33.36,  29.98,  
    32.80,   41.16,  41.74,  29.84,  33.73,   -36.83,  48.47,      
    34.07,    50.71, 40.44, 40.50,  33.64,   38.79,       
    25.04,  41.85 ,   44.26,   35.27,         37.40,  30.45), 
  .name_repair = ~ c("number", "city", "region",  "name", "longitude", "latitude"))


# names(my_people) <- c("name", "longitude", "latitude")
num <- dim(my_people)[1]


names(data_snap)
glimpse(data_snap)

## use explore data from myPeople load
# data_snap <- explore_data %>%
#   filter(date == "2020-04-07")

## For Mypeople bubble count, I must remove the sum duplicates
# data_snap %>% filter(aggregate == "county")
## looks like aggregate broke
# data_all <- data_snap %>% filter(aggregate == "county")

## cut data #######
is.na(data_snap$county)

data_all <- data_snap %>% filter(level != county)

# data_all <- data_snap %>% filter(!is.na(county), !is.na(state))
data_all %>% filter(country == "United States") %>% 
  summarise(usa = sum(cases, na.rm = TRUE))

data_all %>% 
  filter(country == "United States", state == "Arkansas") %>% 
  arrange(desc(cases)) %>%
  summarise(ar = sum(cases, na.rm = TRUE))

## check counts to match features.json
data_all %>% 
  filter(state == "Louisiana") %>%
  arrange(desc(cases))
## why does people out get NA for sum
data_all %>% 
  filter(state == "Louisiana") %>%
  summarise(total = sum(cases))

my_people$name
pep <- 2
## this out is wrong, but I need it to create the out variable
out <- data_all %>%
  filter(  lat <= my_people$latitude[pep] + lat_buffer
           & lat >= my_people$latitude[pep] - lat_buffer
           & long <= my_people$longitude[pep] + lon_buffer
           & long >= my_people$longitude[pep] - lon_buffer) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            tested = sum(tested, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE),
            active = sum(active, na.rm = TRUE),
            population = sum(population, na.rm = TRUE),
            hospitalized = sum(hospitalized, na.rm = TRUE)
  ) %>%
  mutate(name = my_people$name[pep], buffer_deg = lon_buffer)

# max(today$date)
# min(today$date)
## the real question is the number of new cases in the last few days.
## first - you are healthy - then sick - then tested positive - then dead or recovered

## Check Joshua, does Canada count? 44.26, -72.57
## Still have problem of state sum already in observation
# Longit <- my_people$longitude[19]
# Latit <- my_people$latitude[19]

# date_county <- "2020-03-09"
## need to seperate the States
# names(today)
# today %>%
#   filter(  Lat <= Latit + lat_buffer
#            & Lat >= Latit - lat_buffer
#            & Long_ <= Longit + lon_buffer
#            & Long_ >= Longit - lon_buffer) %>%
#   summarize(deaths = sum(Deaths), confirmed = sum(Confirmed))

## is this sum correct? calculating rate change is the number of new cases per day
## where as the date already give a cumulative count.

names(data_all)
for(x in c(1:num)) {
  out[x, ] <- data_all %>%
    filter(    lat <= my_people$latitude[x] + lat_buffer
               & lat >= my_people$latitude[x] - lat_buffer
               & long <= my_people$longitude[x] + lon_buffer
               & long >= my_people$longitude[x] - lon_buffer) %>%
    summarize(cases = sum(cases, na.rm = TRUE),
              deaths = sum(deaths, na.rm = TRUE),
              tested = sum(tested, na.rm = TRUE),
              recovered = sum(recovered, na.rm = TRUE),
              active = sum(active, na.rm = TRUE),
              population = sum(population, na.rm = TRUE),
              hospitalized = sum(hospitalized, na.rm = TRUE)) %>%
    mutate(name = my_people$name[x], 
           buffer_lon_deg = lon_buffer)
}

## Very strange, but when calculating deaths sum first it mixed up death and confirmed

# out %>%
#   arrange(desc(confirmed))

## before filtering max date, add column with count per day
# today %>%
#   group_by(Lat, Long_) %>%
#   mutate(day_count = count - lag(count),
#          day_case = case - lag(case),
#          day_recover = recover - lag(recover)) %>% 
#   arrange(desc(day_count))

## Examining the data shows that this radius may only work in US and China
## Some countries data has only one cental coordinate.

## RUN LONGITUDE.R SCRIPT BEFORE RUNNING BELOW

## add information about miles from degree longitude
radius <- my_people %>%
  mutate(lon_miles = lon_buffer * latlongdeg2mile(my_people$latitude, my_people$longitude),
         lat_miles = lat_buffer * round(circumference / 360, 2))

# max(today$date)

my_people_out <- merge(radius, out, by = "name", all = TRUE) 

names(my_people_out)

my_people_out %>% 
  select(name, city, region, longitude, 
         latitude, lon_miles, lat_miles, tested, 
         cases, deaths, recovered, active) %>%
  arrange(desc(cases), desc(deaths))

format(Sys.time(), "%Y-%m-%d")

my_people_out <- my_people_out %>% 
  mutate(date = format(Sys.time(), "%Y-%m-%d"), area_sqmi = lon_miles * lat_miles,
         cases_per_area = cases / area_sqmi,
         deaths_per_area = deaths / area_sqmi,
         case_per_capita = cases / population, 
         death_per_capita = deaths / population,
         death_per_case = deaths / cases,
         pop_den = population / area_sqmi,
         theoretical_case = cases * 30,
         true_morbidity = (deaths / theoretical_case) * 100)


write_csv(my_people_out, 
          paste0("data/", format(Sys.time(), "%Y%m%d%H%M"), "_my_people_cds_snapshot.csv"))

format(Sys.time(), "%Y-%m-%d")

# ## Map People ##########
my_people_out_usa <- my_people_out %>% filter(longitude < -20)
df <- my_people_out_usa
my_people_out_usa$name
coordinates(df) <- ~longitude+latitude
pep <- 3
df$name

## Normalize
normalized <- (df$cases - min(df$cases)) / (max(df$cases) - min(df$cases)) * 10
median(normalized)
normalized_deaths <- (df$deaths - min(df$deaths)) / (max(df$deaths) - min(df$deaths)) * 10



leaflet(df) %>%
  addTiles() %>%
  # addMarkers(popup = paste(df$name, "has", df$cases, "cases of COVID-19,",
  #                           df$deaths, "deaths,", df$tested, "tested",
  #                          " approximatly ", df$lon_miles,
  #                          "miles radius around them. DATA:coronadatascraper.com"),
   addCircleMarkers(
                   radius = 10,
                   opacity = 0.5,
                   weight = normalized_deaths,
                   fill = TRUE,
                   fillOpacity = 0.4,
                   dashArray = NULL,
                   popupOptions = NULL, label = NULL, labelOptions = NULL,
                   options = pathOptions(), clusterOptions = NULL, clusterId = NULL,
                   # data = getMapData(map),
                   popup = paste(df$name, "has", df$cases, "cases of COVID-19,",
                                          df$deaths, "deaths,", df$tested, "tested",
                                          " approximatly ", df$lon_miles,
                                          "miles radius around them. Data",
                                          "obtained from Corona Data Scraper")
                   ) %>%
  addCircles(radius = normalized)

  # addCircleMarkers(map = dfa,
  #                  fillOpacity = dfa$deaths,
  #                  popup = paste("<font size=3> ", dfa$county, " , ", dfa$state,
  #                                "<p>Population: <B>", dfa$population, "</B></p>
  #                               <p>Cases: <B>", dfa$cases, "</B></p>
  #                               <p>Deaths: <B>", dfa$deaths, "</B></p>
  #                               <p>DATA: <B>https://coronadatascraper.com</B></p></font>")
  #                  ) %>%
  # addCircleMarkers(radius = 10) %>%
  # addRectangles(lat2 = my_people_out_usa$latitude[pep] + lat_buffer,
  #               lat1 = my_people_out_usa$latitude[pep] - lat_buffer,
  #               lng2 = my_people_out_usa$longitude[pep] + lon_buffer,
  #               lng1 = my_people_out_usa$longitude[pep] - lon_buffer,
  #               popup = paste("Estimate from county level data points --" ,
  #                             "(2 degree Longitude, 2 degree Latitude square) In this",
  #                             my_people_out_usa$lon_miles[pep] * 2, "mile wide and ",
  #                             my_people_out_usa$lat_miles[pep] * 2,
  #                             "mile tall area, there are an estimated <B>",
  #                             my_people_out_usa$cases[pep],
  #                             "</B> confirmed cases of COVID-19.",
  #                             "Data obtained from Corona Data Scraper")) %>%
  # addCircles(radius = df$cases * df$deaths / 10) %>%
  # labelOptions()



## plot w/ county #######
## recreate plot from JHU with square and all county counts

my_people_out_usa$name

pep <- 27
pep <- 1

dfa <- data_all %>% 
  filter(country == "United States", 
         state %in% c(
           "Arkansas", "Louisiana", "Missouri",
           "Mississippi", "Oklahoma", "Texas"),
                      # # # Aidan group
                      # "Rhode Island", "Connecticut", "Massachusetts",
                      # "New York", "New Hampshire", "Vermont", "Maine",
                      # "New Jersey", "Pennsylvania"),
                                  # "California", "Nevada", "Arizona", "Oregon"),
         # !is.na(Admin2),
         lat != 0)
names(dfa)

names(dfa)[9] <- "latitude"
names(dfa)[10] <- "longitude"

coordinates(dfa) <- ~longitude+latitude

## Normalize
normalized <- (dfa$cases - min(dfa$cases)) / (max(dfa$cases) - min(dfa$cases)) * 10
median(normalized)

leaflet(dfa) %>% 
  addTiles() %>%
  addRectangles(lat2 = my_people_out_usa$latitude[pep] + lat_buffer,
                lat1 = my_people_out_usa$latitude[pep] - lat_buffer,
                lng2 = my_people_out_usa$longitude[pep] + lon_buffer, 
                lng1 = my_people_out_usa$longitude[pep] - lon_buffer, 
                popup = paste("<font size=3>ESTIMATION SQUARE:
                              <p>Population: <B>", my_people_out_usa$population[pep], "</B></p>
                              <p>Test: <B>", my_people_out_usa$test[pep], "</B></p>
                              <p>Cases: <B>", my_people_out_usa$cases[pep], "</B></p>
                              <p>Active: <B>", my_people_out_usa$active[pep], "</B></p>
                              <p>Recovered: <B>", my_people_out_usa$recovered[pep], "</B></p>
                              <p>Deaths: <B>", my_people_out_usa$deaths[pep], "</B></p>
                              <B>Data obtained from Corona Data Scraper</B></font>")) %>%
  addCircleMarkers(fillOpacity = dfa$deaths, 
                   radius = 4, 
                   weight = 1, 
                   popup = paste("<font size=3> ", dfa$county, " , ", dfa$state,
                                "<p>Population: <B>", dfa$population, "</B></p>
                                <p>Test: <B>", dfa$tested, "</B></p>
                                <p>Cases: <B>", dfa$cases, "</B></p>
                                <p>Active: <B>", dfa$active, "</B></p>
                                <p>Recovered: <B>", dfa$recovered, "</B></p>
                                <p>Deaths: <B>", dfa$deaths, "</B></p>
                                <p><B>Data obtained from Corona Data Scraper</B></p></font>"))


## PUBLISH TEXT
# Experimental
# Click Square -- COVID-19 Map -- Data obtained from Corona Data Scraper
# Check state and territorial health departments.
# Estimate COVID-19 cases from county level data points.
# Data obtained from Corona Data Scraper
# covid_baton_rouge

# Estimation Square -- COVID-19 Map -- Data obtained from Corona Data Scraper
# Check state and territorial health departments.
# Estimate COVID-19 cases from county level data points.
# Data obtained from Corona Data Scraper
# Aidan_RI

# Estimation Square -- COVID-19 Map -- Data obtained from Corona Data Scraper
# Check state and territorial health departments.
# Estimate COVID-19 cases from county level data points.
# Data obtained from Corona Data Scraper
# bliss_oakland



## Normalize
(dfa$cases - min(dfa$cases)) / (max(dfa$cases) - min(dfa$cases)) %>% max()

# paste("Estimate from county level data points --", 
#       "(2 degree Longitude, 2 degree Latitude square) In this",
#       my_people_out_usa$lon_miles[pep] * 2, "mile wide and ",
#       my_people_out_usa$lat_miles[pep] * 2, "mile tall area, there are an estimated <B>",
#       my_people_out_usa$cases[pep], 
#       "</B> confirmed cases of COVID-19. Data obtained from Corona Data Scraper")
# 
# paste("Estimate from county level data points. In this square there are an estimated, ",
#       my_people_out_usa$population[pep], " people. There are<B>",
#       my_people_out_usa$cases[pep], 
#       "</B> cases of COVID-19, and<B>", my_people_out_usa$deaths[pep],
#       "</B>deaths. <B>Data obtained from Corona Data Scraper</B>")) %>%
#   addCircleMarkers(fillOpacity = dfa$cases,
#                    radius = 5, popup = paste("ESTIMATE: ",
#                             dfa$county, " , ", dfa$state, "has<B>",
#                             dfa$cases, "</B>cases of COVID-19, and<B>", 
#                             my_people_out_usa$deaths[pep],
#                             "</B>deaths. Data obtained from Corona Data Scraper</B>")
# 
#    addCircleMarkers(radius = normalized * 15,
#                     popup = paste("Estimate: ",
#                        df$Admin2, " Parish/County, ", df$Province_State, "has<B>",
#                        df$Confirmed, "</B>cases of COVID-19, on",
#                            df$Last_Update, ". DATA: JHU-CSSE"))
                   





## As you increase the radius, how does increase plot
## radius vs. cases #########

my_radius <- seq(0, 2.5, by = 0.2)
my_radius[1]
num_r <- length(my_radius)
names(data_all)

outi <- data_all %>%
  filter(    lat <= my_people_out$latitude[1] + my_radius[1]
             & lat >= my_people_out$latitude[1] - my_radius[1]
             & long <= my_people_out$longitude[1] + my_radius[1]
             & long >= my_people_out$longitude[1] - my_radius[1]) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>%
  mutate(radius = my_radius[1])

my_people_out$name
# pep <- 1

for(x in c(1:num_r)) {
  outi[x, ] <- data_all %>%
    filter(    lat <= my_people_out$latitude[pep] + my_radius[x]
               & lat >= my_people_out$latitude[pep] - my_radius[x]
               & long <= my_people_out$longitude[pep] + my_radius[x]
               & long >= my_people_out$longitude[pep] - my_radius[x]) %>% 
    summarise(cases = sum(cases, na.rm = TRUE),
              deaths = sum(deaths, na.rm = TRUE)) %>%
    mutate(radius = my_radius[x])
}

# max(today$date)

outi %>%
  ggplot(aes(x = radius, y = cases)) +
    geom_line() +
    ggtitle(paste0("Estimate COVID-19 around ", my_people_out$city[pep], ", ", 
                  my_people_out$region[pep], 
                  # " -- (Longitude, Latitude) --", 
                  " (", my_people_out$longitude[pep], ", ", my_people_out$latitude[pep], 
                  ")")) +
    xlab("Degrees Longitude & Latitude Away") +
    annotate("text", x = 1.5, y = 300, 
             label = paste("data source: coronadatascraper.com", Sys.Date()))
      
date()
Sys.Date()

## add information about miles from degree longitude
radiusi <- my_radius %>% tibble() %>%
  mutate(lon_miles = my_radius * latlongdeg2mile(my_people_out$latitude[1], my_people_out$longitude[1]),
         lat_miles = my_radius * round(circumference / 360, 2))
names(radiusi)[1] <- "radius"

my_radius_outi <- merge(radiusi, outi, by = "radius", all = TRUE) %>%
  arrange(desc(deaths), desc(cases))

write_csv(my_radius_outi, paste0("data/", 
                                 format(Sys.time(), "%Y%m%d%H%M"), 
                                 "_", my_people_out$city[pep], "_radius_cds_daily.csv"))


paste0("Estimate COVID-19 around ", my_people_out$city[pep], ", ", 
       my_people_out$region[pep], 
       # " -- (Longitude, Latitude) --", 
       " (", my_people_out$longitude[pep], ", ", my_people_out$latitude[pep], 
       ")")

format(Sys.time(), "%Y%m%d%H%M")



# ## correlation #####
# 
# my_people_out_usa %>%
#   drop_na() %>%
#   select(cases_per_area:death_per_case) %>%
#   pairs()
#     
# 
# 
# # Function to add correlation coefficients
# panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
#   usr <- par("usr")
#   on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   Cor <- abs(cor(x, y)) # Remove abs function if desired
#   txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
#   if(missing(cex.cor)) {
#     cex.cor <- 0.4 / strwidth(txt)
#   }
#   text(0.5, 0.5, txt,
#        cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
# }
# 
# 
# my_people_out_usa %>%
#   drop_na() %>%
#   select(cases_per_area:death_per_case) %>%
#   pairs(upper.panel = panel.cor,    # Correlation panel
#         lower.panel = panel.smooth) # Smoothed regression lines





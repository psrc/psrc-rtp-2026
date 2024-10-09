library(tidyverse)
library(psrcrtp)
library(psrcelmer)
library(here)
library(sf)
library(RSocrata)

# Coordinate Systems for Spatial Data Analysis
wgs84 <- 4326
spn <- 2285
psrc_token <- "tjnJfQzL0SfZJ1cbT0iiCUpO3"
options(dplyr.summarise.inform = FALSE)

# Total Vehicle Registrations
print(str_glue("Loading Total vehicle registrations from WA State Open Portal via RScorata"))
all_vehicle_registrations <- as_tibble(read.socrata("https://data.wa.gov/resource/3d5d-sdqb.json", app_token = psrc_token)) |>
  filter(state %in% c("WA") & county %in% c("King", "Kitsap", "Pierce", "Snohomish")) |>
  select(-"state", -"vehicle_primary_use", -"percent_electric_vehicles") |>
  rename(bev="battery_electric_vehicles_bevs_") |>
  rename(phev="plug_in_hybrid_electric_vehicles_phevs_") |>
  rename(ev="electric_vehicle_ev_total") |>
  rename(non_ev = "non_electric_vehicles") |>
  rename(geography = "county") |>
  mutate(bev=as.numeric(bev), phev=as.numeric(phev)) |>
  mutate(ev=as.numeric(ev), non_ev=as.numeric(non_ev)) |>
  mutate(total_vehicles=as.numeric(total_vehicles)) |>
  group_by(date, geography) |>
  summarise(bev=sum(bev), phev=sum(phev), ev=sum(ev), non_ev=sum(non_ev), total_vehicles=sum(total_vehicles)) |>
  as_tibble() 
  
print(str_glue("Creating a region summary from county data"))
region <- all_vehicle_registrations |>
  group_by(date) |>
  summarise(bev=sum(bev), phev=sum(phev), ev=sum(ev), non_ev=sum(non_ev), total_vehicles=sum(total_vehicles)) |>
  as_tibble() |>
  mutate(geography = "Region")
  
print(str_glue("Combining Region data to county data and calculating shares"))
all_vehicle_registrations <- bind_rows(all_vehicle_registrations, region) |>
  mutate(bev_share = .data$bev / .data$total_vehicles) |>
  mutate(phev_share = .data$phev / .data$total_vehicles) |>
  mutate(ev_share = .data$ev / .data$total_vehicles) |>
  mutate(non_ev_share = .data$non_ev / .data$total_vehicles) |>
  mutate(total_vehicles_share = 1)
rm(region)

saveRDS(all_vehicle_registrations, "data/all_vehicle_registrations.rds")

rtp_local_url <- "C:/coding/"
dol_registration_file <- here(rtp_local_url, "Vehicle_Title_Transactions.csv")

print(str_glue("Loading vehicle registrations from {dol_registration_file} - this is a large file so be patient"))
vehicle_registrations <- read_csv(dol_registration_file, show_col_types = FALSE) |>
  filter(County %in% c("King", "Kitsap", "Pierce", "Snohomish")) |>
  filter(`Vehicle Type` %in% c("MOTORCYCLE", "MULTIPURPOSE PASSENGER VEHICLE (MPV)", "PASSENGER CAR", "TRUCK")) |>
  select(date="Transaction Month and Year", 
         county = "County", tract_id = "2020 GEOID", zipcode = "Postal Code",
         veh_make = "Make", veh_type = "Vehicle Type", veh_year = "Model Year", veh_power = "Electrification Level",
         transaction_type = "Transaction Type", owner_type = "Owner Type", new_or_used = "New or Used Vehicle",
         estimate = "Transaction Count") |>
  mutate(veh_power = str_replace_all(veh_power, "BEV \\(Battery Electric Vehicle\\)", "Battery Electric Vehicle")) |>
  mutate(veh_power = str_replace_all(veh_power, "HEV \\(Hybrid Electric Vehicle\\) - Level Unknown", "Hybrid Electric Vehicle")) |>
  mutate(veh_power = str_replace_all(veh_power, "ICE \\(Internal Combustion Engine\\)", "Internal Combustion Engine")) |>
  mutate(veh_power = str_replace_all(veh_power, "Mild HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
  mutate(veh_power = str_replace_all(veh_power, "Strong HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
  mutate(veh_power = str_replace_all(veh_power, "PHEV \\(Plug-in Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
  mutate(veh_power = str_replace_all(veh_power, "FCEV \\(Fuel Cell Electric Vehicle\\)", "Fuel Cell Vehicle")) |>
  mutate(date = mdy(date))

saveRDS(vehicle_registrations, "data/vehicle_registrations.rds")

max_yr <- max(year(vehicle_registrations$date))
max_mo <- vehicle_registrations |>
  filter(year(date) == max_yr) |>
  select("date") |>
  distinct() |> 
  pull() |>
  max() |>
  month()

# Regional Vehicle Registrations
region_new_all <- vehicle_registrations |> 
  filter(new_or_used == "New") |>
  select("date", "veh_power", "estimate") |>
  group_by(date, veh_power) |>
  summarise(new_vehicles = sum(estimate)) |>
  as_tibble()

temp <- region_new_all |>
  group_by(date) |>
  summarise(total = sum(new_vehicles)) |>
  as_tibble()
  
region_new_all <- left_join(region_new_all, temp, by="date") |>
  mutate(share = new_vehicles/total) |>
  select(-"total") 

rm(temp)
saveRDS(region_new_all, "data/new_vehicle_registrations_region_all_owners.rds")

# County Vehicle Registrations
county_new_all <- vehicle_registrations |> 
  filter(new_or_used == "New") |>
  select("date", "county", "veh_power", "estimate") |>
  group_by(date, county, veh_power) |>
  summarise(new_vehicles = sum(estimate)) |>
  as_tibble()

temp <- county_new_all |>
  group_by(date, county) |>
  summarise(total = sum(new_vehicles)) |>
  as_tibble()
  
county_new_all <- left_join(county_new_all, temp, by=c("date", "county")) |>
  mutate(share = new_vehicles/total) |>
  select(-"total") 

rm(temp)
saveRDS(county_new_all, "data/new_vehicle_registrations_county_all_owners.rds")

# EVs by Make
evs_by_make <- vehicle_registrations |> 
  filter(veh_power == "Battery Electric Vehicle") |>
  select("date", "veh_make", "veh_power", "estimate") |>
  mutate(year = year(date)) |>
  group_by(year, veh_make) |>
  summarise(vehicles = sum(estimate)) |>
  as_tibble()

temp <- evs_by_make |>
  group_by(year) |>
  summarise(total = sum(vehicles)) |>
  as_tibble()

evs_by_make <- left_join(evs_by_make, temp, by=c("year")) |>
  mutate(share = vehicles/total) |>
  select(-"total") |>
    filter(vehicles > 100)

rm(temp)
saveRDS(evs_by_make, "data/evs_by_make.rds")

# EVs by Ownership
evs_by_owner <- vehicle_registrations |> 
  filter(month(date) <= max_mo) |>
  filter(veh_power == "Battery Electric Vehicle") |>
  select("date", "owner_type", "veh_power", "estimate") |>
  mutate(year = year(date)) |>
  group_by(year, owner_type) |>
  summarise(vehicles = sum(estimate)) |>
  as_tibble()

saveRDS(evs_by_owner, "data/evs_by_owner.rds")

# Charging Infrastructure
psrc_counties <- st_read_elmergeo("county_background") |> filter(psrc==1) |> select(county="county_nm") |> st_make_valid() |> st_transform(spn)

temp <- read_csv("data/alt_fuel_stations_sept_2024.csv", show_col_types = FALSE) |>
  select(lon="Longitude", lat="Latitude", station="Station Name", 
         city="City", zipcode="ZIP", group = "Groups With Access Code",
         level1 = "EV Level1 EVSE Num", level2 = "EV Level2 EVSE Num", fast_charge = "EV DC Fast Count",
         date_opened = "Open Date") |>
  mutate(level1 = replace_na(level1, 0), level2 = replace_na(level2, 0), fast_charge = replace_na(fast_charge, 0)) |>
  mutate(city = str_to_title(city)) |>
  mutate(city = str_replace_all(city, "Issaqquah", "Issaquah")) |>
  mutate(city = str_replace_all(city, "Lburien", "Burien")) |>
  mutate(city = str_replace_all(city, "Lynwood", "Lynnwood")) |>
  mutate(city = str_replace_all(city, "Sea Tac", "Seatac")) |>
  mutate(city = str_replace_all(city, "South Bremerton", "Bremerton")) |>
  mutate(city = str_replace_all(city, "Tulalip Bay", "Tulalip")) |>
  mutate(city = str_replace_all(city, "Snoqualmie Pass", "North Bend")) |>
  mutate(city = str_replace_all(city, "Aurora", "Shoreline")) |>
  filter(!(city %in% c("Ashford", "Elbe", "Walla Walla"))) |>
  mutate(year = year(date_opened))

ev_public_charging_stations_lyr <- st_as_sf(temp, coords = c("lon", "lat"), crs = wgs84) |> st_transform(spn)
ev_public_charging_stations <- st_intersection(ev_public_charging_stations_lyr, psrc_counties) |> st_transform(wgs84)
rm(temp, ev_public_charging_stations_lyr)
saveRDS(ev_public_charging_stations, "data/ev_public_charging_stations_lyr.rds")

ev_public_charging_stations_cities <- ev_public_charging_stations |>
  st_drop_geometry() |>
  group_by(city) |>
  summarise(level1=sum(level1), level2=sum(level2), fast_charge=sum(fast_charge)) |>
  as_tibble()
  
temp <- ev_public_charging_stations_cities |>
  mutate(city = "Region") |>
  group_by(city) |>
  summarise(level1=sum(level1), level2=sum(level2), fast_charge=sum(fast_charge)) |>
  as_tibble()

ev_public_charging_stations_cities <- bind_rows(ev_public_charging_stations_cities, temp) |> mutate(year = 2024)
rm(temp)
saveRDS(ev_public_charging_stations_cities, "data/latest_ev_public_charging_cities.rds")

new_ev_public_charging_stations_counties <- ev_public_charging_stations |>
  st_drop_geometry() |>
  group_by(county, year) |>
  summarise(level1=sum(level1), level2=sum(level2), fast_charge=sum(fast_charge)) |>
  as_tibble() |>
  filter(year >= 2017)

temp <- new_ev_public_charging_stations_counties |>
  mutate(county = "Region") |>
  group_by(year, county) |>
  summarise(level1=sum(level1), level2=sum(level2), fast_charge=sum(fast_charge)) |>
  as_tibble()

new_ev_public_charging_stations_counties <- bind_rows(new_ev_public_charging_stations_counties, temp)
rm(temp)
saveRDS(new_ev_public_charging_stations_counties, "data/new_ev_public_charging_counties.rds")

total_ev_public_charging_stations_counties <- NULL
for (y in seq(2017, 2024, by=1)) {
  temp <- ev_public_charging_stations |>
    st_drop_geometry() |>
    filter(year <= y) |>
    group_by(county) |>
    summarise(level1=sum(level1), level2=sum(level2), fast_charge=sum(fast_charge)) |>
    as_tibble() |>
    mutate(year = y)

  if (is.null(total_ev_public_charging_stations_counties)) {total_ev_public_charging_stations_counties <- temp} else {total_ev_public_charging_stations_counties <- bind_rows(total_ev_public_charging_stations_counties, temp)}
  rm(temp)
}

temp <- total_ev_public_charging_stations_counties |>
  mutate(county = "Region") |>
  group_by(year, county) |>
  summarise(level1=sum(level1), level2=sum(level2), fast_charge=sum(fast_charge)) |>
  as_tibble()

total_ev_public_charging_stations_counties <- bind_rows(total_ev_public_charging_stations_counties, temp)
rm(temp)
saveRDS(total_ev_public_charging_stations_counties, "data/total_ev_public_charging_counties.rds")


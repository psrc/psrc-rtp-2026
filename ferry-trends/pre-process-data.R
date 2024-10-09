library(tidyverse)
library(openxlsx)
library(scales)

# Inputs
wgs84 <- 4326
spn <- 2285
options(dplyr.summarise.inform = FALSE)
pre_pandemic <- 2019
current_year <- year(Sys.Date())
prior_year <- year(Sys.Date())-1
analysis_years <- c("2019", "2021", "2022", "2023", "2024")
latest_route_month <- 6

survey_ord <- c("under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65-79", "80+",
                "under $35k", "$35k to $50k", "$50k to $75k", "$75k to\n$100k", "$100k to\n$150k", "$150k to\n$200k", "$200k to\n$250k", "over $250k",
                "Work", "School", "Medical\nAppointment", "Shopping", "Recreation", "Visiting\nfamily or\nfriends", "Other\nPurpose",
                "Driver", "Passenger", "Walked or\nRolled on", "Biked on", "Other Type",
                "Yes", "No",
                "American\nIndian\nor Alaska\nNative", "Asian", "Black or\nAfrican\nAmerican", "Hispanic or\nLatinx", "Middle\nEastern\nor North\nAfrican", "Native\nHawaiian\nor Pacific\nIslander", "Some Other\nRace", "White",
                "Driver of\nVehicle", "Passenger of\nVehicle", "Walked or\nRolled", "Personal\nBike", "Shared Bike", "Bus", "Train", "Ferry", "Rideshare\nServices", "Car Rental", "Vanpool or\nCarpool at\nterminal", "Other Mode",
                "Total")

# Main Teams Channel 
data_dir <- "C:/Users/chelmann/Puget Sound Regional Council/2026-2050 RTP Trends - General/Ferry"

# WSF Data Files
psrc_wsf_routes <- c("bainbridge-seattle", "bremerton-seattle", "clinton-mukilteo", "kingston-edmonds", "southworth-vashon-fauntleroy", "tahlequah-pt-defiance")
ntd_data_file <- file.path(data_dir, "data/ntd-data.xlsx")
mode_file <- file.path(data_dir, "data/ntd-modes.csv")
wsf_daily_ridership_by_route_file <- file.path(data_dir, "data/wsf-daily")
wsf_survey_data_file <- file.path(data_dir, "data/wsf-topline-2023-11-10.xlsx")

# Kitsap Passenger-Only Ferries
kitsap_ff_routes <- c("bremerton", "kingston", "southworth")
kitsap_local_routes <- c("annapolis", "port-orchard")
kitsap_ridership_by_route_file <- file.path(data_dir, "data/KTFF-ridership-dashboard")

# King County Water Taxi Passenger-Only Ferries
king_ff_routes <- c("west-seattle", "vashon")
king_ridership_by_route_file <- file.path(data_dir, "data/KCWT-ridership-dashboard")

# Pierce County Ferries
pierce_ridership_by_route_file <- file.path(data_dir, "data/Ferry_Ridership_and_Left_Behind_Data.csv")

# Monthly Ferry Boardings by Agency from NTD
ntd_tabs <- c("UPT", "VRH")
psrc_agencies <- c("King County Water Taxi", "Kitsap Transit", "Pierce County Ferry","Washington State Ferries")

ntd_ferry_data <- NULL
for (areas in ntd_tabs) {
  print(str_glue("Working on {areas} data processing and cleanup."))
    
  # Open file and filter data to only include Ferry modes
  t <- as_tibble(read.xlsx(ntd_data_file, sheet = areas, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) |>
    rename(Status = contains("Status")) |>
    mutate(NTD.ID = str_pad(string=NTD.ID, width=5, pad="0", side=c("left"))) |>
    filter(Mode == "FB") |> 
    select(-"Legacy.NTD.ID", -"Reporter.Type", -"TOS", -"3.Mode", -contains("Status")) |> 
    pivot_longer(cols = 6:last_col(), names_to = "date", values_to = "estimate", values_drop_na = TRUE) |> 
    mutate(date = my(date))

  # Add Detailed Mode Names & Aggregate  
  t <- t |> 
    mutate(variable="Ferry") |>
    rename(uza="UZA.Name", fips="UACE.CD", geography="Agency", ntd_id="NTD.ID") |> 
    select(-"Mode") |>
    group_by(ntd_id, geography, uza, fips, date, variable) |>
    summarise(estimate=sum(estimate)) |>
    as_tibble() |>
    # Ferry Agencies
    mutate(geography = str_replace_all(geography, "King County Ferry District", "King County")) |>
    mutate(geography = str_replace_all(geography, "King County", "King County Water Taxi")) |>
    mutate(geography = str_replace_all(geography, "Kitsap County Public Transportation Benefit Area Authority", "Kitsap Transit")) |>
    mutate(geography = str_replace_all(geography, "County of Pierce", "Pierce County Ferry")) |>
    # Add Bremerton to Seattle UZA
    mutate(uza = str_replace_all(uza, "Bremerton, WA", "Seattle--Tacoma, WA")) |>
    mutate(fips = str_replace_all(fips, "09946", "80389")) |>
    # Add Vallejo to San Francisco
    mutate(uza = str_replace_all(uza, "Vallejo, CA", "San Francisco--Oakland, CA")) |>
    mutate(fips = str_replace_all(fips, "90028", "78904")) |>
    # UZA Names
    mutate(uza = str_replace_all(uza, "Seattle--Tacoma, WA", "Seattle")) |>
    mutate(uza = str_replace_all(uza, "New York--Jersey City--Newark, NY--NJ", "New York City")) |>
    mutate(uza = str_replace_all(uza, "Atlantic City--Ocean City--Villas, NJ", "Atlantic City")) |>
    mutate(uza = str_replace_all(uza, "San Francisco--Oakland, CA", "San Francisco")) |>
    mutate(uza = str_replace_all(uza, "Miami--Fort Lauderdale, FL", "Miami")) |>
    mutate(uza = str_replace_all(uza, "Virginia Beach--Norfolk, VA", "Norfolk")) |>
    mutate(uza = str_replace_all(uza, "Boston, MA--NH", "Boston")) |>
    mutate(uza = str_replace_all(uza, "Portland, ME", "Portland ME")) |>
    mutate(uza = str_replace_all(uza, "Barnstable Town, MA", "Barnstable MA")) |>
    mutate(uza = str_replace_all(uza, "Providence, RI--MA", "Providence")) |>
    mutate(uza = str_replace_all(uza, "Baltimore, MD", "Baltimore")) |>
    mutate(uza = str_replace_all(uza, "Savannah, GA", "Savannah")) |>
    mutate(uza = str_replace_all(uza, "Jacksonville, FL", "Jacksonville")) |>
    mutate(uza = str_replace_all(uza, "San Juan, PR", "San Juan PR")) |>
    mutate(uza = str_replace_all(uza, "Davenport, IA--IL", "Davenport")) |>
    mutate(uza = str_replace_all(uza, "Chicago, IL--IN", "Chicago")) |>
    mutate(uza = str_replace_all(uza, "Oklahoma City, OK", "Oklahoma City")) |>
    mutate(uza = str_replace_all(uza, "New Orleans, LA", "New Orleans")) |>
    mutate(uza = str_replace_all(uza, "Corpus Christi, TX", "Corpus Christi")) |>
    mutate(uza = str_replace_all(uza, "Honolulu, HI", "Honolulu")) |>
    mutate(uza = str_replace_all(uza, "Los Angeles--Long Beach--Anaheim, CA", "Los Angeles")) |>
    mutate(metric = areas) |>
    mutate(metric = case_when(
      metric == "UPT" ~ "Boardings",
      metric == "VRM" ~ "Revenue-Miles",
      metric == "VRH" ~ "Revenue-Hours")) |>
    mutate(variable = "Monthly") |>
    select(-"ntd_id")
  
  # Full Year Data
  max_yr <- t |> select("date") |> distinct() |> pull() |> max() |> year()
  max_mo <- t |> select("date") |> distinct() |> pull() |> max() |> month()
        
  if (max_mo <12) {
    yr <- max_yr-1
  } else {
    yr <- max_yr
  }
    
  # Trim Data so it only includes full year data and combine
  print(str_glue("Trim {areas} data to only include full year data."))
  full_yr <- t |>
    filter(year(date) <= yr) |>
    mutate(year = year(date)) |>
    group_by(year, geography, uza, fips, metric) |>
    summarise(estimate=sum(estimate)) |>
    as_tibble() |>
    mutate(date = ymd(paste0(year,"-12-01"))) |>
    select(-"year") |>
    mutate(variable = "Annual")

  # Year to Date
  max_mo <- t |> select("date") |> distinct() |> pull() |> max() |> month()
    
  # Trim Data so it only includes ytd for maximum month and combine
  print(str_glue("Trim {areas} data to only months for year to date data through month {max_mo}."))
  ytd <- t |>
    filter(month(date)<=max_mo) |>
    mutate(year = year(date)) |>
    group_by(year, geography, uza, fips, metric) |>
    summarise(estimate=sum(estimate)) |>
    as_tibble() |>
    mutate(date = ymd(paste0(year,"-",max_mo,"-01"))) |>
    select(-"year") |>
    mutate(variable = "Year to Date")

  t <- bind_rows(t, full_yr, ytd)
  if(is.null(ntd_ferry_data)) {ntd_ferry_data <- t} else {ntd_ferry_data <- bind_rows(ntd_ferry_data, t)}
  rm(t, full_yr, ytd)
}

# Create Metro Area Recovery Table for Trend - Year to Date
metro <- ntd_ferry_data |>
  filter(metric == "Boardings" & variable == "Year to Date") |>
  mutate(year = year(date)) |>
  group_by(year, uza, fips) |>
  summarise(estimate = round(sum(estimate), -2)) |>
  as_tibble()
p <- metro |> filter(year == pre_pandemic) |> rename(`Pre-Pandemic` = "estimate") |> select(-"year")
c <- metro |> filter(year == year(Sys.Date())) |> rename(`Current` = "estimate") |> select(-"year")
metro_area_recovery <- left_join(c, p, by=c("uza", "fips")) |> 
  mutate(`Ratio` = `Current` / `Pre-Pandemic`) |>
  arrange(desc(`Ratio`)) |>
  select(`Metro Area` = "uza", `Pre-Pandemic Riders` = "Pre-Pandemic", `Current Riders` = "Current", "Ratio") |>
  tibble::rowid_to_column("Rank")

saveRDS(metro_area_recovery, "data/metro-area-recovery.rds")
rm(p, c, metro)

# Create Metro Area Annual Boardings for most recent full year
metro_area_annual <- ntd_ferry_data |>
  filter(metric == "Boardings" & variable == "Annual") |>
  mutate(year = year(date)) |>
  group_by(year, uza, fips) |>
  summarise(estimate = round(sum(estimate), -2)) |>
  as_tibble() |>
  filter(year == max(year)) |>
  arrange(estimate) |>
  select(`Metro Area` = "uza", `Annual Ridership` = "estimate") |>
  mutate(PSRC = case_when(
    `Metro Area` == "Seattle" ~ "PSRC Region",
    `Metro Area` != "Seattle" ~ "Not in PSRC Region"))

ferry_ord <- metro_area_annual$`Metro Area`
metro_area_annual <- metro_area_annual |> mutate(`Metro Area` = factor(`Metro Area`, levels = ferry_ord))
saveRDS(metro_area_annual, "data/metro_area_annual.rds")

# Ridership by Route - Kitsap Transit
kff_route_ridership <- NULL
for (route in kitsap_ff_routes) {
  print(str_glue("Summarizing Kitsap Transit Fast Ferry ridership for {route} route and month."))
  f <- read.xlsx(paste0(kitsap_ridership_by_route_file, "-", route, ".xlsx"), detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "KTFF-ridership")
  r <- f |> filter(`Direction.(To)` != "Seattle") |> select("Direction.(To)") |> unique() |> pull()
  f <- f |>
    mutate(route = paste0(r," - Seattle - Fast Ferry"), metric = "Boardings", variable = "Monthly") |>
    separate(col = Date, into = c("month", "year"), sep = "-") |>
    mutate(date = ymd(paste0(year, "-", month, "-01"))) |>
    select("route", "date", estimate = "Passenger.count", "metric", "variable") |>
    group_by(date, route, metric, variable) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(operator = "Kitsap Transit")

  if(is.null(kff_route_ridership)) {kff_route_ridership <- f} else {kff_route_ridership <- bind_rows(kff_route_ridership, f)}
  rm(f, r)

}

for (route in kitsap_local_routes) {
  print(str_glue("Summarizing Kitsap Transit Local Ferry ridership for {route} route and month."))
  f <- read.xlsx(paste0(kitsap_ridership_by_route_file, "-", route, ".xlsx"), detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "KTFF-ridership")
  r <- f |> filter(`Direction.(To)` != "Bremerton") |> filter(`Direction.(To)` != "PO") |> select("Direction.(To)") |> unique() |> pull()
  f <- f |>
    mutate(route = paste0(r," - Bremerton"), metric = "Boardings", variable = "Monthly") |>
    separate(col = Date, into = c("month", "year"), sep = "-") |>
    mutate(date = ymd(paste0(year, "-", month, "-01"))) |>
    select("route", "date", estimate = "Passenger.count", "metric", "variable") |>
    group_by(date, route, metric, variable) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(operator = "Kitsap Transit")

  if(is.null(kff_route_ridership)) {kff_route_ridership <- f} else {kff_route_ridership <- bind_rows(kff_route_ridership, f)}
  rm(f, r)

}

# Ridership by Route - King County Water Taxi
kcwt_route_ridership <- NULL
for (route in king_ff_routes) {
  print(str_glue("Summarizing King County Water Taxi ridership for {route} route and month."))
  f <- read.xlsx(paste0(king_ridership_by_route_file, "-", route, ".xlsx"), detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "boardings")
  r <- str_replace_all(str_to_title(route), "-", " ")
  f <- f |>
    mutate(route = paste0(r," - Seattle"), metric = "Boardings", variable = "Weekday") |>
    mutate(date = as.Date(`Month.Year`, origin = "1899-12-30")) |>
    select("route", "date", estimate = "Average.Weekday.Boardings", "metric", "variable") |>
    mutate(operator = "King County Water Taxi")

  if(is.null(kcwt_route_ridership)) {kcwt_route_ridership <- f} else {kcwt_route_ridership <- bind_rows(kcwt_route_ridership, f)}
  rm(f, r)

}

# Ridership by Route - Pierce County Ferries
print(str_glue("Summarizing Pierce County Ferries ridership by month."))
pierce_route_ridership <- read_csv(pierce_ridership_by_route_file, show_col_types = FALSE)
pierce_route_ridership <- pierce_route_ridership |>
    mutate(route = "Steilacoom - Anderson Island", metric = "Boardings", variable = "Monthly") |>
    mutate(date = mdy(Month), estimate = `Walk On` + Vehicles) |>
    select("route", "date", "estimate", "metric", "variable") |>
    mutate(operator = "Pierce County Ferries")

# Ridership by Route - Washington State Ferries
wsf_route_ridership <- NULL
for (route in psrc_wsf_routes) {
  print(str_glue("Summarizing WSF ridership for {route} route and month."))
  f <- read.xlsx(paste0(wsf_daily_ridership_by_route_file, "-", route, ".xlsx"), detectDates = FALSE, skipEmptyRows = TRUE, startRow = 2, colNames = TRUE)
  r <- str_replace_all(str_to_title(route), "-", " - ")
  f <- f |>
    mutate(m = month(mdy(X1)), y = year(mdy(X1))) |>
    select("m", "y", estimate = "Total") |>
    group_by(m, y) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(route = r, date = ymd(paste0(y,"-",m,"-01")), metric = "Boardings", variable = "Monthly", operator = "Washington State Ferries") |>
    select("route", "date", "estimate", "metric", "variable", "operator")

  if(is.null(wsf_route_ridership)) {wsf_route_ridership <- f} else {wsf_route_ridership <- bind_rows(wsf_route_ridership, f)}
  rm(f, r)
}

# Agency Ridership: 2019 to 2024
agency_recovery <- ntd_ferry_data |>
  filter(metric == "Boardings" & variable == "Year to Date") |>
  filter(geography %in% psrc_agencies) |>
  mutate(year = as.character(year(date))) |>
  filter(year %in% analysis_years) |>
  select("geography", "year", "estimate")

saveRDS(agency_recovery, "data/agency_recovery.rds")

# Route Ridership Growth: 2023 to 2024
route_recovery <- bind_rows(kff_route_ridership, kcwt_route_ridership, pierce_route_ridership, wsf_route_ridership) |>
  filter(month(date) <= latest_route_month & year(date) %in% c(prior_year, current_year)) |>
  mutate(year = as.character(year(date))) |>
  select("year", "route", "operator", "estimate") |>
  group_by(year, route, operator) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  pivot_wider(names_from = year, values_from = estimate) |>
  as_tibble() |>
  mutate(ratio = (`2024` / `2023`)-1) |>
  mutate(operator = str_replace_all(operator, "Washington State Ferries", "WSF")) |>
  mutate(operator = str_replace_all(operator, "King County Water Taxi", "King County")) |>
  mutate(operator = str_replace_all(operator, "Pierce County Ferries", "Pierce County")) |>
  arrange(ratio)

ferry_ord <- route_recovery$`route` |> unique()
route_recovery <- route_recovery |> mutate(`route` = factor(`route`, levels = ferry_ord))
  
saveRDS(route_recovery, "data/route_recovery.rds")

# WSF Survey Data
print(str_glue("Summarizing WSF On-Board Survey Data."))
wsf_survey <- as_tibble(read.xlsx(wsf_survey_data_file, sheet = "summary", skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) |>
  mutate(variable = str_wrap(variable, width = 12)) |>
  mutate(variable = factor(variable, levels = survey_ord))

saveRDS(wsf_survey, "data/wsf_survey.rds")

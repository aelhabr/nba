
rm(list = ls())
setwd("O:/_other/projects/nba/")

# Packages. ----
library("dplyr")
library("stringr")
library("readxl")
library("geonames")
library("ggplot2")
library("maps")
library("ggrepel")

# Parameters. ----
export <- TRUE

if (export == TRUE) {
  filename_export_base <- "locations"
  filename_export_suffix <- "-scraped"
  filename_export_ext <- ".csv"
  dir_export <- "data/"
  filepath_export <-
    str_c(
      dir_export,
      filename_export_base,
      filename_export_suffix,
      filename_export_ext
    )
  # filepath_export_ts <-
  #   str_c(
  #     dir_export,
  #     filename_export_base,
  #     filename_export_suffix,
  #     format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #     filename_export_ext
  #   )
}

options(geonamesUsername = "aelhabr")

filepath_db <- "data/db_nba.xlsm"
excel_sheets(filepath_db)
tms <- read_excel(filepath_db, sheet = "nba_tms")

get_geo_info <- function(city, country = "US") {
  # GNsearch(name = city, countryCode = "US") %>%
  if(city == "Toronto") country <- "CA"
  GNsearch(name_equals = city, countryCode = country) %>%
    as_tibble() %>%
    # arrange(desc(population)) %>% 
    select(toponymName, adminCode1, countryCode, lat, lng, population) %>%
    rename(
      city = toponymName,
      state = adminCode1,
      country = countryCode,
      pop = population
    ) %>%
    slice(1) 
}

cities <- tms %>% distinct(city) %>% pull(city)
i <- 1
while(i <= length(cities)) {
  city <- cities[[i]]
  d <- get_geo_info(city)
  if(nrow(d) == 0) next
  if(i == 1) {
    all <- d
  } else {
    all <- bind_rows(all, d)
  }
  i <- i + 1
  cat(city, "\n")
}

# Must be "Washington D.C."
all_fixed <- 
  all %>% 
  lapply(function(x) { x %>% str_replace_all("District of Columbia", "Washington, D.C.") }) %>% 
  as_tibble() %>% 
  mutate_at(vars(lat, lng, pop), funs(as.numeric))

output <-
  all_fixed %>% 
  inner_join(tms, by = "city")
all_fixed %>% anti_join(tms)

if(export == TRUE) {
  write_csv(output, filepath_export)
}

# Check that the locations make sense.
output <- read_csv(filepath_export)
map_states <- map_data("state")
map_cities <- output %>% select(city, lat, lng, tm, pop)

# Trying to extract locations with ggmap does not successfully retrieve all results.
# library("ggmap")
# map_cities_ggmap <- geocode(output$city)

ggplot() +
  geom_polygon(data = map_states,
               aes(x = long, y = lat, group = group), color = "black", fill = "white") +
  geom_point(data = map_cities, aes(x = lng, y = lat, size = pop), color = "red") +
  geom_label_repel(data = map_cities, aes(x = lng, y = lat, label = tm), color = "black") +
  coord_fixed(1.3)


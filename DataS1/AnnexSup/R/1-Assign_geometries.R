library(tidyverse)
library(sf)
library(eurostat)
library(here)

# 1. Assign spatial polygons -------------------------------------------------

## 1.1. administrative names classification  -----------------------------------

setwd("~/GitHub/DAME datapaper")
CropMED_nuts_3 <- c("albania", "belgium", "bulgaria", "croatia", "czech-republic",
                 "denmark", "estonia", "finland", "france", "germany",
                 "greece", "hungary", "ireland", "italy", "luxembourg", "malta",
                 "montenegro", "north-macedonia", "portugal", "romania","serbia",
                 "slovakia", "slovenia", "spain", "sweden", "switzerland",
                 "norway", "turkey", "lithuania", "england")

CropMED_nuts_2 <- c("austria", "belgium", "bulgaria", "croatia", "denmark", 
                 "netherlands", "poland", "serbia")

CropMED_nuts_1 <- c("germany", "latvia", "scotland", "wales" )

CropMED_nuts_0 <- c("netherlands", "slovenia", "lithuania")


CropMED_nuts_2_names <- c("austria", "belgium", "bulgaria", "croatia", "denmark", 
                       "netherlands", "norway", "poland", "serbia")



CropMED_lau1_names <- c("cyprus")

CropMED_other_adm03_names <- c("egypt", "jordan", "lebanon", "moldova", "morocco",
                            "northern-ireland", "palestine", "syria", "tunisia",
                            "ukraine", "wales")

CropMED_other_adm02_names <- c("belarus", "bosnia-herzegovina", "scotland", "ukraine")

CropMED_other_adm01_names <- c("belarus", "bosnia-herzegovina", "kosovo", "northern-ireland")


my_pallette_provinces <- colorRampPalette(colors = c("#6F4748", "#3E90B5", "#E7CB65", "#E57742", "#AA9ABE",
                                                     "#A8B85D", "#EB5B4B", "#DECBD6", "#414F44", "#319483",
                                                     "#722368", "#47BFDB", "#898493", "#EE629F", "#F2BB6E",
                                                     "#232660"))



obtain_geoms_nuts <- function(nuts_level, country_code) {
  if (nuts_level == 3) {
    nuts_geoms <- eurostat::get_eurostat_geospatial(nuts_level = 3, 
                                                        year = 2021,
                                                        resolution = "03")
  } else if (nuts_level == 2) {
    nuts_geoms <- eurostat::get_eurostat_geospatial(nuts_level = 2, 
                                                        year = 2021,
                                                        resolution = "03")
  } else if (nuts_level == 1) {
    nuts_geoms <- eurostat::get_eurostat_geospatial(nuts_level = 1, 
                                                        year = 2021,
                                                        resolution = "03")
  } else if (nuts_level == 0) {
    nuts_geoms <- eurostat::get_eurostat_geospatial(nuts_level = 0, 
                                                        year = 2021,
                                                        resolution = "03")
  } else {stop("nuts_level must be 0, 1, 2 or 3")}
  
  nuts_geoms_country <- nuts_geoms |> 
    filter(CNTR_CODE == country_code) |> 
    rename(adm_name = NUTS_NAME,
           country_code = CNTR_CODE,
           adm_id = NUTS_ID) |> 
    select(country_code, adm_name, adm_id, geometry)
  
  return(nuts_geoms_country)
}

obtain_std_crop_data <- function(country_name,
                                 file_extension = ".xlsx") {
  if (file_extension == ".xlsx") {
    std_crop_country <- readxl::read_xlsx(paste0("Std Data Sets/std_", country_name, ".xlsx")) |> 
      mutate(across(-c(1:4),
                    ~as.numeric(.x)))  
  } else if (file_extension == ".csv") {
    std_crop_country <- readr::read_delim(paste0("Std Data Sets/std_", 
                                          country_name, ".csv"),
                                          locale = locale(encoding = "UTF-8")) |> 
      mutate(across(-c(1:4),
                    ~as.numeric(.x)))
    
  } else {stop("`file_extension` must be either `.xlsx` or `.csv`")}
  
  return(std_crop_country)
}

visualize_std_crop_geoms <- function(std_crop_sf, 
                                     std_crop_name,
                                     overlapping_sf) {

  if(any(std_crop_name == "all")) {
    std_crop_name <- std_crop_sf |> 
      select(-c(adm_id, adm_level, adm_cat_local, geometry)) |> 
      names()
  } else {std_crop_name}
  
  if(!any(std_crop_name %in% names(std_crop_sf))) {
    stop("your `std_crop_name` is not present in the database for the regions in  `std_crop_sf`.
         Type `scan_crops_sf(std_crop_sf)` to see available data.")
  }
  
  std_crop_tidy <- std_crop_sf |>
    mutate(across(-c(adm_level, adm_cat_local, adm_id, geometry),
                  ~as.numeric(.x))) |> 
    pivot_longer(cols = -c(adm_level, adm_cat_local, adm_id, geometry),
                 names_to = "crop",
                 values_to = "hectares") |> 
    filter(crop %in% std_crop_name)
    
  plot_sf <- ggplot()+
    geom_sf(data = overlapping_sf,
             aes(geometry = geometry),
             fill = "gray79")+
    geom_sf(data = std_crop_tidy,
            aes(geometry = geometry,
                fill = hectares))+
    facet_wrap(.~crop)+
    theme_bw()+
    theme(title = element_text(face = "bold"))+
    khroma::scale_fill_lajolla()
  return(plot_sf) 
}

scan_crops_sf <- function(std_crop_sf) {
  available_crops <- std_crop_sf |> 
    select(-c(adm_name, adm_level, adm_cat_local, geometry)) |> 
    names()
  
  return(available_crops)
}

obtain_geoms_nuts <- function(nuts_level, country_code) {
  if (nuts_level == 3) {
    nuts_geoms <- eurostat::get_eurostat_geospatial(nuts_level = 3, 
                                                        year = 2021,
                                                        resolution = "03")
  } else if (nuts_level == 2) {
    nuts_geoms <- eurostat::get_eurostat_geospatial(nuts_level = 2, 
                                                        year = 2021,
                                                        resolution = "03")
  } else if (nuts_level == 1) {
    nuts_geoms <- eurostat::get_eurostat_geospatial(nuts_level = 1, 
                                                        year = 2021,
                                                        resolution = "03")
  } else if (nuts_level == 0) {
    nuts_geoms <- eurostat::get_eurostat_geospatial(nuts_level = 0, 
                                                        year = 2021,
                                                        resolution = "03")
  } else {stop("nuts_level must be 0, 1, 2 or 3")}
  
  nuts_geoms_country <- nuts_geoms |> 
    filter(CNTR_CODE == country_code) |> 
    rename(adm_name = NUTS_NAME,
           country_code = CNTR_CODE,
           adm_id = NUTS_ID) |> 
    select(country_code, adm_name, adm_id, geometry)
  
  return(nuts_geoms_country)
}

obtain_std_crop_data <- function(country_name,
                                 file_extension = ".xlsx") {
  if (file_extension == ".xlsx") {
    std_crop_country <- readxl::read_xlsx(paste0("Std Data Sets/std_", country_name, ".xlsx")) |> 
      mutate(across(-c(1:4),
                    ~as.numeric(.x)))  
  } else if (file_extension == ".csv") {
    std_crop_country <- readr::read_delim(paste0("Std Data Sets/std_", 
                                          country_name, ".csv"),
                                          locale = locale(encoding = "UTF-8")) |> 
      mutate(across(-c(1:4),
                    ~as.numeric(.x)))
    
  } else {stop("`file_extension` must be either `.xlsx` or `.csv`")}
  
  return(std_crop_country)
}

visualize_std_crop_geoms <- function(std_crop_sf, 
                                     std_crop_name,
                                     overlapping_sf) {

  if(any(std_crop_name == "all")) {
    std_crop_name <- std_crop_sf |> 
      select(-c(adm_id, adm_level, adm_cat_local, geometry)) |> 
      names()
  } else {std_crop_name}
  
  if(!any(std_crop_name %in% names(std_crop_sf))) {
    stop("your `std_crop_name` is not present in the database for the regions in  `std_crop_sf`.
         Type `scan_crops_sf(std_crop_sf)` to see available data.")
  }
  
  std_crop_tidy <- std_crop_sf |>
    mutate(across(-c(adm_level, adm_cat_local, adm_id, geometry),
                  ~as.numeric(.x))) |> 
    pivot_longer(cols = -c(adm_level, adm_cat_local, adm_id, geometry),
                 names_to = "crop",
                 values_to = "hectares") |> 
    filter(crop %in% std_crop_name)
    
  plot_sf <- ggplot()+
    geom_sf(data = overlapping_sf,
             aes(geometry = geometry),
             fill = "gray79")+
    geom_sf(data = std_crop_tidy,
            aes(geometry = geometry,
                fill = hectares))+
    facet_wrap(.~crop)+
    theme_bw()+
    theme(title = element_text(face = "bold"))+
    khroma::scale_fill_lajolla()
  return(plot_sf) 
}

obtain_geoms_ne <- function(country_code,
                            path = NULL) {
  if(is.null(path)) {
    ne_geoms <- read_sf("ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")
    ne_geoms_country <- ne_geoms |> 
      filter(iso_a2 == country_code) |> 
      rename(adm_name = geonunit,
             country_code = iso_a2,
             adm_id = iso_3166_2) |> 
      select(country_code, adm_name, adm_id, geometry)
  } else {
    ne_geoms <- read_sf(path)
    ne_geoms_country <- ne_geoms
  }
  
  return(ne_geoms_country)
}

## 1.2. NUTS3 from Eurostat  -----------------------------------


std_crops_geoms_nuts3 <- tibble(country_code = NULL,
                                adm_level = NULL,
                                adm_cat_local = NULL,
                                adm_id = NULL,
                                adm_name.x = NULL)


#### a) albania --------------------------------------------------------------

albania_sf <- obtain_geoms_nuts(nuts_level = 3,
                             country_code = "AL")

std_crops_albania_geoms_nuts3 <- obtain_std_crop_data(country_name = "albania") |> 
  inner_join(albania_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())

#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_albania_geoms_nuts3,
                         std_crop_name = "wheat",
                         overlapping_sf = albania_sf)

std_crops_geoms_nuts3 <- rbind(std_crops_geoms_nuts3, std_crops_albania_geoms_nuts3)

#### b) belgium --------------------------------------------------------------

belgium_sf <- obtain_geoms_nuts(nuts_level = 3,
                                country_code = "BE") |> 
  mutate(grouped_ids = case_when(adm_id %in% c("BE335", "BE336") ~"BE335+BE336",
                                   TRUE ~ adm_id),
         grouped_units = case_when(str_detect(adm_name, "Verviers") ~"Verviers",
                                   TRUE ~ adm_name)) |> 
  group_by(country_code, grouped_ids, grouped_units) |> 
  summarise(geometry = st_union(geometry),
            .groups = "drop") |> 
  rename(adm_id = grouped_ids,
         adm_units = grouped_units)
  

std_crops_belgium_geoms_nuts3 <- obtain_std_crop_data(country_name = "belgium") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(belgium_sf) |> 
  relocate(country_code, adm_level, adm_cat_local, adm_name, .before = everything())

visualize_std_crop_geoms(std_crop_sf = std_crops_belgium_geoms_nuts3,
                         std_crop_name = "wheat",
                         overlapping_sf = belgium_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_belgium_geoms_nuts3)

#### c) bulgaria --------------------------------------------------------------

bulgaria_sf <- obtain_geoms_nuts(nuts_level = 3,
                                 country_code = "BG")

std_crops_bulgaria_geoms_nuts3 <- obtain_std_crop_data(country_name = "bulgaria") |> 
  inner_join(bulgaria_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())

#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_bulgaria_geoms_nuts3,
                         std_crop_name = "potato",
                         overlapping_sf = bulgaria_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_bulgaria_geoms_nuts3)

#### d) croatia --------------------------------------------------------------

croatia_sf <- obtain_geoms_nuts(nuts_level = 3,
                                 country_code = "HR")

std_crops_croatia_geoms_nuts3 <- obtain_std_crop_data(country_name = "croatia") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(croatia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_croatia_geoms_nuts3,
                         std_crop_name = "maize",
                         overlapping_sf = croatia_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_croatia_geoms_nuts3)

#### e) czech_republic --------------------------------------------------------------

czech_republic_sf_no_union <- obtain_geoms_nuts(nuts_level = 3,
                                                country_code = "CZ")
  

czech_republic_sf <- czech_republic_sf_no_union |> 
  filter(adm_name %in% c("Hlavní město Praha", "Středočeský kraj")) |> 
  summarise(adm_name = "JOINED Hlavní město Praha and Středočeský kraj",
            adm_id = "CZ010+CZ020",
            country_code = "CZ",
            geometry = st_union(geometry)) |> 
  bind_rows(czech_republic_sf_no_union) |> 
  mutate(adm_name = stringi::stri_trans_nfc(adm_name)) 

std_crops_czech_republic_geoms_nuts3 <- obtain_std_crop_data(country_name = "czech-republic") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(czech_republic_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())  


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_czech_republic_geoms_nuts3,
                         std_crop_name = "oats",
                         overlapping_sf = czech_republic_sf)

visualize_std_crop_geoms(std_crop_sf = std_crops_czech_republic_geoms_nuts3,
                         std_crop_name = "apple",
                         overlapping_sf = czech_republic_sf)


std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_czech_republic_geoms_nuts3)


#### f) denmark --------------------------------------------------------------

denmark_sf <- obtain_geoms_nuts(nuts_level = 3,
                                country_code = "DK") |> 
  mutate(grouped_ids = case_when(adm_id %in% c("DK011", "DK012", "DK013") ~"DK011+DK012+DK013",
                                 adm_id %in% c("DK021", "DK022") ~"DK021+DK022",
                                 TRUE ~ adm_id),
         grouped_units = case_when(adm_name %in%   c("Byen København", "Københavns omegn", "Nordsjælland") ~"JOINED Byen København and Københavns omegn and Nordsjælland",
                                   adm_name %in%   c("Vest- og Sydsjælland", "Østsjælland") ~"JOINED Østsjælland and Vest- og Sydsjælland",
                                   TRUE ~ adm_name)) |> 
  group_by(country_code, grouped_ids, grouped_units) |> 
  summarise(geometry = st_union(geometry),
            .groups = "drop") |> 
  rename(adm_id = grouped_ids,
         adm_name = grouped_units)

std_crops_denmark_geoms_nuts3 <- obtain_std_crop_data(country_name = "denmark") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(denmark_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())  


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_denmark_geoms_nuts3,
                         std_crop_name = "oilseed rape",
                         overlapping_sf = denmark_sf)


std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_denmark_geoms_nuts3)

#### g) estonia --------------------------------------------------------------

estonia_sf <- obtain_geoms_nuts(nuts_level = 3,
                                country_code = "EE")

std_crops_estonia_geoms_nuts3 <- obtain_std_crop_data(country_name = "estonia") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(estonia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_estonia_geoms_nuts3,
                         std_crop_name = "barley",
                         overlapping_sf = estonia_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_estonia_geoms_nuts3)

#### h) finland --------------------------------------------------------------

finland_sf <- obtain_geoms_nuts(nuts_level = 3,
                                country_code = "FI")

std_crops_finland_geoms_nuts3 <- obtain_std_crop_data(country_name = "finland") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(finland_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_finland_geoms_nuts3,
                         std_crop_name = "barley",
                         overlapping_sf = finland_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_finland_geoms_nuts3)

#### i) france --------------------------------------------------------------

france_sf <- obtain_geoms_nuts(nuts_level = 3,
                                country_code = "FR") |> 
  filter(!str_detect(adm_id, "FRY")) #exclude départements d'outre-mer

std_crops_france_geoms_nuts3 <- obtain_std_crop_data(country_name = "france") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(france_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_france_geoms_nuts3,
                         std_crop_name = "maize",
                         overlapping_sf = france_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_france_geoms_nuts3)

#### j) germany --------------------------------------------------------------

germany_sf <- obtain_geoms_nuts(nuts_level = 3,
                               country_code = "DE") 

std_crops_germany_geoms_nuts3 <- obtain_std_crop_data(country_name = "germany") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(germany_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_germany_geoms_nuts3,
                         std_crop_name = "barley",
                         overlapping_sf = germany_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_germany_geoms_nuts3)


#### j) greece --------------------------------------------------------------

greece_sf <- obtain_geoms_nuts(nuts_level = 3,
                                country_code = "EL") 

std_crops_greece_geoms_nuts3 <- obtain_std_crop_data(country_name = "greece") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(greece_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_greece_geoms_nuts3,
                         std_crop_name = "vineyards for wine",
                         overlapping_sf = greece_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_greece_geoms_nuts3)

#### k) hungary --------------------------------------------------------------

hungary_sf <- obtain_geoms_nuts(nuts_level = 3,
                               country_code = "HU") 

std_crops_hungary_geoms_nuts3 <- obtain_std_crop_data(country_name = "hungary") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(hungary_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_hungary_geoms_nuts3,
                         std_crop_name = "sunflower",
                         overlapping_sf = hungary_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_hungary_geoms_nuts3)

#### l) ireland --------------------------------------------------------------

ireland_sf <- obtain_geoms_nuts(nuts_level = 3,
                                country_code = "IE") |> 
  mutate(grouped_ids = case_when(adm_id %in% c("IE061", "IE062") ~"IE061+IE062",
                                 TRUE ~ adm_id),
         grouped_units = case_when(adm_name %in% c("Mid-East", "Dublin") ~"JOINED Dublin and Mid-East",
                                   TRUE ~ adm_name)) |> 
  group_by(country_code, grouped_ids, grouped_units) |> 
  summarise(geometry = st_union(geometry),
            .groups = "drop") |> 
  rename(adm_id = grouped_ids,
         adm_units = grouped_units)

std_crops_ireland_geoms_nuts3 <- obtain_std_crop_data(country_name = "ireland") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(ireland_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_ireland_geoms_nuts3,
                         std_crop_name = "barley",
                         overlapping_sf = ireland_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_ireland_geoms_nuts3)

#### m) italy --------------------------------------------------------------

italy_sf <- obtain_geoms_nuts(nuts_level = 3,
                              country_code = "IT") 

std_crops_italy_geoms_nuts3 <- obtain_std_crop_data(country_name = "italy") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(italy_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_italy_geoms_nuts3,
                         std_crop_name = "vineyards for wine",
                         overlapping_sf = italy_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_italy_geoms_nuts3)

#### n) luxembourg --------------------------------------------------------------

luxembourg_sf <- obtain_geoms_nuts(nuts_level = 3,
                                   country_code = "LU") 

std_crops_luxembourg_geoms_nuts3 <- obtain_std_crop_data(country_name = "luxembourg") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(luxembourg_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_luxembourg_geoms_nuts3,
                         std_crop_name = "common wheat",
                         overlapping_sf = luxembourg_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_luxembourg_geoms_nuts3)

#### o) malta --------------------------------------------------------------

malta_sf <- obtain_geoms_nuts(nuts_level = 3,
                                   country_code = "MT") 

std_crops_malta_geoms_nuts3 <- obtain_std_crop_data(country_name = "malta") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(malta_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_malta_geoms_nuts3,
                         std_crop_name = "olive groves for oil",
                         overlapping_sf = malta_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_malta_geoms_nuts3)

#### p) montenegro --------------------------------------------------------------

montenegro_sf <- obtain_geoms_nuts(nuts_level = 3,
                              country_code = "ME") 

std_crops_montenegro_geoms_nuts3 <- obtain_std_crop_data(country_name = "montenegro") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(montenegro_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_montenegro_geoms_nuts3,
                         std_crop_name = "watermelon",
                         overlapping_sf = montenegro_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_montenegro_geoms_nuts3)

#### q) north-macedonia --------------------------------------------------------------

north_macedonia_sf <- obtain_geoms_nuts(nuts_level = 3,
                                        country_code = "MK") 

std_crops_north_macedonia_geoms_nuts3 <- obtain_std_crop_data(country_name = "north-macedonia") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(north_macedonia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_north_macedonia_geoms_nuts3,
                         std_crop_name = "tomato",
                         overlapping_sf = north_macedonia_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_north_macedonia_geoms_nuts3)

#### r) portugal --------------------------------------------------------------

portugal_sf <- obtain_geoms_nuts(nuts_level = 3,
                                 country_code = "PT") 

std_crops_portugal_geoms_nuts3 <- obtain_std_crop_data(country_name = "portugal") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(portugal_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_portugal_geoms_nuts3,
                         std_crop_name = "sweet orange",
                         overlapping_sf = portugal_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_portugal_geoms_nuts3)

#### s) romania --------------------------------------------------------------

romania_sf <- obtain_geoms_nuts(nuts_level = 3,
                                 country_code = "RO") 

std_crops_romania_geoms_nuts3 <- obtain_std_crop_data(country_name = "romania") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(romania_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_romania_geoms_nuts3,
                         std_crop_name = "maize",
                         overlapping_sf = romania_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_romania_geoms_nuts3)

#### t) serbia --------------------------------------------------------------

serbia_sf <- obtain_geoms_nuts(nuts_level = 3,
                                country_code = "RS") 

std_crops_serbia_geoms_nuts3 <- obtain_std_crop_data(country_name = "serbia") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(serbia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_serbia_geoms_nuts3,
                         std_crop_name = "maize",
                         overlapping_sf = serbia_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_serbia_geoms_nuts3)

#### u) slovakia --------------------------------------------------------------

slovakia_sf <- obtain_geoms_nuts(nuts_level = 3,
                               country_code = "SK") 

std_crops_slovakia_geoms_nuts3 <- obtain_std_crop_data(country_name = "slovakia") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(slovakia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_slovakia_geoms_nuts3,
                         std_crop_name = "common wheat",
                         overlapping_sf = slovakia_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_slovakia_geoms_nuts3)

#### v) slovenia --------------------------------------------------------------

slovenia_sf <- obtain_geoms_nuts(nuts_level = 3,
                                 country_code = "SI") 

std_crops_slovenia_geoms_nuts3 <- obtain_std_crop_data(country_name = "slovenia") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(slovenia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_slovenia_geoms_nuts3,
                         std_crop_name = "potato",
                         overlapping_sf = slovenia_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_slovenia_geoms_nuts3)

#### x) spain --------------------------------------------------------------

spain_sf <- obtain_geoms_nuts(nuts_level = 3,
                                 country_code = "ES") |> 
  mutate(grouped_ids = case_when(adm_id %in% c("ES531", "ES532", "ES533") ~"ES531+ES532+ES533",
                                 adm_id %in% c("ES708", "ES705", "ES704") ~"ES708+ES705+ES704",
                                 adm_id %in% c("ES709", "ES707", "ES706", "ES703") ~"ES709+ES707+ES706+ES703",
                                 TRUE ~ adm_id),
       grouped_units = case_when(adm_name %in% c("Eivissa y Formentera", "Mallorca", "Menorca") ~"Islas Baleares/Illes Balears",
                                 adm_name %in% c("Lanzarote", "Gran Canaria", "Fuerteventura") ~"Las Palmas",
                                 adm_name %in% c("Tenerife", "La Gomera", "La Palma", "El Hierro") ~"Santa Cruz de Tenerife",
                                 TRUE ~ adm_name)) |> 
  group_by(country_code, grouped_ids, grouped_units) |> 
  summarise(geometry = st_union(geometry),
            .groups = "drop") |> 
  rename(adm_id = grouped_ids,
         adm_units = grouped_units)
std_crops_spain_geoms_nuts3 <- obtain_std_crop_data(country_name = "spain") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(spain_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_spain_geoms_nuts3,
                         std_crop_name = "vineyards for wine",
                         overlapping_sf = spain_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_spain_geoms_nuts3)

#### y) sweden --------------------------------------------------------------

sweden_sf <- obtain_geoms_nuts(nuts_level = 3,
                                 country_code = "SE") 

std_crops_sweden_geoms_nuts3 <- obtain_std_crop_data(country_name = "sweden") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(sweden_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_sweden_geoms_nuts3,
                         std_crop_name = "barley",
                         overlapping_sf = sweden_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_sweden_geoms_nuts3)

#### z) switzerland --------------------------------------------------------------

switzerland_sf <- obtain_geoms_nuts(nuts_level = 3,
                                    country_code = "CH") 

std_crops_switzerland_geoms_nuts3 <- obtain_std_crop_data(country_name = "switzerland") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(switzerland_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_switzerland_geoms_nuts3,
                         std_crop_name = "oilseed rape",
                         overlapping_sf = switzerland_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_switzerland_geoms_nuts3)

#### aa) turkey --------------------------------------------------------------

turkey_sf <- obtain_geoms_nuts(nuts_level = 3,
                                    country_code = "TR") 

std_crops_turkey_geoms_nuts3 <- obtain_std_crop_data(country_name = "turkey") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(turkey_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_turkey_geoms_nuts3,
                         std_crop_name = "common wheat",
                         overlapping_sf = turkey_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_turkey_geoms_nuts3)

#### bb) norway --------------------------------------------------------------

norway_sf <- obtain_geoms_nuts(nuts_level = 3,
                               country_code = "NO") |> 
  filter(!adm_name %in% c("Svalbard", "Jan Mayen")) |> 
  mutate(grouped_ids = case_when(adm_id %in% c("NO081", "NO082") ~"NO081+NO082",
                                 TRUE ~ adm_id),
         grouped_units = case_when(adm_name %in% c("Oslo", "Viken") ~"Oslo of Viken",
                                   TRUE ~ adm_name)) |> 
  group_by(country_code, grouped_ids, grouped_units) |> 
  summarise(geometry = st_union(geometry),
            .groups = "drop") |> 
  rename(adm_id = grouped_ids,
         adm_units = grouped_units)

std_crops_norway_geoms_nuts3 <- obtain_std_crop_data(country_name = "norway") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(norway_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_norway_geoms_nuts3,
                         std_crop_name = "potato",
                         overlapping_sf = norway_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_norway_geoms_nuts3)


#### bb) lithuania --------------------------------------------------------------

lithuania_sf <- obtain_geoms_nuts(nuts_level = 3,
                                country_code = "LT")

std_crops_lithuania_geoms_nuts3 <- obtain_std_crop_data(country_name = "lithuania") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(lithuania_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())

#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_lithuania_geoms_nuts3,
                         std_crop_name = "oilseed rape",
                         overlapping_sf = lithuania_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_lithuania_geoms_nuts3)

#### cc) england --------------------------------------------------------------

england_sf <- obtain_geoms_nuts(nuts_level = 3,
                                  country_code = "UK")

std_crops_england_geoms_nuts3 <- obtain_std_crop_data(country_name = "england") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(england_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())

#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_england_geoms_nuts3,
                         std_crop_name = "wheat",
                         overlapping_sf = england_sf)

std_crops_geoms_nuts3 <- bind_rows(std_crops_geoms_nuts3, std_crops_england_geoms_nuts3)
#### cc) joined data set --------------------------------------------------------------
std_crops_geoms_nuts3
visualize_std_crop_geoms(std_crop_sf = std_crops_geoms_nuts3,
                         std_crop_name = "potato",
                         overlapping_sf = turkey_sf)



## 1.3. NUTS2 from Eurostat  -----------------------------------

print(CropMED_nuts_2)

std_crops_geoms_nuts2 <- tibble(country_code = NULL,
                                adm_level = NULL,
                                adm_cat_local = NULL,
                                adm_id = NULL,
                                adm_name.x = NULL)

#### a) austria --------------------------------------------------------------

austria_sf <- obtain_geoms_nuts(nuts_level = 2,
                               country_code = "AT") 

std_crops_austria_geoms_nuts2 <- obtain_std_crop_data(country_name = "austria") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(austria_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_austria_geoms_nuts2,
                         std_crop_name = "barley",
                         overlapping_sf = austria_sf)

std_crops_geoms_nuts2 <- rbind(std_crops_geoms_nuts2, std_crops_austria_geoms_nuts2)


#### b) belgium --------------------------------------------------------------

belgium_sf <- obtain_geoms_nuts(nuts_level = 2,
                                country_code = "BE") 

std_crops_belgium_geoms_nuts2 <- obtain_std_crop_data(country_name = "belgium") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(belgium_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_belgium_geoms_nuts2,
                         std_crop_name = "fiber flax",
                         overlapping_sf = belgium_sf)

std_crops_geoms_nuts2 <- bind_rows(std_crops_geoms_nuts2, std_crops_belgium_geoms_nuts2)

#### c) bulgaria --------------------------------------------------------------

bulgaria_sf <- obtain_geoms_nuts(nuts_level = 2,
                                country_code = "BG") 

std_crops_bulgaria_geoms_nuts2 <- obtain_std_crop_data(country_name = "bulgaria") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(bulgaria_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_bulgaria_geoms_nuts2,
                         std_crop_name = "sweet cherries",
                         overlapping_sf = bulgaria_sf)

std_crops_geoms_nuts2 <- bind_rows(std_crops_geoms_nuts2, std_crops_bulgaria_geoms_nuts2)

#### d) croatia --------------------------------------------------------------

croatia_sf <- obtain_geoms_nuts(nuts_level = 2,
                                 country_code = "HR") 

std_crops_croatia_geoms_nuts2 <- obtain_std_crop_data(country_name = "croatia") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(croatia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_croatia_geoms_nuts2,
                         std_crop_name = "hazelnut",
                         overlapping_sf = croatia_sf)

std_crops_geoms_nuts2 <- bind_rows(std_crops_geoms_nuts2, std_crops_croatia_geoms_nuts2)

#### e) netherlands --------------------------------------------------------------

netherlands_sf <- obtain_geoms_nuts(nuts_level = 2,
                                    country_code = "NL") 

std_crops_netherlands_geoms_nuts2 <- obtain_std_crop_data(country_name = "netherlands") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(netherlands_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_netherlands_geoms_nuts2,
                         std_crop_name = "sugar beet",
                         overlapping_sf = netherlands_sf)

std_crops_geoms_nuts2 <- bind_rows(std_crops_geoms_nuts2, std_crops_netherlands_geoms_nuts2)

#### f) poland --------------------------------------------------------------

poland_sf <- obtain_geoms_nuts(nuts_level = 2,
                                    country_code = "PL") 

std_crops_poland_geoms_nuts2 <- obtain_std_crop_data(country_name = "poland") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(poland_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_poland_geoms_nuts2,
                         std_crop_name = "apple",
                         overlapping_sf = poland_sf)

std_crops_geoms_nuts2 <- bind_rows(std_crops_geoms_nuts2, std_crops_poland_geoms_nuts2)

#### g) serbia --------------------------------------------------------------

serbia_sf <- obtain_geoms_nuts(nuts_level = 2,
                               country_code = "RS") 

std_crops_serbia_geoms_nuts2 <- obtain_std_crop_data(country_name = "serbia") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(serbia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_serbia_geoms_nuts2,
                         std_crop_name = "pepper",
                         overlapping_sf = serbia_sf)

std_crops_geoms_nuts2 <- bind_rows(std_crops_geoms_nuts2, std_crops_serbia_geoms_nuts2)

## 1.4. NUTS1 from Eurostat  -----------------------------------

print(CropMED_nuts_1)

std_crops_geoms_nuts1 <- tibble(country_code = NULL,
                                adm_level = NULL,
                                adm_cat_local = NULL,
                                adm_id = NULL,
                                adm_name.x = NULL)

#### a) germany --------------------------------------------------------------

germany_sf <- obtain_geoms_nuts(nuts_level = 1,
                                country_code = "DE") 

std_crops_germany_geoms_nuts1 <- obtain_std_crop_data(country_name = "germany") |> 
  filter(str_detect(adm_level, "adm01")) |> 
  inner_join(germany_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_germany_geoms_nuts1,
                         std_crop_name = "sunflower",
                         overlapping_sf = germany_sf)

std_crops_geoms_nuts1 <- rbind(std_crops_geoms_nuts1, std_crops_germany_geoms_nuts1)

#### b) latvia --------------------------------------------------------------

latvia_sf <- obtain_geoms_nuts(nuts_level = 1,
                                country_code = "LV") 

std_crops_latvia_geoms_nuts1 <- obtain_std_crop_data(country_name = "latvia") |> 
  filter(str_detect(adm_level, "adm01")) |> 
  inner_join(latvia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_latvia_geoms_nuts1,
                         std_crop_name = "oats",
                         overlapping_sf = latvia_sf)

std_crops_geoms_nuts1 <- bind_rows(std_crops_geoms_nuts1, std_crops_latvia_geoms_nuts1)

#### c) scotland --------------------------------------------------------------

scotland_sf <- obtain_geoms_nuts(nuts_level = 1,
                                 country_code = "UK") |> 
  filter(adm_id == "UKM")

std_crops_scotland_geoms_nuts1 <- obtain_std_crop_data(country_name = "scotland") |> 
  filter(str_detect(adm_level, "adm01")) |> 
  inner_join(scotland_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_scotland_geoms_nuts1,
                         std_crop_name = "carrot",
                         overlapping_sf = scotland_sf)

std_crops_geoms_nuts1 <- bind_rows(std_crops_geoms_nuts1, std_crops_scotland_geoms_nuts1)

#### d) wales --------------------------------------------------------------

wales_sf <- obtain_geoms_nuts(nuts_level = 1,
                              country_code = "UK") |> 
  filter(adm_id == "UKL")

std_crops_wales_geoms_nuts1 <- obtain_std_crop_data(country_name = "wales") |> 
  filter(str_detect(adm_level, "adm01")) |> 
  inner_join(wales_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_wales_geoms_nuts1,
                         std_crop_name = "oilseed rape",
                         overlapping_sf = wales_sf)

std_crops_geoms_nuts1 <- bind_rows(std_crops_geoms_nuts1, std_crops_wales_geoms_nuts1)

## 1.5. NUTS0 from Eurostat  -----------------------------------

print(CropMED_nuts_0)

std_crops_geoms_nuts0 <- tibble(country_code = NULL,
                                adm_level = NULL,
                                adm_cat_local = NULL,
                                adm_id = NULL,
                                adm_name.x = NULL)

#### a) netherlands --------------------------------------------------------------

netherlands_sf <- obtain_geoms_nuts(nuts_level = 0,
                                country_code = "NL") 

std_crops_netherlands_geoms_nuts0 <- obtain_std_crop_data(country_name = "netherlands") |> 
  filter(str_detect(adm_level, "adm00")) |> 
  inner_join(netherlands_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_netherlands_geoms_nuts0,
                         std_crop_name = "green peas",
                         overlapping_sf = netherlands_sf)

std_crops_geoms_nuts0 <- rbind(std_crops_geoms_nuts0, std_crops_netherlands_geoms_nuts0)

#### b) slovenia --------------------------------------------------------------

slovenia_sf <- obtain_geoms_nuts(nuts_level = 0,
                                 country_code = "SI") 

std_crops_slovenia_geoms_nuts0 <- obtain_std_crop_data(country_name = "slovenia") |> 
  filter(str_detect(adm_level, "adm00")) |> 
  inner_join(slovenia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_slovenia_geoms_nuts0,
                         std_crop_name = "apple",
                         overlapping_sf = slovenia_sf)

std_crops_geoms_nuts0 <- bind_rows(std_crops_geoms_nuts0, std_crops_slovenia_geoms_nuts0)

#### bb lithuania --------------------------------------------------------------

lithuania_sf <- obtain_geoms_nuts(nuts_level = 0,
                                 country_code = "LT") 

std_crops_lithuania_geoms_nuts0 <- obtain_std_crop_data(country_name = "lithuania") |> 
  filter(str_detect(adm_level, "adm00")) |> 
  inner_join(lithuania_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())


#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_lithuania_geoms_nuts0,
                         std_crop_name = "cabbage",
                         overlapping_sf = lithuania_sf)

std_crops_geoms_nuts0 <- bind_rows(std_crops_geoms_nuts0, std_crops_lithuania_geoms_nuts0)

## 1.5. adm_03 other than NUTS  -----------------------------------
ne_adm03 <- read_sf(here("ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp"))

std_crops_geoms_adm03_nonuts <- tibble(country_code = NULL,
                                       adm_level = NULL,
                                       adm_cat_local = NULL,
                                       adm_id = NULL,
                                       adm_name.x = NULL)
#### a) algeria --------------------------------------------------------------
algeria_sf <- obtain_geoms_ne(country_code = "DZ")

std_crops_algeria_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "algeria") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(algeria_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_algeria_geoms_ne_adm03,
                         std_crop_name = "lentils",
                         overlapping_sf = algeria_sf)

std_crops_geoms_adm03_nonuts <- rbind(std_crops_geoms_adm03_nonuts, std_crops_algeria_geoms_ne_adm03)

#### b) cyprus --------------------------------------------------------------

cyprus_sf <- obtain_geoms_ne(country_code = "CY")

std_crops_cyprus_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "cyprus") |> 
  filter(str_detect(adm_level, "adm04")) |> 
  inner_join(cyprus_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_cyprus_geoms_ne_adm03,
                         std_crop_name = "sweet orange",
                         overlapping_sf = cyprus_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_cyprus_geoms_ne_adm03)

#### c) egypt --------------------------------------------------------------

egypt_sf <- obtain_geoms_ne(country_code = "EG")

std_crops_egypt_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "egypt") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(egypt_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_egypt_geoms_ne_adm03,
                         std_crop_name = "cotton",
                         overlapping_sf = egypt_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_egypt_geoms_ne_adm03)

#### d) jordan --------------------------------------------------------------

jordan_sf <- obtain_geoms_ne(country_code = "JO")

std_crops_jordan_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "jordan") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(jordan_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_jordan_geoms_ne_adm03,
                         std_crop_name = "snake cucumber",
                         overlapping_sf = jordan_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_jordan_geoms_ne_adm03)

#### e) lebanon --------------------------------------------------------------
## Shapefile at level 1 obtained from https://data.amerigeoss.org/tr/dataset/lebanon-administrative-boundaries-levels-0-3
lebanon_sf <- obtain_geoms_ne(country_code = "LB",
                              path = "Source Data Sets/Lebanon/lbn_adm1/lbn_admbnda_adm1_cdr_20241113.shp") |> 
  rename(adm_name = ADM0_EN,
         country_code = ADM0_PCODE,
         adm_id = ADM1_PCODE) |> 
  select(country_code, adm_name, adm_id, geometry)

std_crops_lebanon_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "lebanon") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(lebanon_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_lebanon_geoms_ne_adm03,
                         std_crop_name = "vineyards for table grapes",
                         overlapping_sf = lebanon_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_lebanon_geoms_ne_adm03)

#### f) moldova --------------------------------------------------------------

moldova_sf <- st_read("ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp") |> 
  filter(iso_a2 == "MD") |> 
  rename(adm_name = geonunit,
         country_code = iso_a2,
         adm_id = adm1_code) |> 
  select(country_code, adm_name, adm_id, geometry)
std_crops_moldova_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "moldova") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(moldova_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_moldova_geoms_ne_adm03,
                         std_crop_name = "sunflower",
                         overlapping_sf = moldova_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_moldova_geoms_ne_adm03)

#### g) morocco --------------------------------------------------------------
## SOURCE -> https://open.arfrica/dataset/morocco-maps/resource/d08eae57-767d-4ca1-8bf6-9d8e78fa5451
morocco_sf <- st_read("~/Maroc_adm/Morocco_adm2/mar_admbnda_adm2_hcp_20230925.shp") |> 
  rename(adm_name = ADM0_EN,
         country_code = ADM0_PCODE,
         adm_id = ADM2_PCODE) |>
  mutate(grouped_ids = case_when(adm_id %in% c("MA008002", "MA008003", "MA008001") ~"MA008002+MA008003+MA008001",
                                 adm_id %in% c("MA002003", "MA002004", "MA002005", "MA002006") ~"MA002003+MA002004+MA002005+MA002006",
                                 adm_id %in% c("MA001002", "MA001003") ~"MA001002+MA001003",
                                 TRUE ~ adm_id),
         grouped_units = case_when(adm_name %in% c("Rabat", "Salé", "Skhirate-Témara") ~"Rabat + Salé + Skhirate-Témara",
                                   adm_name %in% c("Mediouna", "Nouaceur", "Casablanca", "Mohammadia") ~"JOINED Casablanca",
                                   adm_name %in% c("Béni Mellal", "Fquih Ben Salah") ~"JOINED Béni Mellal + Fquih Ben Salah",
                                  TRUE ~ adm_name)) |> 
  group_by(country_code, grouped_ids, grouped_units) |> 
  summarise(geometry = st_union(geometry),
            .groups = "drop") |> 
  rename(adm_id = grouped_ids,
         adm_name = grouped_units) |> 
  select(country_code, adm_name, adm_id, geometry) 



std_crops_morocco_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "morocco") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(morocco_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_morocco_geoms_ne_adm03,
                         std_crop_name = "durum wheat",
                         overlapping_sf = morocco_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_morocco_geoms_ne_adm03)

#### h) northern_ireland --------------------------------------------------------------

northern_ireland_sf <- st_read("Source Data Sets/United Kingdom/Northern Ireland/local_gov_districts/OSNI_Open_Data_-_Largescale_Boundaries_-_Local_Government_Districts_(2012).shp") |> 
  mutate(country_code = "UK") |> 
  rename(adm_name = LGDNAME,
         adm_id = LGDCode) |> 
  st_as_sf() |> 
  st_transform(crs = 4326) |> 
  st_zm()  |>   # Remove Z coordinate
  st_cast("POLYGON")    

std_crops_northern_ireland_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "northern-ireland") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(northern_ireland_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_northern_ireland_geoms_ne_adm03,
                         std_crop_name = "potato",
                         overlapping_sf = northern_ireland_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_northern_ireland_geoms_ne_adm03)

#### i) palestine --------------------------------------------------------------
## shapefile from: https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-state-of-palestine
palestine_sf <- st_read("Source Data Sets/Palestine/palestine_borders_adm2/geoBoundaries-PSE-ADM2.shp") |> 
  mutate(country_code = "PS") |> 
  rename(adm_name = shapeName,
         adm_id = shapeISO)
  
std_crops_palestine_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "palestine") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(palestine_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_palestine_geoms_ne_adm03,
                         std_crop_name = "olive groves",
                         overlapping_sf = palestine_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_palestine_geoms_ne_adm03)

#### j) syria --------------------------------------------------------------
## shapefile from: https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-state-of-syria
syria_sf <- obtain_geoms_ne(country_code = "SY")

std_crops_syria_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "syria") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(syria_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_syria_geoms_ne_adm03,
                         std_crop_name = "cucumber",
                         overlapping_sf = syria_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_syria_geoms_ne_adm03)

#### k) tunisia --------------------------------------------------------------
## shapefile from: https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-state-of-tunisia
tunisia_sf <- st_read("Source Data Sets/Tunisia/gouvernorat shp/Gouvernorats_TN.shp") |> 
  mutate(country_code = "TN") |> 
  mutate(REF_TN_COD = map_chr(.x = REF_TN_COD,
                              .f = ~paste0("TN-", .x))) |> 
  rename(adm_name = name_fr,
         adm_id = REF_TN_COD) |> 
  st_transform(4326)


std_crops_tunisia_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "tunisia") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(tunisia_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_tunisia_geoms_ne_adm03,
                         std_crop_name = "pomegranate",
                         overlapping_sf = tunisia_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_tunisia_geoms_ne_adm03)


#### l) wales --------------------------------------------------------------
## shapefile from: https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-state-of-wales
wales_sf <- obtain_geoms_ne(country_code = "GB") |> 
  filter(adm_name == "Wales")


std_crops_wales_geoms_ne_adm03 <- obtain_std_crop_data(country_name = "wales") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(wales_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_wales_geoms_ne_adm03,
                         std_crop_name = "barley",
                         overlapping_sf = wales_sf)

std_crops_geoms_adm03_nonuts <- bind_rows(std_crops_geoms_adm03_nonuts, std_crops_wales_geoms_ne_adm03)


## 1.6. adm_02 other than NUTS  -----------------------------------
ne_adm02 <- read_sf("ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")

std_crops_geoms_adm02_nonuts <- tibble(country_code = NULL,
                                       adm_level = NULL,
                                       adm_cat_local = NULL,
                                       adm_id = NULL,
                                       adm_name.x = NULL)


#### a) ukraine --------------------------------------------------------------
## shapefile from: https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-state-of-ukraine
ukraine_sf <- obtain_geoms_ne(country_code = "UA")


std_crops_ukraine_geoms_ne_adm02 <- obtain_std_crop_data(country_name = "ukraine") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(ukraine_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_ukraine_geoms_ne_adm02,
                         std_crop_name = "sunflower",
                         overlapping_sf = ukraine_sf)

std_crops_geoms_adm02_nonuts <- rbind(std_crops_geoms_adm02_nonuts, std_crops_ukraine_geoms_ne_adm02)

#### b) belarus --------------------------------------------------------------
## shapefile from: https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-state-of-belarus
belarus_sf <- obtain_geoms_ne(country_code = "BY")

std_crops_belarus_geoms_ne_adm02 <- obtain_std_crop_data(country_name = "belarus") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(belarus_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_belarus_geoms_ne_adm02,
                         std_crop_name = "potato",
                         overlapping_sf = belarus_sf)

std_crops_geoms_adm02_nonuts <- bind_rows(std_crops_geoms_adm02_nonuts, std_crops_belarus_geoms_ne_adm02)

#### c) bosnia_herzegovina --------------------------------------------------------------
## shapefile for Bosnia and Herzegovina from: https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-bosnia-and-herzegovina
## shapefile for Republic Srpska from ne_data

republic_srpska_sf <- st_read("Source Data Sets/Bosnia/BA_shp/geoBoundaries-BIH-ADM3.shp") |> 
  mutate(country_code = "BA") |> 
  rename(adm_name = shapeName,
         adm_id = shapeID) |> 
  select(country_code, adm_name, adm_id, geometry) 

federation_bosnia_herzegovina_sf <- st_read("Source Data Sets/Bosnia/BA_shp/geoBoundaries-BIH-ADM2.shp") |> 
  mutate(country_code = "BA") |> 
  rename(adm_name = shapeName,
         adm_id = shapeID) |> 
  filter(adm_name != "Republic Srpska") |> 
  select(country_code, adm_name, adm_id, geometry) 

std_crops_bosnia_herzegovina_geoms_ne_adm02 <- obtain_std_crop_data(country_name = "bosnia-herzegovina") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(federation_bosnia_herzegovina_sf, by = "adm_id")

std_crops_republica_srpska_geoms_ne_adm02 <- obtain_std_crop_data(country_name = "bosnia-herzegovina") |> 
  filter(str_detect(adm_level, "adm03")) |> 
  inner_join(republic_srpska_sf, by = "adm_id")

std_crops_bih_srpska <- bind_rows(std_crops_republica_srpska_geoms_ne_adm02,
                                  std_crops_bosnia_herzegovina_geoms_ne_adm02) |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_bih_srpska,
                         std_crop_name = "potato",
                         overlapping_sf = republic_srpska_sf)

std_crops_geoms_adm02_nonuts <- bind_rows(std_crops_geoms_adm02_nonuts, std_crops_bih_srpska)

#### d) scotland --------------------------------------------------------------
## shapefile from: https://www.spatialdata.gov.scot/geonetwork/srv/api/records/f12c3826-4b4b-40e6-bf4f-77b9ed01dc14
scotland_sf <- st_read("Source Data Sets/United Kingdom/Scotland/scotland_shp/SG_NHS_HealthBoards_2019.shp") |> 
  rename(adm_id = HBCode,
         adm_name = HBName) |> 
  mutate(country_code = "GB") |> 
  select(country_code, adm_name, adm_id, geometry) |> 
  st_transform(4326) |> 
  mutate(grouped_ids = case_when(adm_id %in% c("S08000031", "S08000032") ~"S08000031+S08000032",
                                 TRUE ~ adm_id),
       grouped_units = case_when(adm_name %in% c("Greater Glasgow and Clyde", "Lanarkshire") ~"JOINED Greater Glasgow and Clyde + Lanarkshire",
                                 TRUE ~ adm_name)) |> 
  group_by(country_code, grouped_ids, grouped_units) |> 
  summarise(geometry = st_union(geometry),
            .groups = "drop") |> 
  rename(adm_id = grouped_ids,
         adm_name = grouped_units) |> 
  select(country_code, adm_name, adm_id, geometry) 
std_crops_scotland_geoms_ne_adm02 <- obtain_std_crop_data(country_name = "scotland") |> 
  filter(str_detect(adm_level, "adm02")) |> 
  inner_join(scotland_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_scotland_geoms_ne_adm02,
                         std_crop_name = "oats and triticale",
                         overlapping_sf = scotland_sf)

std_crops_geoms_adm02_nonuts <- bind_rows(std_crops_geoms_adm02_nonuts, std_crops_scotland_geoms_ne_adm02)

# 1.6. adm_01 other than NUTS  -----------------------------------
std_crops_geoms_adm01_nonuts <- tibble(country_code = NULL,
                                       adm_level = NULL,
                                       adm_cat_local = NULL,
                                       adm_id = NULL,
                                       adm_name.x = NULL)


#### a) belarus --------------------------------------------------------------
## shapefile from https://data.humdata.org/dataset/cod-ab-blr
belarus_sf <- st_read("Source Data Sets/Belarus/belarus_shp/blr_admbnda_adm0_unicef_20220727.shp") |> 
  rename(adm_id = ADM0_PCODE,
         adm_name = ADM0_EN) |> 
  mutate(country_code = "BY") |> 
  select(country_code, adm_name, adm_id, geometry)

std_crops_belarus_geoms_ne_adm01 <- obtain_std_crop_data(country_name = "belarus") |> 
  filter(str_detect(adm_level, "adm00")) |> 
  inner_join(belarus_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_belarus_geoms_ne_adm01,
                         std_crop_name = "oilseed rape",
                         overlapping_sf = belarus_sf)

std_crops_geoms_adm01_nonuts <- rbind(std_crops_geoms_adm01_nonuts, std_crops_belarus_geoms_ne_adm01)

#### b) bosnia-herzegovina --------------------------------------------------------------
## shapefile from https://data.humdata.org/dataset/cod-ab-blr
bosnia_herzegovina_sf <- st_read("Source Data Sets/Bosnia/BA_shp/geoBoundaries-BIH-ADM1.shp") |> 
  rename(adm_id = shapeISO,
         adm_name = shapeName) |> 
  mutate(country_code = "BA") |> 
  select(country_code, adm_name, adm_id, geometry)

std_crops_bosnia_herzegovina_geoms_ne_adm01 <- obtain_std_crop_data(country_name = "bosnia-herzegovina") |> 
  filter(str_detect(adm_level, "adm01")) |> 
  inner_join(bosnia_herzegovina_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_bosnia_herzegovina_geoms_ne_adm01,
                         std_crop_name = "soybean",
                         overlapping_sf = bosnia_herzegovina_sf)

std_crops_geoms_adm01_nonuts <- bind_rows(std_crops_geoms_adm01_nonuts, std_crops_bosnia_herzegovina_geoms_ne_adm01)

bosnia_herzegovina_sf_adm00 <- st_read("Source Data Sets/Bosnia/BA_shp/geoBoundaries-BIH-ADM0.shp") |> 
  rename(adm_id = shapeISO,
         adm_name = shapeName) |> 
  mutate(country_code = "BA") |> 
  select(country_code, adm_name, adm_id, geometry)

std_crops_bosnia_herzegovina_geoms_ne_adm00 <- obtain_std_crop_data(country_name = "bosnia-herzegovina") |> 
  filter(str_detect(adm_level, "adm00")) |> 
  inner_join(bosnia_herzegovina_sf_adm00, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_bosnia_herzegovina_geoms_ne_adm00,
                         std_crop_name = "apple",
                         overlapping_sf = bosnia_herzegovina_sf_adm00)

std_crops_geoms_adm01_nonuts <- bind_rows(std_crops_geoms_adm01_nonuts, std_crops_bosnia_herzegovina_geoms_ne_adm00)

#### c) kosovo --------------------------------------------------------------
## shapefile from https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/kosovo-shapefile
kosovo_sf <- st_read("ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  filter(GEOUNIT == "Kosovo") |> 
  rename(adm_id = ISO_A2_EH,
         adm_name = NAME) |> 
  mutate(country_code = "XK") |> 
  select(country_code, adm_name, adm_id, geometry)

std_crops_kosovo_geoms_ne_adm01 <- obtain_std_crop_data(country_name = "kosovo") |> 
  filter(str_detect(adm_level, "adm01")) |> 
  inner_join(kosovo_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_kosovo_geoms_ne_adm01,
                         std_crop_name = "plum",
                         overlapping_sf = kosovo_sf)

std_crops_geoms_adm01_nonuts <- bind_rows(std_crops_geoms_adm01_nonuts, std_crops_kosovo_geoms_ne_adm01)

#### d) northern ireland --------------------------------------------------------------
northern_ireland_adm01_sf <- obtain_geoms_nuts(nuts_level = 1,
                               country_code = "UK") |> 
  filter(adm_name == "Northern Ireland")  
  
std_crops_northern_ireland_geoms_ne_adm01 <- obtain_std_crop_data(country_name = "northern-ireland") |> 
  filter(str_detect(adm_level, "adm01")) |> 
  inner_join(northern_ireland_adm01_sf, by = "adm_id") |> 
  relocate(country_code, adm_level, adm_cat_local, adm_id, .before = everything())
#let's check everything is plotted
visualize_std_crop_geoms(std_crop_sf = std_crops_northern_ireland_geoms_ne_adm01,
                         std_crop_name = "barley",
                         overlapping_sf = northern_ireland_adm01_sf)

std_crops_geoms_adm01_nonuts <- bind_rows(std_crops_geoms_adm01_nonuts, std_crops_northern_ireland_geoms_ne_adm01)

# 1.7. join all geoms  -----------------------------------

std_crops_geoms <- bind_rows(std_crops_geoms_nuts3, 
                             std_crops_geoms_nuts2,
                             std_crops_geoms_nuts1,
                             std_crops_geoms_nuts0,
                             std_crops_geoms_adm03_nonuts,
                             std_crops_geoms_adm02_nonuts,
                             std_crops_geoms_adm01_nonuts)

## export it in .xslx wide format (a formatted copy will be available in the same 
## folder after modification with excel) and in csv

CropMED_std_crops_geoms <- std_crops_geoms |> 
  select(-adm_name.y) |> 
  mutate(adm_name.x = map2_chr(.x = adm_name.x,
                               .y = adm_name,
                               .f = ~case_when(is.na(.x) ~.y,
                                               .default = .x))) |>
  select(-adm_name, -AREA, - OBJECTID, -shapeID, -shapeGroup,
         - shapeType, -adm_units) |> 
  rename(adm_name = adm_name.x) |> 
  arrange(country_code) |> 
  relocate(country_code, adm_id, adm_name, adm_level, adm_cat_local, geometry) |> 
  mutate(geometry = st_as_text(geometry))

write_csv(CropMED_std_crops_geoms, file = here("CropMED_crops_admin.csv"))

















## export it in .xslx wide format (a formatted copy will be available in the same 
## folder after modification with excel) and in csv
CropMED_std <- readRDS("~/GitHub/CropMED datapaper/Sink Data Sets/CropMED_std_crops_geoms_wide.rds")
CropMED_std_crops_geoms_vert <- CropMED_std_crops_geoms |> 
  pivot_longer(cols = c(-country_code, -adm_id, -adm_name, -adm_level, -adm_cat_local,
                        -geometry),
              names_to = "std_crop",
              values_to = "hectares")
writexl::write_xlsx(CropMED_std_crops_geoms_vert,
                    path = "Sink Data Sets/CropMED_std_crops_geoms_long_RAW.xlsx")  
#write_csv(CropMED_std_crops_geoms_vert, file = "Sink Data Sets/CropMED_std_crops_geoms_long.csv")
#saveRDS(CropMED_std_crops_geoms_vert, file = "~/GitHub/CropMED datapaper/Sink Data Sets/CropMED_std_crops_geoms_long.rds")

##obtain unique crops to standardize groups and subgroups
unique_std_crops <- data.frame(names(CropMED_std_crops_geoms)[7:length(CropMED_std_crops_geoms)])
colnames(unique_std_crops) <- "std_crop_unique"
writexl::write_xlsx(x = unique_std_crops,
                    "~/GitHub/DAME datapaper/Sink Data Sets/unique_std_crops.xlsx")

## combine structure hierarchy
crop_group_hierarchy <- readxl::read_xlsx("~/GitHub/CropMED datapaper/Sink Data Sets/crop_hierarchy.xlsx") |> 
  rename(std_crop = std_crop_unique)

CropMED_std_groups_vert <- CropMED_std_crops_geoms_vert  |> 
  inner_join(crop_group_hierarchy)
write_rds(CropMED_std_groups_vert, 
          file = "~/GitHub/CropMED datapaper/Sink Data Sets/CropMED_std_groups_vert.rds")


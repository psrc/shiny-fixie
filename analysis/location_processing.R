source("functions.R")
library(sf)


# get PSRC layers
county_layer <- st_transform(st_read_elmergeo('COUNTY_BACKGROUND'), 2926)
city_layer <- st_transform(st_read_elmergeo('CITIES'), 2926)
center_layer <- st_transform(st_read_elmergeo('URBAN_CENTERS'), 2926)
rg_layer <- st_transform(st_read_elmergeo('REGIONAL_GEOGRAPHIES'), 2926)


# join county, city, center layers
get_psrc_geographies <- function(data, id, lng, lat, prefix_name){
  
  gdf <- data %>% 
    select(all_of(c(id,lng,lat))) %>%
    st_as_sf(coords = c(lng, lat), crs = 4326) %>%
    st_transform(2926)
  
  df <- gdf %>% 
    st_join(county_layer %>% select(county_nm), join = st_intersects) %>%
    st_join(city_layer %>% select(city_name), join = st_intersects) %>%
    st_join(center_layer %>% select(name), join = st_intersects) %>%
    st_join(rg_layer %>% select(class_desc), join = st_intersects) %>%
    rename(county = county_nm,
           city = city_name,
           center = name,
           rg = class_desc) %>%
    rename_with(~ paste0(prefix_name, .), all_of(c("county","city","center", "rg")))
  
  st_geometry(df) <- NULL
  
  return(df)
}

Trip_dest <- get_psrc_geographies(c_Trip,"recid","dest_lng","dest_lat","dest_")
Trip_origin <- get_psrc_geographies(c_Trip %>% filter(!is.na(origin_lat)),"recid","origin_lng","origin_lat","origin_")
Household_home <- get_psrc_geographies(c_Household %>% filter(!is.na(lng)),"hhid","lng","lat","home_")
Person_work <- get_psrc_geographies(c_Person %>% filter(!is.na(work_lat)),"person_id","work_lng","work_lat","work_")
Person_school <- get_psrc_geographies(c_Person %>% filter(!is.na(school_loc_lng)),"person_id","school_loc_lng","school_loc_lat","school_")



df_trip_loc <- c_Trip %>%
  select(recid) %>%
  left_join(Trip_dest, by = "recid") %>%
  left_join(Trip_origin, by = "recid")
df_person_loc <- c_Person %>%
  select(person_id) %>%
  left_join(Person_work, by = "person_id") %>%
  left_join(Person_school, by = "person_id")
df_hh_loc <- Household_home
  

saveRDS(list(df_trip_loc,df_person_loc,df_hh_loc), "df_loc.RDS")

source("functions.R")

# read data
Trip <- get_query("SELECT * FROM dbo.ex_trip_unlinked", db_name = "HouseholdTravelSurvey2025")
Trip_linked <- get_query("SELECT * FROM dbo.ex_trip_linked", db_name = "HouseholdTravelSurvey2025")

Household <- get_query("SELECT * FROM dbo.ex_hh", db_name = "HouseholdTravelSurvey2025")
Person <- get_query("SELECT * FROM dbo.ex_person", db_name = "HouseholdTravelSurvey2025")
Day <- get_query("SELECT * FROM dbo.ex_day", db_name = "HouseholdTravelSurvey2025")
Vehicle <- get_query("SELECT * FROM dbo.ex_vehicle", db_name = "HouseholdTravelSurvey2025")
Location <- get_query("SELECT * FROM dbo.ex_location", db_name = "HouseholdTravelSurvey2025")

# data processing
df_trip <- Trip %>%
  get_county("o_bg","o_county") %>%
  get_county("d_bg","d_county")

df_person <- Person %>%
  get_county("work_bg","work_county")

# analysis data
tables <- list("trip_unlinked"=df_trip,
               "trip_linked"=Trip_linked,
               "hh"=Household,
               "person"=df_person,
               "day"=Day,
               "vehicle"=Vehicle,
               "location"=Location)
saveRDS(tables, "analysis_table.RDS")

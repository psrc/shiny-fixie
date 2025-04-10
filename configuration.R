

# names of views for edits
# have to fix number of tabs in server/ui after changing number of tabs
# TODO: view name can be typed in by the analyst editing the view
edit_persons_view_name <- c("person_all","person_Mike")

# list of columns showing in trip table ----
view.cols <- c("tripnum","modes_desc","daynum","depart_dhm","arrive_dhm","miles","mph","Error",
               "cotravelers","origin_purpose","dest_purpose","dest_name","duration_at_dest",
               "origin_coord","dest_coord","recid","rc","elevate_issue")

edit.cols <- c("depart_time_timestamp", "arrival_time_timestamp", "distance_miles", "origin_purpose", "origin_label", "origin_lat", 
               "origin_lng", "dest_lat", "dest_lng", "mode_1", "mode_2", "mode_3", "mode_4", "mode_acc", "mode_egr", "driver", "travelers_total", 
               "travelers_hh", "mode_egr", "hhmember1", "hhmember2", "hhmember3", "hhmember4", "hhmember5", "hhmember6", "hhmember7", "hhmember8")

# get variable values
codebook <- readxl::read_xlsx("J:/Projects/Surveys/HHTravel/Survey2023/Data/old_stuff/data_deliverable_81823/codebook_guide/PSRC_Combined_Codebook_2023_08162023.xlsx", 
                                   sheet = 'value_labels_2023')

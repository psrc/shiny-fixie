

# names of views for edits
# have to fix number of tabs in server/ui after changing number of tabs
# TODO: view name can be typed in by the analyst editing the view
edit_persons_view_name <- c("person_all_test","person_all_error","person_too_long_at_dest","person_mode_1_missing")

# list of columns showing in trip table ----
view.cols <- c("tripnum","modes_desc","daynum","depart_dhm","arrive_dhm","miles","mph","Error",
               "cotravelers","origin_purpose","dest_purpose","dest_name","duration_at_dest",
               # "origin_coord","dest_coord",
               "recid","rc","elevate_issue")

# get variable values
codebook <- readxl::read_xlsx("J:/Projects/Surveys/HHTravel/Survey2023/Data/old_stuff/data_deliverable_81823/codebook_guide/PSRC_Combined_Codebook_2023_08162023.xlsx", 
                                   sheet = 'value_labels_2023')

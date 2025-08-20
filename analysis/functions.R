library(tidyverse)
library(psrcelmer)

# lookup tool
library(qdapTools)
# summary table
library(tableone)

codebook <- read_csv("value_labels.csv")

get_labels <- function(.data, varname, table_name, order=TRUE){
  
  var_lookup <- codebook[codebook$table == table_name & codebook$variable == varname,]
  var_lookup <- data.frame(var_lookup$value,var_lookup$label)
  
  s_unordered <- lookup(.data, var_lookup)
  s_ordered <- factor(s_unordered, levels=var_lookup[['var_lookup.label']])
  
  return( if(order){s_ordered} else{s_unordered} )
}

get_county <- function(.data, varname, rename){
  counties <- data.frame(value=c("53033", "53035", "53053", "53061"),
                         county = c("King","Kitsap","Pierce","Snohomish")
  )
  
  test <- .data %>%
    mutate(geog_county = lookup(substr(.[[varname]],1,5),counties))
  .data[[rename]] <- test[['geog_county']]
  
  return(.data)
  
}

# get summary table
get_vars_summary <- function(table_name, summary_vars, order = TRUE){
  
  df <- tables[[table_name]] %>%
    mutate(across(all_of(summary_vars), ~get_labels(., varname = cur_column(), table_name, order = order)))
  
  return(
    CreateTableOne(data = df,
                   vars = summary_vars,
                   includeNA = TRUE
    )
  )
}
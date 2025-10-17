edit_interface_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("editplatform"))
  )
}

#' HTS data editor user interface
edit_interface_server <- function(id, selected_error_type) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
  # person control panel ----
  personID <- person_panel_server("panel-person", selected_error_type=reactive(selected_error_type()))
    
    # the trip table ----
    
    # person data from database
    edit_dt <- reactive({ get_data(person_id = personID(), order_by=c("person_id", "tripnum")) })
    
    output$thetable <- DT::renderDataTable(
      
      edit_dt() %>% 
        # remove missing response pattern
        mutate(Modes = str_replace(Modes, ",?Missing Response,?", "")) %>%
        select(-c("person_id")),
      
      class = list('hover row-border order-column'),
      options =list(ordering = F, 
                    dom = 't',
                    pageLength = -1,
                    scroller = TRUE,
                    scrollY = '60vh',
                    scrollCollapse = TRUE),
      fillContainer = T,
      rownames = FALSE,
      server=TRUE
      
      )
    
    
    # data cleaning tools ----
    
    selected_row_recid <- reactive({ edit_dt()[input$thetable_rows_selected, "recid"] })

    # selected trips (full rows) to drive the map
    selected_trips_df <- reactive({
      req(edit_dt())
      rows <- input$thetable_rows_selected
      if (is.null(rows) || length(rows) == 0) return(NULL)
      # order by tripnum to define color ramp order
      df <- edit_dt()[rows, , drop = FALSE]
      if (all(c("tripnum") %in% names(df))) {
        df <- df %>% arrange(.data$tripnum)
      }
      df
    })

    # Leaflet map: base
    output$trip_map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) %>%
        leaflet::addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                          attribution = "&copy; OpenStreetMap contributors") %>%
        leaflet::setView(lng = -122.33, lat = 47.61, zoom = 9)
    })

    # helper: compute bearing in degrees from origin to destination
    .bearing_deg <- function(lat1, lon1, lat2, lon2){
      toRad <- function(x) x * pi / 180
      toDeg <- function(x) x * 180 / pi
      phi1 <- toRad(lat1); phi2 <- toRad(lat2)
      dlam <- toRad(lon2 - lon1)
      y <- sin(dlam) * cos(phi2)
      x <- cos(phi1) * sin(phi2) - sin(phi1) * cos(phi2) * cos(dlam)
      (toDeg(atan2(y, x)) + 360) %% 360
    }

    # Update map when selections change
    observe({
      df <- selected_trips_df()
      proxy <- leaflet::leafletProxy("trip_map")
      proxy %>% leaflet::clearGroup("trips")
      if (is.null(df) || nrow(df) == 0) return(invisible())

      # ensure needed cols exist
      needed <- c("origin_lat","origin_lng","dest_lat","dest_lng")
      if (!all(needed %in% names(df))) return(invisible())

      n <- nrow(df)
      cols <- grDevices::colorRampPalette(c("#e41a1c","#ff7f00","#ffff33","#4daf4a","#377eb8"))(max(n, 2))[seq_len(n)]

      # draw each selected trip
      for (i in seq_len(n)){
        r <- df[i, ]
        olat <- suppressWarnings(as.numeric(r$origin_lat)); olon <- suppressWarnings(as.numeric(r$origin_lng))
        dlat <- suppressWarnings(as.numeric(r$dest_lat)); dlon <- suppressWarnings(as.numeric(r$dest_lng))
        if (any(is.na(c(olat,olon,dlat,dlon)))) next

        # polyline
        proxy %>% leaflet::addPolylines(lng = c(olon, dlon), lat = c(olat, dlat),
                                        color = cols[i], weight = 4, opacity = 0.9,
                                        group = "trips")

        # origin and destination markers with hover labels
        dep_lab <- tryCatch({
          if ("depart_time_timestamp" %in% names(r)) paste0("Depart: ", format(r$depart_time_timestamp, "%Y-%m-%d %H:%M")) else NULL
        }, error = function(e) NULL)
        arr_lab <- tryCatch({
          if ("arrival_time_timestamp" %in% names(r)) paste0("Arrive: ", format(r$arrival_time_timestamp, "%Y-%m-%d %H:%M")) else NULL
        }, error = function(e) NULL)

        proxy %>% leaflet::addCircleMarkers(lng = olon, lat = olat, radius = 5,
                                            fillColor = cols[i], color = cols[i],
                                            fillOpacity = 0.9, stroke = FALSE,
                                            label = dep_lab, labelOptions = leaflet::labelOptions(noHide = FALSE),
                                            group = "trips")

        # destination marker as arrowhead if rotate plugin available, else circle
        has_extras <- requireNamespace("leaflet.extras", quietly = TRUE)
        if (has_extras) {
          angle <- .bearing_deg(olat, olon, dlat, dlon)
          icn <- leaflet::makeAwesomeIcon(icon = "location-arrow", library = "fa",
                                          markerColor = "white", iconColor = cols[i])
          proxy <- leaflet.extras::addRotateMarkers(map = proxy, lng = dlon, lat = dlat,
                                                    icon = icn, rotationAngle = angle,
                                                    group = "trips",
                                                    label = arr_lab,
                                                    labelOptions = leaflet::labelOptions(noHide = FALSE))
        } else {
          proxy %>% leaflet::addCircleMarkers(lng = dlon, lat = dlat, radius = 5,
                                              fillColor = cols[i], color = cols[i],
                                              fillOpacity = 0.9, stroke = FALSE,
                                              label = arr_lab, labelOptions = leaflet::labelOptions(noHide = FALSE),
                                              group = "trips")
        }

        # dwell time label at destination (fixed small label)
        dwell_txt <- NULL
        full <- edit_dt()
        if (all(c("arrival_time_timestamp","depart_time_timestamp","recid") %in% names(full))) {
          # compute next depart from full dataset for this person
          this_recid <- if ("recid" %in% names(r)) r$recid else NA
          idx <- if (!is.na(this_recid)) which(full$recid == this_recid)[1] else NA
          if (!is.na(idx) && idx < nrow(full)) {
            next_dep <- suppressWarnings(full$depart_time_timestamp[idx + 1])
            arr_tm <- r$arrival_time_timestamp
            if (!is.na(next_dep) && !is.na(arr_tm)) {
              mins <- as.numeric(difftime(next_dep, arr_tm, units = "mins"))
              if (!is.na(mins) && is.finite(mins) && mins >= 0) {
                dwell_txt <- paste0("Dwell ", round(mins), " min")
              }
            }
          }
        }
        if (!is.null(dwell_txt)) {
          proxy %>% leaflet::addLabelOnlyMarkers(lng = dlon, lat = dlat,
                                                 label = dwell_txt,
                                                 labelOptions = leaflet::labelOptions(noHide = TRUE,
                                                                                      textOnly = TRUE,
                                                                                      direction = "right",
                                                                                      style = list(
                                                                                        "color" = "#333",
                                                                                        "font-size" = "11px",
                                                                                        "background" = "rgba(255,255,255,0.7)",
                                                                                        "padding" = "1px 3px",
                                                                                        "border-radius" = "2px"
                                                                                      )),
                                                 group = "trips")
        }
      }

      # fit bounds to all points
      all_lng <- c(suppressWarnings(as.numeric(df$origin_lng)), suppressWarnings(as.numeric(df$dest_lng)))
      all_lat <- c(suppressWarnings(as.numeric(df$origin_lat)), suppressWarnings(as.numeric(df$dest_lat)))
      all_lng <- all_lng[is.finite(all_lng)]; all_lat <- all_lat[is.finite(all_lat)]
      if (length(all_lng) >= 2 && length(all_lat) >= 2) {
        proxy %>% leaflet::fitBounds(lng1 = min(all_lng), lat1 = min(all_lat),
                                     lng2 = max(all_lng), lat2 = max(all_lat))
      }
    })
    
    ## button to add new trip
    modal_new_trip_server("button_new", selected_recid = reactive(selected_row_recid()))
    ## activate Edit Trip modal
    modal_edit_trip_server("button_edit", selected_recid = reactive(selected_row_recid()))
    ## button to delete trip
    modal_delete_trip_server("button_delete", selected_recid = reactive(selected_row_recid()))
    ## link trips interface
    modal_link_trips_server("button_link", selected_recid = reactive(selected_row_recid()))
    ## unlink trip interface
    modal_unlink_trip_server("button_unlink", selected_recid = reactive(selected_row_recid()))
    
    # platform layout ----
    
    output$editplatform <- renderUI({
      tagList(
        # Header: left (person and actions) + right (map)
        fluidRow(
          column(6,
                 div(style = "margin: 0 1rem;",
                     # person selector and summary
                     person_panel_ui(ns("panel-person")),
                     # actions compact panel
                     div(
                       class = "trip-buttons-panel well",
                       style = "padding: 10px; margin-top: 6px;",
                       div(p("Select one trip to edit")),
                       div(
                         style = "display:flex; flex-wrap:wrap; gap:6px;",
                         modal_new_trip_ui(ns('button_new')),
                         modal_edit_trip_ui(ns('button_edit')),
                         modal_delete_trip_ui(ns('button_delete')),
                         modal_unlink_trip_ui(ns('button_unlink'))
                       ),
                       div(style = "margin-top:6px; display:flex; align-items:center; gap:8px;",
                           tags$span("Select multiple trips to"),
                           modal_link_trips_ui(ns('button_link'))
                       )
                     )
                 )
          ),
          column(6,
                 div(style = "margin: 0 1rem;",
                     leaflet::leafletOutput(ns("trip_map"), height = "380px")
                 )
          )
        ),

        # Trip table
        fluidRow(column(12, DT::dataTableOutput(ns("thetable"))))
      )
    }) 
    
  }) 
}


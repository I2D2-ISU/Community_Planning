# LOAD Libraries ---------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(waiter)



# START Services UI ------------------------------------------------------------
ui <- page_sidebar(
  # Set Bootstrap to version 5
  theme = bslib::bs_theme(version = 5), 
  
  # Add title for browser
  window_title = "Iowa Data Drive",

  # Add shiny settings
  tagList(
    tags$head(
      # Add CSS styling
      includeCSS("www/style.css"),
      
      # Add loader to the whole dashboard and set style for individual figures
      useWaiter(),
      waiterPreloader(html = spin_5(), color = "grey")
    )
  ),
  
  # CREATE Left-Side-Bar UI ----------------------------------------------------
  sidebar = sidebar(
    selectizeInput(inputId = "GEOGRAPHY_SELECT", label = strong("Select Geography"),
                   choices = list(
                     "Iowa Counties"        = "IA-county",
                     "ECI Areas"            = "ECI-area",
                     "HHS Regions"          = "HHS-region",
                     "Head Start Grantees"  = "HS-grantee",
                     "MIECHV"               = "MIECHV-area"
                   ),
                   selected = 'IA-county', multiple = FALSE),
    
    selectizeInput(inputId = "LOCATION_SELECT", 
                   label = strong("Select Location"), 
                   choices = list("Adair"  = 19001), 
                   selected = 19001, 
                   multiple = FALSE),
    br(),
    strong("Adjust Map"),
    shiny::checkboxInput('MAP_COUNTY_OUTLINES', 
                         "Show Outline of Selected Locations", 
                         value = TRUE),
    shiny::checkboxInput('MAP_COUNTY_LABELS', 
                         "Show Names of Selected Locations", 
                         value = FALSE),
    br()
  ),

  # CREATE Tab Panels for each Measure -----------------------------------------
  navset_card_pill(
    id = "MEASURE",

    ## .. Community Planning UI --------------------------------------------------

    nav_panel(
      title = "Community Planning",
      fluidRow(
        column(
          width = 5,
          fluidRow(
            column(
              width = 12,
              h3("Indicator Map"),
              selectInput(
                inputId = "CP_INDICATOR",
                label = NULL, # label = strong("Indicator"),
                choices = static_5_indicators,
                width = "100%"
              ),
              # shiny::plotOutput("CP_MAP"),
              leaflet::leafletOutput("CP_LEAF", height = '400px'),
              p("Select Colors"),
              selectInput(
                inputId = "CP_MAP_COL",
                label = NULL, #"Chose Color Palette for Map",
                choices = map_color_choices,
                selected = "colors_red_gradient"
              ),
              # shiny::downloadButton("CP_MAP_DOWNLOAD", label = "Download the Map"),
              br(),
              h3("Area Characteristic"),
              DTOutput("CP_TABLE")
            )
          )
        ),
        column(
          width = 7,
          h3('Ranking Plot'),
          selectInput(
            inputId = "CP_LINE_NUMBER",
            label = NULL, 
            choices = seq(5, 15, 5), 
            selected = 5,
            width = "60%"
          ),
          shiny::plotOutput("CP_LINE", height = "auto"),
          shiny::downloadButton("CP_DATA_DOWNLOAD", label = "Download Data"),
          shiny::downloadButton("CP_LINE_DOWNLOAD", label = "Download Figure")
        )
      ),
    # # TEST ---------------------------------------------------------------------
    #   fluidRow(
    #     h1("TESTING"),
    #     h3("Check Elements"),
    #     shiny::tableOutput("TABLE1")
    #   ),
      br()
    )
  # END of Tab Panels ----------------------------------------------------------
  )
)

# START Server side ------------------------------------------------------------
server <- function(input, output, session) {
  
  # # TEST -----------------------------------------------------------------------
  # output$TABLE1 <- renderTable({
  #   rank_data.cp() %>%
  #   # map_data.cp() %>%
  #   # table_data.cp() %>%
  #     # arrange(location) %>%
  #     head(20)
  # })
  
  
  # UPDATE DROP-DOWNS ----------------------------------------------------------

  ### ··· Load drop-down data for LOCATION -------------------------------------
  dropdown_data.locations <-
    reactive({
      # require that the trigger has a value to prevent error at startup
      req(input$GEOGRAPHY_SELECT)
      # read correct sf data file based on selected geography
      switch(input$GEOGRAPHY_SELECT,
             "IA-county"   = ia_county_droplist,
             "ECI-area"    = eci_area_droplist,
             "HHS-region"  = hhs_region_droplist,
             "HS-grantee"  = hs_grantee_droplist,
             "MIECHV-area" = miechv_droplist
      )
    })

  ### ··· Create LOCATION drop-down list ---------------------------------------
  observeEvent(
    input$GEOGRAPHY_SELECT,
    updateSelectizeInput(session = session,
                         inputId = 'LOCATION_SELECT',
                         choices = dropdown_data.locations(),
                         options = list(maxItems = 1L, placeholder = 'Select Location(s)'),
                         server = TRUE)
  )

  # LOAD DATA ---------------------------------------------------------------

  ## > Get base map of selected geographies
  base.map.geos <- reactive({
    # require that the trigger has a value to prevent error at startup
    req(input$GEOGRAPHY_SELECT)
    # read correct sf data file based on selected geography
    switch(input$GEOGRAPHY_SELECT,
           "IA-county"   = read_my_rds("map_IA-county.rds"),
           "ECI-area"    = read_my_rds("map_ECI-areas.rds"),
           "HHS-region"  = read_my_rds("map_HHS-regions.rds"),
           "HS-grantee"  = read_my_rds("map_HS-grantess.rds"),
           "MIECHV-area" = read_my_rds("map_MIECHV-areas.rds")
    )
  })
  
  ## > Get indicator data of selected geographies
  data.cp <- reactive({
    # require that the trigger has a value to prevent error at startup
    req(input$GEOGRAPHY_SELECT)
    # read correct sf data file based on selected geography
    data <- 
      switch(input$GEOGRAPHY_SELECT,
             "IA-county"   = read_my_csv("IDD-Ranking_IA-county.csv"),
             "ECI-area"    = read_my_csv("IDD-Ranking_ECI-area.csv"),
             "HHS-region"  = read_my_csv("IDD-Ranking_HHS-region.csv"),
             "HS-grantee"  = read_my_csv("IDD-Ranking_HS-grantee.csv"),
             "MIECHV-area" = read_my_csv("IDD-Ranking_MIECHV-area.csv")
             )
    return(data)
  })
  
  current.location.name <- reactive({
    req(input$LOCATION_SELECT)
    names(dropdown_data.locations()[dropdown_data.locations()==input$LOCATION_SELECT])
  })

 
  ### ··· MAP ------------------------------------------------------------------
  ### ··· Get selected CP data for map
  map_data.cp <- reactive({
    req(nrow(data.cp()) > 0)
    req(input$CP_INDICATOR)

    data <-
      data.cp() %>%
      filter(fips != 19) %>%
      filter(indicator == input$CP_INDICATOR) %>%
      mutate(label = sprintf(paste0("%.", 1, "f%s"), index*100, "%"),
             index = ifelse(index == -9999, NA_real_, index)) %>%
      rowwise() %>%
      mutate(popup_label =
               htmltools::HTML(sprintf('<b>%s</b>
                         <br><span style="padding-left: 10px;">year: <b>%s</b>
                         <br><span style="padding-left: 10px;">%s: <b>%s</b>',
                                       str_replace_all(str_wrap(location, width = 20), "\n", "<br>"), 
                                       year, "value", label))
             ) %>%
      ungroup()
      
    return(data)
  })

  ### ··· Render leaflet map
  output$CP_LEAF <- renderLeaflet({
    plot_map_leaflet(
      DATA      <- map_data.cp(),
      BASE_MAP  <- base.map.geos(),
      LOCATIONS <- input$LOCATION_SELECT, 
      OUTLINES  <- input$MAP_COUNTY_OUTLINES,
      LABELS    <- input$MAP_COUNTY_LABELS,
      COL       <- input$CP_MAP_COL
    )
  })

  # ### ··· Plot map for selected CP indicator
  # map.cp <- reactive({
  #   req(map_data.cp(), base.map.geos())
  #   req(nrow(map_data.cp()) > 0)
  #   plot_map_view(DATA = map_data.cp(),
  #                 BASE_MAP = base.map.geos(),
  #                 LOCATIONS = input$LOCATION_SELECT,
  #                 DATA_TYPE = "percent", #current_indicator_type(),
  #                 OUTLINES = input$MAP_COUNTY_OUTLINES,
  #                 LABELS = input$MAP_COUNTY_LABELS,
  #                 COL = input$CP_MAP_COL)
  # })
  
  
  ### ... TABLE ----------------------------------------------------------------
  ### ··· Get selected CP data for table
  table_data.cp <- reactive({
    req(data.cp(), nrow(data.cp()) > 0)
    req(input$LOCATION_SELECT)

    data <-
      data.cp() %>%
      filter(
        fips %in% c(19, input$LOCATION_SELECT),
        indicator %in% static_5_indicators
        ) %>% 
      mutate(label = sprintf(paste0("%.", 1, "f%s"), index*100, "%"),
             index = ifelse(index == -9999, NA_real_, index)) %>%
      select(indicator, location, label) %>%
      spread(location, label) %>%
      select(Characteristic = indicator, Statewide, everything()) %>%
      ungroup()
    
    return(data)
  })

  ### ··· Render table for selected 5 CP indicators
  output$CP_TABLE <- renderDT({
    datatable(table_data.cp(),
              rownames = FALSE, 
              options = list(
                searching = FALSE,  # Removes the search box
                paging = FALSE,     # Removes pagination and page buttons
                info = FALSE,       # Removes the "Showing 1 to X of Y entries" text
                columnDefs = list(
                  list(className = 'dt-center', targets = c(1, 2)), # Centers numeric text
                  list(className = 'dt-left', targets = 0)          # Left aligns characteristics
                  )
                )
              )
  })

    
  ### ··· LINE ----------------------------------------------------------------
  ### ··· Get selected CP data for trend line
  rank_data.cp <- reactive({
    req(data.cp(), nrow(data.cp()) > 0)
    req(input$LOCATION_SELECT)
    
    df <- data.cp() %>%
      filter(!indicator %in% static_5_indicators) 
    
    data_range <- 
      df %>% 
      filter(index >= 0) %>% 
      group_by(indicator) %>%
      summarise(
        index_min = min(index, na.rm = TRUE), 
        index_max = max(index, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        label_min =
          ifelse(indicator %in% ratio_indicators,
                 format(round(index_min, 1), big.mark = ',', trim = TRUE),
                 sprintf(paste0("%.", 1, "f%s"), index_min*100, "%")),
        label_max =
          ifelse(indicator %in% ratio_indicators,
                 format(round(index_max, 1), big.mark = ',', trim = TRUE),
                 sprintf(paste0("%.", 1, "f%s"), index_max*100, "%")),
      )
    
    data_statewide <-
      df %>%
      filter(fips == 19) %>%
      select(indicator, statewide = index) %>%
      mutate(
        statewide_label =
          ifelse(indicator %in% ratio_indicators,
                 format(round(statewide, 1), big.mark = ',', trim = TRUE),
                 sprintf(paste0(" %.", 1, "f%s"), statewide*100, "%"))
      )
    
    data_rank <-
      df %>%
      filter(fips == input$LOCATION_SELECT) %>%
      mutate(
        label =
          ifelse(indicator %in% ratio_indicators,
                 format(round(index, 1), big.mark = ',', trim = TRUE),
                 sprintf(paste0("%.", 1, "f%s"), index*100, "%")),
        # show suppressed instead of NA on popup/tooltip box
        label =
          case_when(
            index == -9999 ~ "Suppressed",
            is.na(index) ~ "Not Available/Not Applicable",
            TRUE ~ as.character(label)),
        index = ifelse(index == -9999, NA_real_, index)
      ) %>%
      select(indicator, index, ranking, label)
    
    data <- 
      data_range %>%
      left_join(data_statewide, by = "indicator") %>%
      left_join(data_rank, by = "indicator") %>%
      arrange(ranking, indicator) %>%
      mutate(needs = row_number()#,
             # needs_label = paste0("Top Needs #", needs, "\n", str_wrap(indicator, width = 20))
             ) 
    
    return(data)
  })
  
  
  ### ··· Plot line graph for selected CP indicators
  line.cp <- reactive({
    plot_line_view(
      DATA = rank_data.cp(),
      LOCATION = current.location.name(),
      FONT = 12,
      TOP_RANK = as.integer(input$CP_LINE_NUMBER))
  })

  ### ··· Render line for selected CP indicator
  output$CP_LINE <- renderPlot({
    line.cp()
  }, 
  res = 72,
  height = function() {
    # Calculate height base on number of ranked indicators selected
    additional <- (as.integer(input$CP_LINE_NUMBER) - 5)
    if (additional == 0) return(400)
    return(400 + (additional * 75))
  })

  ### ... Download data
  output$CP_DATA_DOWNLOAD <- downloadHandler(
    filename = function() {
      paste("I2D2-Ranking-Data-Export-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- rank_data.cp() %>%
        mutate(Location = current.location.name()) %>%
        arrange(needs) %>%
        select(Location,
               Indicator = indicator,
               `Top Need` = needs,
               Rank = ranking,
               Index = label,
               Statewide = statewide_label,
               Min = label_min,
               Max = label_max
               )
      write.csv(data, file, row.names = FALSE, na = "")
    }
  )

  ### ··· Download line graph for selected CP indicators
  output$CP_LINE_DOWNLOAD <- downloadHandler(
    filename = function() { paste0("I2D2-Ranking-Data-Export-", Sys.Date(), ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <- line.cp()
        # format_map_download(map.cp(), fig_titles, current_indicator_source_fig)
      download_height <- 8 * as.integer(input$CP_LINE_NUMBER) / 5
      ggsave(file,
             plot = download_fig,
             width = 10, height = download_height, scale = 0.95, dpi = 150, bg = "white")
    }
  )
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

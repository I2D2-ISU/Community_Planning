# Load required packages -------------------------------------------------------
# Only load tidyverse packages that is used by shiny, 
# those that are not used comment out.
# Packages directly related to shiny are loaded with app.R script 

library(AzureStor)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)
library(tidyr)
library(ggtext)
library(sf)
library(leaflet)
library(leaflegend)
library(DT)



# FUNCTIONS --------------------------------------------------------------------

## Function to create named list ----------------
# it assume that the first column of input dataframe is a numeric code 
# assigned to geography and the second column is a name to be displayed  
create_named_list <- function(df) {
  my_list <- as.list(as.numeric(df[[1]]))
  names(my_list) <- df[[2]]
  return(my_list)
}



# Function to Read Indicator Data Files ----------------------------------------
# if (is.na(Sys.getenv("SHINY_SERVER_VERSION", unset = NA))) {
if (dir.exists("data")) {
  message("Reading data from local CyBox...")
  
  # Read csv files from CyBox and transform
  read_my_csv <- function(file_name) {
    read_csv(file.path("data/Blob", file_name), col_types = "icicdi") %>%
      filter(!fips %in% 699)
  }
  
} else {
  message("Reading data from Azure Storage...")
  
  # Read environmental variables from local env file
  readRenviron("../.Renviron")
  
  # Set endpoint for azure container 
  azure_container_con <- function(container_name = "iowa-data-drive"){
    # Get variables from local environment
    account_url <- Sys.getenv("AZURE_STORAGE_URL") # WA_AZURE_DATA_URL
    access_sas <- Sys.getenv("AZURE_STORAGE_SAS") # WA_AZURE_STORAGE_URL https://isuaabiddstg.blob.core.windows.net/iowa-data-drive
    # Create a storage endpoint object
    endp_sas <- storage_endpoint(account_url, sas = access_sas)
    container <- storage_container(endpoint = endp_sas, name = container_name)
    return(container)
  }
  
  # Read csv files from Azure Blob and transform
  read_my_csv <- function(dir, geo, file_name) {
    storage_read_csv(azure_container_con(), 
                     file.path("Dashboards", "Community_Planning", file_name), 
                     col_types = "icicdi") %>%
      filter(!fips %in% 699)
  }
}


# Function to Read Geography Files ----------------------------------------
if (dir.exists("data/Geos")) {
  read_my_rds <- function(file_name) {
    read_rds(file.path("data/Geos", file_name))
  }
} else {
  read_my_rds <- function(file_name) {
    read_rds(file.path("../common/data", file_name))
  }
}


# Set themes for plots ---------------------------------------------------------

# theme for maps
theme_view_map <- 
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.height = unit(4, "mm"),
        legend.key.width = unit(16, "mm"),
        legend.justification = c(0.45, 0.9),
        legend.text = element_text(size = 16, face = "bold"),
        text = element_text(family = 'Arial')
  ) 

# theme for line
theme_view_line <-
  theme_void() +
  theme(
    # Set height-to-width ratio per facet (e.g., 0.25 means height is 1/4th of width)
    aspect.ratio = 0.10,
    strip.text = element_blank(), # Hide default facet strip headers
    panel.background = element_rect(fill = "#DCEAF7", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    panel.spacing = unit(0.8, "lines"), # Gap between indicator boxes
    plot.margin = margin(15, 15, 15, 15),
    
    legend.position = "bottom",          # Place at bottom of the entire plot
    legend.title = element_blank(),      # Hide the word "Legend" if preferred
    legend.text = element_text(face = "bold", size = 16),
    legend.box = "horizontal",
    legend.key = element_blank() # Clears any gray boxes behind icons
  )

# Functions for making plots ---------------------------------------------------

## > DATA for MAP
plot_line_view <- function(DATA, LOCATION = "Selected Location", FONT = 12, TOP_RANK = 5){
  # Set a font size
  font_size_pt <- FONT
  
  # Set names for line chart legend
  legend_items <- c("Statewide", as.character(LOCATION)[1])
  
  # Plotting
  my_plot <- 
    DATA %>%
    filter(needs <= TOP_RANK) %>%
    # mutate(needs = fct_reorder(needs, needs)) %>%
    mutate(
      range = index_max - index_min,
      # 1. Increase multiplier (e.g. 0.45 or 0.55) to push text start position further left
      label_x  = index_min - range * 0.45,
      label_x_right  = index_max + range * 0.15,
      # 2. Add an outer anchor point so the left border of the box expands accordingly
      box_left = index_min - range * 0.02,
      box_right = index_max + range * 0.2,
      # Proportional width for the red bar
      bar_width = range * 0.01
    ) %>%
    
    # Plotting with faceting per indicator
    ggplot(aes(y = 1)) +
    # Expand left boundary of each panel box
    geom_blank(aes(x = box_left)) +
    geom_blank(aes(x = box_right)) +
    # 1. Main connecting line
    geom_segment(
      aes(x = index_min, xend = index_max, yend = 1),
      color = "#1F6388",
      linewidth = 1
    ) +
    # 2. End dots (Min & Max)
    geom_point(aes(x = index_min), color = "#1F6388", size = 2.5) +
    geom_point(aes(x = index_max), color = "#1F6388", size = 2.5) +
    # 3. Middle black dot
    geom_point(aes(x = statewide), color = "black", size = 6) +
    # 4. Red vertical marker (with black border)
    # geom_tile(aes(x = index, width = bar_width),      height = 0.35,    fill = "#C00000",    color = "black",    linewidth = 0.5) +
    # 4. Red triangular marker (with red border)
    geom_point(aes(x = index, y = 1.5), shape = 25, size = 6, fill = "#C00000", color = "#C00000", stroke = 0.5, show.legend = FALSE) +
    # 5. Left side indicator label inside the box
    geom_text(aes(x = label_x_right, y = 2.0, label = "Ranked"), size = font_size_pt / .pt, hjust = 0.5) +
    geom_text(aes(x = label_x_right, y = 1.0, label = ranking), fontface = "bold", size = (font_size_pt + 8) / .pt, hjust = 0.5, vjust = 0.8) +
    
    geom_text(aes(x = label_x, y = 2.0, label = paste("Top Needs #", needs)), size = font_size_pt / .pt, hjust = 0) +
    geom_text(aes(x = label_x, y = 1.0, label = str_wrap(indicator, width = 20)), fontface = "bold", size = (font_size_pt + 1) / .pt, hjust = 0, vjust = 0.7) +
    
    
    # 6. Value labels (below markers)
    geom_text(aes(x = index_min, label = label_min),    vjust = 2.3, fontface = "bold", size = 4) +
    geom_text(aes(x = index_max, label = label_max),    vjust = 2.3, fontface = "bold", size = 4) +
    geom_text(aes(x = statewide, label = statewide_label),    vjust = 2.3, fontface = "bold", size = 4) +
    geom_text(aes(x = index, label = label),    vjust = -2.2, fontface = "bold", size = font_size_pt / .pt) +
    
    # 7. Facet per indicator with independent x-axis scales
    facet_wrap(~ needs, ncol = 1, scales = "free_x") +
    
    # 8. Y-limits and theme styling for distinct boxed panels
    scale_y_continuous(limits = c(-0.2, 2.5)) +
    
    # This fake layer doesn't plot anything (x & y are NA), but forces the exact legend you want
    geom_point(
      data = data.frame(
        x = rep(NA_real_, length(legend_items)),
        y = rep(NA_real_, length(legend_items)),
        Legend_Item = factor(legend_items, levels = legend_items)
      ),
      aes(x = x, y = y, color = Legend_Item), size = 5, na.rm = TRUE
    ) +
    scale_color_manual(values = setNames(c("black", "#C00000"), legend_items)) +
    guides(
      color = guide_legend(
        override.aes = list(
          shape = c(16, 25),
          size = c(5, 5),
          color = c("black", "#C00000"),
          fill = c("black", "#C00000")
        )
      )
    ) +
    theme_view_line
  
  return(my_plot)
}

## > MAP plot for LEAFLET ------------------------------------------------------
plot_map_leaflet <- function(DATA, BASE_MAP, LOCATIONS,
                             OUTLINES = FALSE, LABELS = FALSE,
                             COL = "colors_red_gradient") {
  
  # set colors
  pal <- leaflet::colorNumeric(
    palette = map_colors[[COL]], 
    na.color = "whitesmoke",
    domain = na.exclude(DATA$index)
  )
  
  # Plot Iowa map
  my_leaflet_map <-
    BASE_MAP %>%
    left_join(DATA, c("fips")) %>%
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     minZoom = 6, maxZoom = 8,
                                     dragging = TRUE)) %>% 
    addTiles() %>%
    addPolygons(stroke = TRUE, 
                weight = 1,
                color = "grey",
                smoothFactor = 0.3, 
                fillOpacity = 0.975,
                fillColor = ~pal(index), 
                label = ~popup_label,
                labelOptions = labelOptions(
                  direction = "auto", 
                  textsize = "15px",
                  style = list(
                    "border-color" = "rgba(0,0,0,0.5)",
                    padding = "4px 8px"
                  )),
                highlightOptions = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.905,
                  bringToFront = FALSE)
    ) #%>% addProviderTiles(providers$CartoDB.Positron)
  
  # set default text for NAs
  my_na_label <- "Not Available"
  
  # add continuous legend
  my_leaflet_map <-  
    my_leaflet_map %>%
    addLegendNumeric(pal = pal, values = na.omit(DATA$index), orientation = 'horizontal',
                     naLabel = my_na_label,
                     numberFormat = function(x) {
                       paste0(prettyNum(x*100, format = "f", big.mark = ",", digits = 3, scientific = FALSE), "%")
                     },
                     width = 120, height = 20, position = 'bottomright')
  
  
  # exclude statewide from the list
  SELECTED_LOCS <- LOCATIONS[LOCATIONS != 19]
  
  # add names of selected locations
  if(LABELS & length(SELECTED_LOCS) > 0) {
    my_leaflet_map <- 
      my_leaflet_map %>%
      addLabelOnlyMarkers(
        data = BASE_MAP %>% filter(fips %in% SELECTED_LOCS),
        lng = ~long, lat = ~lat, label = ~name,
        labelOptions = labelOptions(noHide = TRUE, 
                                    direction = 'center',
                                    textOnly = TRUE
        ))
  } 
  
  # add outline of selected locations
  if(OUTLINES) {
    my_leaflet_map <- 
      my_leaflet_map %>%
      addPolylines(
        data = BASE_MAP %>% filter(fips %in% SELECTED_LOCS),
        stroke = TRUE,
        weight = 3,
        opacity = 0.75,
        color = "black") 
  } 
  
  # add outline of state
  my_leaflet_map <- 
    my_leaflet_map %>% 
    addPolylines(data = ia_state_map, highlightOptions = FALSE,
                 weight = 3,
                 opacity = 0.75,
                 color = "black")
  
  return(my_leaflet_map)
}


## > MAP plot for VIEW ---------------------------------------------------------
plot_map_view <- function(DATA, BASE_MAP, LOCATIONS,
                          DATA_TYPE = "percent",
                          OUTLINES = FALSE, LABELS = FALSE,
                          COL = "colors_red_gradient"){
  my_map <-
    BASE_MAP %>%
    left_join(DATA, by = "fips") %>%
    ggplot() +
    geom_sf(aes(fill = index), linewidth = 0.2) +
    geom_sf(data = ia_state_map, fill = NA, linewidth = 1.2, color = "black") +
    scale_fill_continuous(
      labels = ~ (if (DATA_TYPE == "percent") scales::percent(.x) else scales::comma(.x)), 
      breaks = c(min(DATA$index, na.rm = TRUE), max(DATA$index, na.rm = TRUE)),
      na.value = "grey90",
      name = NULL,
      palette = map_colors[[COL]]) +
    theme_view_map +
    # align legend bar and its text and remove ticks
    guides(
      fill = guide_colorbar(
        label.theme = element_text(hjust = c(1.1, -0.1), vjust = 5),
        ticks = FALSE)
    )
  
  # add names of the counties
  if(LABELS & !is.null(LOCATIONS)) {
    my_map <- 
      my_map +
      geom_text(data = BASE_MAP %>% filter(fips %in% LOCATIONS),
                aes(long, lat, label = str_wrap(name, 16)),
                lineheight = 1.1, size = 4)
  } 
  
  # outline selected counties
  if(OUTLINES & !is.null(LOCATIONS)) {
    my_map <- 
      my_map +
      geom_sf(data = BASE_MAP %>% filter(fips %in% LOCATIONS),
              fill = NA, color = "black", linewidth = 1)
  } 
  
  return(my_map)
}


## > MAP plot for DOWNLOAD -----------------------------------------------------
format_map_download <- function(fig, fig_title_data, fig_source_data) {
  my_title <- fig_title_data()
  my_source <- fig_source_data()
  url <- "https:// iadatadrive.i2d2.iastate.edu"
  
  download_fig <- 
    fig +
    labs(title = my_title$title[1],
         subtitle = " ",
         caption = sprintf(
           "**Source:** I2D2, IA Data Drive, %s<br>**Data:** %s.<br>**Year:** %s<br>**Downloaded on:** %s",
           url, my_source$source, my_source$year, my_source$date
         ),
         # tag = "Designed by Giorgi Chighladze",
         alt = "Iowa heatmap") +
    theme(
      plot.title = element_textbox_simple(size = 30, face = "bold", halign = 0.45, vjust = 0.5, lineheight = 1.5,  maxwidth = unit(9, "in")),
      plot.caption = element_markdown(size = 10, hjust = 0, margin = margin(l = 20), lineheight = 1.3),
      plot.tag.position = c(0.99, 0.19),
      plot.tag = element_text(hjust = 1, vjust = 1, size = 9, face = "bold.italic", color = "grey99"),
      plot.margin = margin(t = 15, b = 15, l = 10, r = 10, unit = "pt")
    ) +
    # align legend bar and its text and remove ticks
    guides(
      fill = guide_colorbar(
        barwidth = 20, 
        barheight = 1.2,
        label.theme = element_text(hjust = c(1.1, -0.1), vjust = 5, size = 20),
        ticks = FALSE)
    ) +
    coord_sf(clip = "off")
  
  return(download_fig)
}

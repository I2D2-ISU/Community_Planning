# SET UPS  ---------------------------------------------------------------------

## Color Palettes ----------------------------------------------------------

# Set main colors
all_8_colors <- c("#D0202E", "#001B71", "#76777A", "#0097CE", "#863399", "#D6A415", "#0B5715", "#666666")

# Set colors for heat map
map_colors <- list(
  colors_red_gradient     = rev(c("#d0202e", "#d84342", "#df5d57", "#e5736d", "#eb8984", "#ef9e9b", "#f1b3b3", "#f3c7cb")),
  colors_blue_gradient    = rev(c("#001b71", "#2d2f80", "#48458f", "#615b9e", "#7973ad", "#918bbc", "#aaa4cb", "#c2beda")),
  colors_purple_gradient  = rev(c("#863399", "#944aa4", "#a160af", "#ae75ba", "#bb8ac4", "#c89fcf", "#d5b4da", "#e1cae5"))
)

# Create named list for Color drop-down selection
map_color_choices <- 
  list(
    "Red" = "colors_red_gradient",
    "Blue" = "colors_blue_gradient",
    "Purple" = "colors_purple_gradient"
  )


## List Indicators ----------------------------------------------------------

# Set names of 5 characteristic indicators
static_5_indicators <- 
  c(
    "% of children under 6",
    "% of children under 5 -- White",
    "% of household -- English only",
    "% of household with children under 6 -- All working parents",
    "% of household with children under 6 -- Single mom"
  )

# Set names of rate indicators
ratio_indicators <- 
  c(
    "Homelessness", #"Point in Time (PIT)",
    "Child abuse or neglect", #"Child Abuse or Neglect",
    "Alcohol and Drug-Related Crime",
    "Arrest",
    "Domestic violence", # "Domestic Violence",
    "Rate of serious crime", #"Serious Crime",
    "Foster Care"
  )



# READ ADATA -------------------------------------------------------------------

## Read Metadata -------------------------------------------------------
# Read Iowa county crosswalk for all service geographies
ia_county_crosswalk <- 
  read_csv("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/data/ia_county_crosswalk.csv")

# Read indicator types
metadata.fig.types <-
  read_csv("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/data/idd_data_type.csv")

# Read indicator sources
metadata.fig.sources <-
  read_csv("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/data/idd_data_source.csv")

# Read figure titles and tooltips
metadata.fig.titles <-
  read_csv("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/data/idd_tooltip.csv")


# Create named list for Location drop-down selection
# Named list of 99 Iowa counties
ia_county_droplist <- ia_county_crosswalk %>% count(fips, county_name) %>% create_named_list()
# Named list of 34 ECI Areas
eci_area_droplist <- ia_county_crosswalk %>% count(eci_number, eci_area_name) %>% create_named_list()
# Named list of 7 HHS Regions
hhs_region_droplist <- ia_county_crosswalk %>% count(hhs_number, hhs_regions) %>% create_named_list()
# Named list of 17 Head Start Grantees
hs_grantee_droplist <- ia_county_crosswalk %>% count(hs_number, head_start_grantees) %>% create_named_list()
# Named list of 8 MIECHV Service Areas (based on 2026)
miechv_droplist <- ia_county_crosswalk %>% count(miechv_number_26, miechv_26) %>% filter(miechv_number_26 != 699) %>% create_named_list()


## Read Geo Data ----------------------------------------------------------
# Read sf data for mapping State of Iowa
ia_state_map <- read_my_rds("map_IA-state.rds")



library(tidyverse)

# Get data from CyBox
proj_dir <- getwd()
data_dir <- "C:/Users/gio/Box/Iowa IDS/Projects/_ECI/1.Dashboard Planning/Current_2026_Services Dashboard Development/2026 Community Planning Page work/R codes_Rishita/Community Planning Dashboard"
setwd(data_dir)

df_eci <- read_csv("outputs_ECI_Area/IDD-for-ranking_ECI-area_2026-10-05_ranked.csv")
df_hhs <- read_csv("outputs_HHS_Region/IDD-for-ranking_HHS-region_2026-10-05_ranked.csv")
df_rg <- read_csv("outputs_HS_Grantee/IDD-for-ranking_HS-grantee_2026-10-05_ranked.csv")
df_ia <- read_csv("outputs_IA_County/IDD-for-ranking_IA-county_2026-10-05_ranked.csv")
df_mchv <- read_csv("outputs_MIECHV/IDD-for-ranking_MIECHV-area_2026-10-05_ranked.csv")

setwd(proj_dir)


# Function to rename columns and select only those that are used by the dashboard
clean_data <- function(df) {
  df %>%
    select(1:3, indicator, index, ranking) %>% 
    rename_with(~ c('fips', 'location'), all_of(1:2)) %>% 
    arrange(indicator, fips)
}


clean_data(df_eci) %>% write_csv("data/Blob/IDD-Ranking_ECI-area.csv")
clean_data(df_hhs) %>% write_csv("data/Blob/IDD-Ranking_HHS-region.csv")
clean_data(df_rg) %>% write_csv("data/Blob/IDD-Ranking_HS-grantee.csv")
clean_data(df_ia) %>% write_csv("data/Blob/IDD-Ranking_IA-county.csv")
clean_data(df_mchv) %>% write_csv("data/Blob/IDD-Ranking_MIECHV-area.csv")

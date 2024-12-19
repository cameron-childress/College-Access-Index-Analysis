# This code builds off of a previously developed longitudinal dataset compiling a variety of variables related to Pell student enrollment, 
Pell student share, institutional finances, and other variables. Most data pulled from IPEDS NCES, rankings generated using Barron's selectivity ratings. 

# The previously developed dataset is organized in long form. Here, we reshape it to wide format to calculate percent change variables from 2011 to 2021, 
which will be used to generate rankings. The data will then be exported to an Excel file for group review, trend identification, and further analysis.


# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(writexl)
library(openxlsx)

# Set file path variable
college_index_data <- "G:/Shared drives/ET Shared Drive (Staff)/Active Projects (Staff)/College Access Index 2023/data/clean/"

# Load the prepared dataset
data <- read_csv(paste0(college_index_data, "college_index_data.csv"))

# Filter data for specific years
data_filtered <- data %>%
  filter(year %in% c(2011, 2021))

# Select and rename variables
data_filtered <- data_filtered %>%
  select(unitid, instnm, year, rating, rating_analysis, state_flagship, stabbr, region, sector, control, 
         pell_total_undergrad, pell_pct_undergrad, eftotlt2, endow_per_fte)

# Reshape data to wide format
data_wide <- data_filtered %>%
  pivot_wider(names_from = year, values_from = starts_with("pell") | starts_with("eftotlt2") | starts_with("endow"))

# Calculate percent changes, percentage point changes, and total changes
data_wide <- data_wide %>%
  mutate(
    change_ug_pell_num_2011_2021 = (pell_total_undergrad_2021 - pell_total_undergrad_2011) / pell_total_undergrad_2011,
    change_ug_enroll_2011_2021 = (eftotlt2_2021 - eftotlt2_2011) / eftotlt2_2011,
    pct_pt_change_ug_pell_2011_2021 = pell_pct_undergrad_2021 - pell_pct_undergrad_2011
  )

# Generate rankings
data_wide <- data_wide %>%
  mutate(
    pell_rank_ug_2021 = dense_rank(desc(pell_pct_undergrad_2021)),
    pell_rank_ug_rating_2021 = ave(pell_pct_undergrad_2021, rating_analysis, FUN = function(x) dense_rank(desc(x))),
    change_ug_pell_rank_2011_2021 = dense_rank(desc(change_ug_pell_num_2011_2021))
  )

# Create wealth quintiles and rank within groups
data_wide <- data_wide %>%
  mutate(
    wealth_group = ntile(endow_per_fte_2021, 5),
    pell_rank_ug_wealth_2021 = ave(pell_pct_undergrad_2021, wealth_group, FUN = function(x) dense_rank(desc(x)))
  )

# Create endowment buckets
data_wide <- data_wide %>%
  mutate(
    endow_bucket = case_when(
      endow_per_fte_2021 < 100000 ~ "Less than $100k",
      endow_per_fte_2021 < 250000 ~ "$100k-$250k",
      endow_per_fte_2021 < 500000 ~ "$250k-$500k",
      endow_per_fte_2021 < 1000000 ~ "$500k-$1 million",
      endow_per_fte_2021 >= 1000000 ~ "$1 million+",
      TRUE ~ "N/A"
    )
  )

# Rank within endowment buckets and other categories
data_wide <- data_wide %>%
  mutate(
    pell_rank_ug_endow_2021 = ave(pell_pct_undergrad_2021, endow_bucket, FUN = function(x) dense_rank(desc(x))),
    pell_rank_ug_region_2021 = ave(pell_pct_undergrad_2021, region, FUN = function(x) dense_rank(desc(x))),
    pell_rank_ug_cntrl_2021 = ave(pell_pct_undergrad_2021, control, FUN = function(x) dense_rank(desc(x)))
  ) %>%
  mutate(across(c(pell_rank_ug_endow_2021, pell_rank_ug_wealth_2021), ~ ifelse(endow_per_fte_2021 == NA, NA, .)))

# Export "analysis" sheet into Excel
write_xlsx(data_wide, paste0(college_index_data, "college_index_data_analysis_R.xlsx"))

# Weighted Pell share and race share analysis

# Merge the 'endow_bucket' variable from 'data_wide' into 'data'
merged_data <- data %>%
  left_join(data_wide %>% select(unitid, endow_bucket), by = "unitid")


# Collapse data for weighted means
weighted_means <- merged_data %>%
  group_by(endow_bucket, year) %>%
  summarise(
    count = n(),
    pell_pct_undergrad_mean = weighted.mean(pell_pct_undergrad, eftotlt2, na.rm = TRUE),
    urm_pct_undergrad_t_mean = weighted.mean(urm_pct_undergrad_t, eftotlt2, na.rm = TRUE),
    bkaa_pct_undergrad_t_mean = weighted.mean(bkaa_pct_undergrad_t, eftotlt2, na.rm = TRUE),
    hisp_pct_undergrad_t_mean = weighted.mean(hisp_pct_undergrad_t, eftotlt2, na.rm = TRUE)
  )

# Export weighted means to Excel
write_xlsx(weighted_means, paste0(college_index_data, "college_index_data_endow_bucket_averages_R.xlsx"))

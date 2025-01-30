

### Data Cleaning & Preparation

# Check if dataset exists before loading
if (!file.exists("linelist_raw.csv")) {
  stop("Error: Missing 'linelist_raw.csv'. Upload it before running the app.")
}

# Import dataset
linelist_raw <- read.csv("linelist_raw.csv", stringsAsFactors = FALSE)

# Ensure file has data before proceeding
if (ncol(linelist_raw) == 0) {
  stop("Error: 'linelist_raw.csv' is empty or missing column names.")
} else {
  linelist_raw <- clean_names(linelist_raw)
}

# Rename columns safely
if ("new_hiv_infections_children_aged_0_14_children_0_14_estimate" %in% colnames(linelist_raw)) {
  linelist_raw <- linelist_raw %>%
    rename(new_infection_children = new_hiv_infections_children_aged_0_14_children_0_14_estimate)
} else {
  stop("Error: Expected column 'new_hiv_infections_children_aged_0_14_children_0_14_estimate' not found in dataset.")
}

# Convert columns to appropriate data types
linelist_cleaned <- linelist_raw %>%
  mutate(
    year = as.numeric(year),
    code = as.character(code),
    new_infection_children = as.numeric(new_infection_children)
  )

# Map country codes to country names
linelist_cleaned <- linelist_cleaned %>%
  mutate(country = countrycode(code, "iso3c", "country.name"))

# Remove rows with missing values and filter valid years
linelist_cleaned <- linelist_cleaned %>%
  filter(!is.na(year), !is.na(code), !is.na(new_infection_children), year >= 2010, year <= 2016)

# Remove duplicates
linelist_cleaned <- linelist_cleaned %>% distinct(year, code, .keep_all = TRUE)

# Save cleaned dataset
write.csv(linelist_cleaned, "linelist_cleaned.csv", row.names = FALSE)

# Check if world map file exists
if (!file.exists("world_map.rds")) {
  stop("Error: Missing 'world_map.rds'. Upload it before running the app.")
}
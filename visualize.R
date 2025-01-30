
world <- readRDS("world_map.rds")

### Load Processed Data

# Check if cleaned dataset exists before loading
if (!file.exists("linelist_cleaned.csv")) {
  stop("Error: Missing 'linelist_cleaned.csv'. Ensure it's generated before running the app.")
}

global_data <- read.csv("linelist_cleaned.csv", stringsAsFactors = FALSE)

# Ensure file has valid column names before applying clean_names()
if (ncol(global_data) == 0) {
  stop("Error: 'linelist_cleaned.csv' is empty or missing column names.")
} else {
  global_data <- clean_names(global_data)
}
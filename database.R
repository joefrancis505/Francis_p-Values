# Set working directory to where this script is located
setwd(dirname(sys.frame(1)$ofile))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("sf", "dplyr", "purrr", "readr", "units")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Clear console
cat("\014")

# Function to read specific columns from CSV files with error handling
read_csv_columns <- function(file_path, columns) {
  tryCatch({
    col_types <- cols(
      GISJOIN = col_character(),
      .default = col_double()
    )
    
    df <- read_csv(file_path, col_types = col_types)
    
    if (!"GISJOIN" %in% names(df)) {
      stop("GISJOIN column not found in the CSV file")
    }
    
    missing_cols <- setdiff(columns, names(df))
    if (length(missing_cols) > 0) {
      stop(paste("The following columns are missing from the CSV file:", paste(missing_cols, collapse = ", ")))
    }
    
    df[, c("GISJOIN", columns)]
  }, error = function(e) {
    cat("Error in read_csv_columns function:\n")
    cat("File path:", file_path, "\n")
    cat("Requested columns:", paste(columns, collapse = ", "), "\n")
    cat("Error message:", conditionMessage(e), "\n")
    cat("Available columns in the CSV file:\n")
    tryCatch({
      available_cols <- names(read_csv(file_path, n_max = 1))
      cat(paste(available_cols, collapse = ", "), "\n")
    }, error = function(e) {
      cat("Unable to read column names from the CSV file.\n")
    })
    stop(e)
  })
}

# Function to add slavery legality dummy variable
add_slavery_legality <- function(df) {
  slave_states <- c(
    "Alabama", "Arkansas", "Delaware", "Florida", "Georgia",
    "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri",
    "North Carolina", "South Carolina", "Tennessee", "Texas", "Virginia",
    "Indian Territory", "New Mexico Territory", "Arizona Territory",
    "Arkansas Territory", "Florida Territory",
    "Orleans Territory", "Southwest Territory",
    "West Virginia", "Oklahoma", "Oklahoma Territory",
    "District of Columbia"
  )
  
  df %>%
    mutate(slavery_legal = as.integer(
      state %in% slave_states |
        (state == "New York" & year == 1790) |
        (state == "New Jersey" & (year == 1790 | year == 1800))
    ))
}

# Function to get census data for a specific year
get_census_data <- function(year) {
  tryCatch({
    census_data <- switch(as.character(year),
                          "1790" = {
                            df <- read_csv_columns("Data/Census/nhgis0129_csv/nhgis0129_ds1_1790_county.csv",
                                                   c("AAA001", "AAK001", "AAQ002"))
                            df %>%
                              rename(census_pop = AAA001) %>%
                              mutate(
                                urban_census_pop = AAK001,
                                rural_census_pop = census_pop - urban_census_pop,
                                enslaved = AAQ002
                              ) %>%
                              transmute(
                                GISJOIN,
                                census_pop,
                                urban_census_pop,
                                rural_census_pop,
                                enslaved
                              )
                          },
                          "1800" = {
                            df <- read_csv_columns("Data/Census/nhgis0129_csv/nhgis0129_ds2_1800_county.csv",
                                                   c("AAS001", "AAV001", "AAY002"))
                            df %>%
                              rename(census_pop = AAS001) %>%
                              mutate(
                                urban_census_pop = AAV001,
                                rural_census_pop = census_pop - urban_census_pop,
                                enslaved = AAY002
                              ) %>%
                              transmute(
                                GISJOIN,
                                census_pop,
                                urban_census_pop,
                                rural_census_pop,
                                enslaved
                              )
                          },
                          "1810" = {
                            df <- read_csv_columns("Data/Census/nhgis0129_csv/nhgis0129_ds3_1810_county.csv",
                                                   c("AA3001", "AA4001", "AA7002"))
                            df %>%
                              rename(census_pop = AA3001) %>%
                              mutate(
                                urban_census_pop = AA4001,
                                rural_census_pop = census_pop - urban_census_pop,
                                enslaved = AA7002
                              ) %>%
                              transmute(
                                GISJOIN,
                                census_pop,
                                urban_census_pop,
                                rural_census_pop,
                                enslaved
                              )
                          },
                          "1820" = {
                            df <- read_csv_columns("Data/Census/nhgis0129_csv/nhgis0129_ds4_1820_county.csv",
                                                   c("ABA001", "ABE001", "ABB003", "ABB004"))
                            df %>%
                              rename(census_pop = ABA001) %>%
                              mutate(
                                urban_census_pop = ABE001,
                                rural_census_pop = census_pop - urban_census_pop,
                                enslaved = ABB004 + ABB003
                              ) %>%
                              transmute(
                                GISJOIN,
                                census_pop,
                                urban_census_pop,
                                rural_census_pop,
                                enslaved
                              )
                          },
                          "1830" = {
                            df <- read_csv_columns("Data/Census/nhgis0129_csv/nhgis0129_ds5_1830_county.csv",
                                                   c("ABN001", "ABU001", "ABO003", "ABO004"))
                            df %>%
                              rename(census_pop = ABN001) %>%
                              mutate(
                                urban_census_pop = ABU001,
                                rural_census_pop = census_pop - urban_census_pop,
                                enslaved = ABO003 + ABO004
                              ) %>%
                              transmute(
                                GISJOIN,
                                census_pop,
                                urban_census_pop,
                                rural_census_pop,
                                enslaved
                              )
                          },
                          "1840" = {
                            df <- read_csv_columns("Data/Census/nhgis0129_csv/nhgis0129_ds7_1840_county.csv",
                                                   c("ACD001", "ACN001", "ACZ013", "ACZ014", "ACZ015", "ACZ016", "ACZ017", "ACZ018", "ACZ019", "ACZ020", "ACZ021", "ACZ022", "ACZ023", "ACZ024"))
                            df %>%
                              rename(census_pop = ACD001) %>%
                              mutate(
                                urban_census_pop = ACN001,
                                rural_census_pop = census_pop - urban_census_pop,
                                enslaved = ACZ013 + ACZ014 + ACZ015 + ACZ016 + ACZ017 + ACZ018 + ACZ019 + ACZ020 + ACZ021 + ACZ022 + ACZ023 + ACZ024
                              ) %>%
                              transmute(
                                GISJOIN,
                                census_pop,
                                urban_census_pop,
                                rural_census_pop,
                                enslaved
                              )
                          },
                          "1850" = {
                            pop <- read_csv_columns("Data/Census/nhgis0128_csv/nhgis0128_ds10_1850_county.csv",
                                                    c("ADQ001", "ADZ001", "AE6003"))
                            farm <- read_csv_columns("Data/Census/nhgis0128_csv/nhgis0128_ds9_1850_county.csv",
                                                     c("ADJ001", "ADI001", "ADI002"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              rename(census_pop = ADQ001) %>%
                              transmute(
                                GISJOIN,
                                census_pop,
                                urban_census_pop = ADZ001,
                                rural_census_pop = census_pop - urban_census_pop,
                                enslaved = AE6003,
                                farmv_total = ADJ001,
                                land = ADI001 + ADI002,
                                farmv = farmv_total / land,
                                improved = ADI001,
                                unimproved = ADI002,
                                pc_improved = improved / (improved + unimproved) * 100
                              )
                          },
                          "1860" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds14_1860_county.csv",
                                                    c("AG3001", "AHF001", "AH3003"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds13_1860_county.csv",
                                                     c("AGV001", "AGP001", "AGP002"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              rename(census_pop = AG3001) %>%
                              transmute(
                                GISJOIN,
                                census_pop,
                                urban_census_pop = AHF001,
                                rural_census_pop = census_pop - urban_census_pop,
                                enslaved = AH3003,
                                farmv_total = AGV001,
                                land = AGP001 + AGP002,
                                farmv = farmv_total / land,
                                improved = AGP001,
                                unimproved = AGP002,
                                pc_improved = improved / (improved + unimproved) * 100
                              )
                          },
                          stop("Invalid year")
    )
    return(census_data)
  }, error = function(e) {
    cat("Error in get_census_data function for year:", year, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    stop(e)
  })
}

# Function to calculate distances
calculate_distances_simplified <- function(counties, current_border) {
  # Ensure all geometries are in the same CRS
  stopifnot(st_crs(counties) == st_crs(current_border),
            st_is_longlat(counties) == FALSE)
  
  # Create centroids from longitude and latitude
  centroids <- st_as_sf(counties, coords = c("longitude", "latitude"), crs = st_crs(current_border)) %>%
    st_transform(st_crs(current_border))
  
  # Calculate distances from centroids to current border
  distances_current <- st_distance(centroids, current_border)
  
  # Convert distances from meters to miles
  distances_current_miles <- set_units(distances_current, "m") |> set_units("miles") |> as.numeric()
  
  # Add the distances as a new column
  counties %>%
    mutate(distance = distances_current_miles)
}

# Function to add the border dummy variable
add_border_dummy <- function(df) {
  df %>%
    mutate(border = ifelse(is.na(border) | border == 0, 0, 1))
}

# Function to process a specific year
process_year <- function(year) {
  cat("Processing year:", year, "\n")
  
  # Read county shapefile
  counties <- st_read(paste0("Data/Counties/", year, "_US_county/US_county_", year, ".shp"))
  cat("Columns after reading shapefile:", paste(names(counties), collapse = ", "), "\n")
  
  # Fix geometries for counties
  counties <- st_make_valid(counties)
  cat("County geometries fixed\n")
  
  # Keep original CRS
  original_crs <- st_crs(counties)
  
  # Rename columns from shapefiles
  counties <- counties %>%
    rename(
      state = STATENAM,
      longitude = X_CENTROID,
      latitude = Y_CENTROID,
      area = SHAPE_AREA,
      year = DECADE
    )
  
  # Add longitude_miles
  counties <- counties %>%
    mutate(longitude_miles = longitude / 1609.344)
  
  # Read current year border shapefile
  border_year <- if (year %in% c("1790", "1800", "1810")) year else "1820"
  current_border <- st_read(paste0("Data/Border/", border_year, "_border/", border_year, "_border.shp"))
  
  # Fix border geometries
  current_border <- st_make_valid(current_border)
  cat("Border geometries fixed\n")
  
  # Calculate distances to current border
  distances_current <- st_distance(st_centroid(counties), current_border)
  distances_current_miles <- set_units(distances_current, "m") |> set_units("miles") |> as.numeric()
  counties <- counties %>%
    mutate(distance = distances_current_miles)
  
  # Calculate distances to 1820 border
  border_1820 <- st_read("Data/Border/1820_border/1820_border.shp")
  border_1820 <- st_make_valid(border_1820)
  distances_1820 <- st_distance(st_centroid(counties), border_1820)
  distances_1820_miles <- set_units(distances_1820, "m") |> set_units("miles") |> as.numeric()
  counties <- counties %>%
    mutate(distance_1820 = distances_1820_miles)
  
  # Get census data for the year
  census_data <- get_census_data(year)
  
  # Join county data with census data
  counties_df <- counties %>%
    st_drop_geometry() %>%
    left_join(census_data, by = "GISJOIN")
  
  # Add slavery legality dummy variable
  counties_df <- add_slavery_legality(counties_df)
  
  # Add border dummy variable
  counties_df <- add_border_dummy(counties_df)
  
  # Adjust distances for slave states
  counties_df <- counties_df %>%
    mutate(
      distance = ifelse(slavery_legal == 1, -1 * distance, distance),
      distance_1820 = ifelse(slavery_legal == 1, -1 * distance_1820, distance_1820)
    )
  
  # Calculate population densities
  counties_df <- counties_df %>%
    mutate(
      area_sq_miles = units::set_units(area, "m^2") %>% units::set_units("mile^2") %>% as.numeric(),
      ruralpopden = rural_census_pop / area_sq_miles,
      urbanpopden = urban_census_pop / area_sq_miles
    )
  
  # Create Samples directory if it doesn't exist
  dir.create("Samples", showWarnings = FALSE)
  
  # Save each year
  write_csv(counties_df, paste0("Samples/", year, "_all.csv"))
  
  return(counties_df)
}

# List of years to process
years <- c("1790", "1800", "1810", "1820", "1830", "1840", "1850", "1860")

# Process all years
all_counties <- map_df(years, process_year)

# Write the result to a CSV file
write_csv(all_counties, "database.csv")

# Print a summary of the processed data
cat("Processing complete. Summary of the data:\n")
cat("Total number of rows:", nrow(all_counties), "\n")
cat("Columns:", paste(colnames(all_counties), collapse = ", "), "\n")
cat("Data for each year:\n")
print(table(all_counties$year))
cat("\nData has been written to 'database.csv'\n")
cat("Annual subsets have been saved in the 'Samples' folder\n")

# Fixed Database Builder
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("sf", "dplyr", "purrr", "readr", "stringr", "units")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# --- Data Specifications ---

# State classifications (restored full list from original)
slave_states <- c(
  "Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida",
  "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri",
  "North Carolina", "South Carolina", "Tennessee", "Texas", "Virginia",
  "Indian Territory", "New Mexico Territory", "Arizona Territory",
  "Arkansas Territory", "Florida Territory", "Missouri Territory",
  "Orleans Territory", "Southwest Territory",
  "West Virginia", "Oklahoma", "Oklahoma Territory"
)

# Define required columns in exact order
required_columns <- c(
  "year", "NHGISNAM", "NHGISST", "NHGISCTY", "ICPSRST", "ICPSRCTY", "ICPSRNAM",
  "state", "ICPSRSTI", "ICPSRCTYI", "ICPSRFIP", "STATE", "COUNTY", "PID",
  "longitude", "latitude", "GISJOIN", "GISJOIN2", "area", "SHAPE_LEN", "border",
  "longitude_miles", "distance", "distance_1820", "census_pop", "urban_census_pop",
  "rural_census_pop", "enslaved", "slavery_legal", "area_sq_miles", "ruralpopden",
  "urbanpopden", "farmv_total", "land", "farmv", "improved", "unimproved", "pc_improved"
)

# --- Helper Functions ---

# Function to find shapefiles
find_shapefiles <- function(year) {
  primary_patterns <- c(
    paste0("Data/Counties/", year, "_US_county/US_county_", year, ".shp"),
    paste0("Data/Counties/", year, "_US_county/us_county_", year, ".shp"),
    paste0("Data/Counties/US_county_", year, "/US_county_", year, ".shp"),
    paste0("Data/Counties/us_county_", year, "/us_county_", year, ".shp")
  )
  
  for (pattern in primary_patterns) {
    if (file.exists(pattern)) {
      return(pattern)
    }
  }
  
  if (dir.exists("Data/Counties")) {
    all_shp_files <- list.files("Data/Counties", pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
    year_files <- all_shp_files[grepl(as.character(year), all_shp_files)]
    
    if (length(year_files) > 0) {
      priority_files <- year_files[grepl(paste0("county.*", year, "|", year, ".*county"), year_files, ignore.case = TRUE)]
      if (length(priority_files) > 0) {
        return(priority_files[1])
      } else {
        return(year_files[1])
      }
    }
  }
  
  return(NULL)
}

# Function to find NHGIS files
find_nhgis_files <- function(year) {
  census_dirs <- list.dirs("Data/Census", recursive = TRUE, full.names = TRUE)
  
  found_files <- list()
  
  for (dir_path in census_dirs) {
    csv_files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
    year_files <- csv_files[grepl(year, csv_files)]
    
    if (length(year_files) > 0) {
      if (year %in% c("1850", "1860")) {
        pop_files <- year_files[grepl("ds10|ds14", year_files)]
        ag_files <- year_files[grepl("ds9|ds13", year_files)]
        
        if (length(pop_files) > 0) found_files$population <- pop_files[1]
        if (length(ag_files) > 0) found_files$agriculture <- ag_files[1]
      } else {
        found_files$population <- year_files[1]
      }
      
      if (!is.null(found_files$population)) break
    }
  }
  
  return(found_files)
}

# Function to get census data
get_census_data <- function(year) {
  files <- find_nhgis_files(year)
  
  if (is.null(files$population)) {
    stop(paste("No census file found for", year))
  }
  
  full_data <- read_csv(files$population, show_col_types = FALSE)
  
  if (year == 1790) {
    pop_data <- full_data %>%
      select(GISJOIN,
             census_pop = AAA001, urban_census_pop = AAK001) %>%
      mutate(rural_census_pop = census_pop - urban_census_pop)
    
    enslaved_data <- full_data %>% select(GISJOIN, enslaved = AAQ002)
    pop_data <- pop_data %>% left_join(enslaved_data, by = "GISJOIN")
    
  } else if (year == 1800) {
    pop_data <- full_data %>%
      select(GISJOIN,
             census_pop = AAS001, urban_census_pop = AAV001) %>%
      mutate(rural_census_pop = census_pop - urban_census_pop)
    
    enslaved_data <- full_data %>% select(GISJOIN, enslaved = AAY002)
    pop_data <- pop_data %>% left_join(enslaved_data, by = "GISJOIN")
    
  } else if (year == 1810) {
    pop_data <- full_data %>%
      select(GISJOIN,
             census_pop = AA3001, urban_census_pop = AA4001) %>%
      mutate(rural_census_pop = census_pop - urban_census_pop)
    
    enslaved_data <- full_data %>% select(GISJOIN, enslaved = AA7002)
    pop_data <- pop_data %>% left_join(enslaved_data, by = "GISJOIN")
    
  } else if (year == 1820) {
    pop_data <- full_data %>%
      select(GISJOIN,
             census_pop = ABA001, urban_census_pop = ABE001) %>%
      mutate(rural_census_pop = census_pop - urban_census_pop)
    
    enslaved_data <- full_data %>%
      select(GISJOIN, enslaved_male = ABB003, enslaved_female = ABB004) %>%
      mutate(enslaved = enslaved_male + enslaved_female) %>%
      select(GISJOIN, enslaved)
    pop_data <- pop_data %>% left_join(enslaved_data, by = "GISJOIN")
    
  } else if (year == 1830) {
    pop_data <- full_data %>%
      select(GISJOIN,
             census_pop = ABN001, urban_census_pop = ABU001) %>%
      mutate(rural_census_pop = census_pop - urban_census_pop)
    
    enslaved_data <- full_data %>%
      select(GISJOIN, enslaved_male = ABO003, enslaved_female = ABO004) %>%
      mutate(enslaved = enslaved_male + enslaved_female) %>%
      select(GISJOIN, enslaved)
    pop_data <- pop_data %>% left_join(enslaved_data, by = "GISJOIN")
    
  } else if (year == 1840) {
    pop_data <- full_data %>%
      select(GISJOIN,
             census_pop = ACD001, urban_census_pop = ACN001) %>%
      mutate(rural_census_pop = census_pop - urban_census_pop)
    
    slave_cols <- paste0("ACZ", sprintf("%03d", 13:24))
    available_slave_cols <- slave_cols[slave_cols %in% names(full_data)]
    
    if (length(available_slave_cols) > 0) {
      enslaved_data <- full_data %>%
        select(GISJOIN, all_of(available_slave_cols)) %>%
        mutate(enslaved = rowSums(select(., all_of(available_slave_cols)), na.rm = TRUE)) %>%
        select(GISJOIN, enslaved)
      pop_data <- pop_data %>% left_join(enslaved_data, by = "GISJOIN")
    }
    
  } else if (year == 1850) {
    pop_data <- full_data %>%
      select(GISJOIN,
             census_pop = ADQ001, urban_census_pop = ADZ001) %>%
      mutate(rural_census_pop = census_pop - urban_census_pop)
    
    enslaved_data <- full_data %>% select(GISJOIN, enslaved = AE6003)
    pop_data <- pop_data %>% left_join(enslaved_data, by = "GISJOIN")
    
    if (!is.null(files$agriculture)) {
      ag_data <- read_csv(files$agriculture, show_col_types = FALSE)
      farm_data <- ag_data %>%
        select(GISJOIN, farmv_total = ADJ001, improved = ADI001, unimproved = ADI002) %>%
        mutate(
          land = improved + unimproved,
          farmv = farmv_total / land,
          pc_improved = improved / land * 100
        )
      pop_data <- pop_data %>% left_join(farm_data, by = "GISJOIN")
    }
    
  } else if (year == 1860) {
    pop_data <- full_data %>%
      select(GISJOIN,
             census_pop = AG3001, urban_census_pop = AHF001) %>%
      mutate(rural_census_pop = census_pop - urban_census_pop)
    
    enslaved_data <- full_data %>% select(GISJOIN, enslaved = AH3003)
    pop_data <- pop_data %>% left_join(enslaved_data, by = "GISJOIN")
    
    if (!is.null(files$agriculture)) {
      ag_data <- read_csv(files$agriculture, show_col_types = FALSE)
      farm_data <- ag_data %>%
        select(GISJOIN, farmv_total = AGV001, improved = AGP001, unimproved = AGP002) %>%
        mutate(
          land = improved + unimproved,
          farmv = farmv_total / land,
          pc_improved = improved / land * 100
        )
      pop_data <- pop_data %>% left_join(farm_data, by = "GISJOIN")
    }
  }
  
  return(pop_data)
}

# Function to calculate distance to border using units package
calculate_distance_to_border <- function(counties_sf, year, distance_type = "year_specific") {
  if (distance_type == "distance_1820") {
    border_file <- "Data/Border/1820_border/1820_border.shp"
  } else {
    # Use historical border for pre-1820, 1820 border for 1820 onwards
    if (year >= 1820) {
      border_file <- "Data/Border/1820_border/1820_border.shp"
    } else {
      border_file <- paste0("Data/Border/", year, "_border/", year, "_border.shp")
      if (!file.exists(border_file)) {
        border_file <- "Data/Border/1820_border/1820_border.shp"
      }
    }
  }
  
  if (!file.exists(border_file)) {
    warning(paste("No border file found for", year))
    return(rep(NA_real_, nrow(counties_sf)))
  }
  
  tryCatch({
    border_sf <- read_sf(border_file)
    
    if (st_crs(counties_sf) != st_crs(border_sf)) {
      border_sf <- st_transform(border_sf, st_crs(counties_sf))
    }
    
    # Use pre-computed centroids (longitude/latitude already in projected coords)
    centroids <- st_as_sf(
      data.frame(x = counties_sf$longitude, y = counties_sf$latitude),
      coords = c("x", "y"),
      crs = st_crs(counties_sf)
    )
    
    distances <- st_distance(centroids, border_sf)
    min_distances <- apply(distances, 1, min)
    
    # Convert using units package (meters to miles)
    distance_miles <- set_units(set_units(min_distances, "m"), "miles")
    return(as.numeric(distance_miles))
  }, error = function(e) {
    warning(paste("Error calculating border distance:", e$message))
    return(rep(NA_real_, nrow(counties_sf)))
  })
}

# Function to process data for a specific year
process_year_data <- function(year) {
  cat(sprintf("\n--- Processing year: %s ---\n", year))
  
  # Find and read shapefile (base table)
  shp_file <- find_shapefiles(year)
  if (is.null(shp_file)) {
    stop(paste("No shapefile found for year", year))
  }
  
  counties_sf <- read_sf(shp_file)
  cat(sprintf("Loaded shapefile: %s (%d polygons)\n", basename(shp_file), nrow(counties_sf)))
  
  # Fix invalid geometries
  invalid_geoms <- !st_is_valid(counties_sf)
  if (any(invalid_geoms)) {
    counties_sf <- st_make_valid(counties_sf)
  }
  
  # Rename columns from shapefile (use pre-computed centroids)
  counties_sf <- counties_sf %>%
    rename(
      state = STATENAM,
      longitude = X_CENTROID,
      latitude = Y_CENTROID,
      area = SHAPE_AREA,
      year_shp = DECADE
    )
  
  # Add longitude_miles (preserving sign from projected coordinates)
  counties_sf <- counties_sf %>%
    mutate(longitude_miles = longitude / 1609.344)
  
  # Calculate distances
  distance <- calculate_distance_to_border(counties_sf, year, "year_specific")
  distance_1820 <- calculate_distance_to_border(counties_sf, year, "distance_1820")
  
  counties_sf <- counties_sf %>%
    mutate(
      distance = distance,
      distance_1820 = distance_1820
    )
  
  # Read border counties from CSV file
  border_file <- paste0("Data/Border/Borderc/borderc_", year, ".csv")
  if (file.exists(border_file)) {
    border_counties <- read_csv(border_file, show_col_types = FALSE)
    counties_sf <- counties_sf %>%
      mutate(border = as.integer(GISJOIN %in% border_counties$GISJOIN))
  } else {
    warning(paste("Border county file not found:", border_file, "- falling back to shapefile border column"))
    counties_sf <- counties_sf %>%
      mutate(border = ifelse(is.na(border) | border == 0, 0L, 1L))
  }
  
  # Drop geometry for joining
  geo_data <- counties_sf %>%
    st_drop_geometry()
  
  # Get census data
  census_data <- get_census_data(year)
  cat(sprintf("Loaded %d census records\n", nrow(census_data)))
  
  # Join: shapefile as base, census data joined on
  final_data <- geo_data %>%
    left_join(census_data, by = "GISJOIN") %>%
    mutate(
      year = !!year,
      GISJOIN2 = str_remove(GISJOIN, "^G"),
      PID = row_number(),
      
      # Area conversion
      area_sq_miles = units::set_units(units::set_units(area, "m^2"), "mile^2") %>% as.numeric(),
      
      # Population densities
      ruralpopden = pmax(rural_census_pop, 0, na.rm = TRUE) / area_sq_miles,
      urbanpopden = pmax(urban_census_pop, 0, na.rm = TRUE) / area_sq_miles,
      
      # Slavery classification
      slavery_legal = as.integer(
        state %in% slave_states |
          (state == "New York" & year == 1790) |
          (state == "New Jersey" & (year == 1790 | year == 1800))
      ),
      
      # Make distance negative for slave states
      distance = ifelse(slavery_legal == 1, -1 * abs(distance), abs(distance)),
      distance_1820 = ifelse(slavery_legal == 1, -1 * abs(distance_1820), abs(distance_1820))
    )
  
  # Add missing farm variables for years before 1850
  required_farm_cols <- c("farmv_total", "land", "farmv", "improved", "unimproved", "pc_improved")
  for (col in required_farm_cols) {
    if (!col %in% names(final_data)) {
      final_data[[col]] <- NA_real_
    }
  }
  
  # Add any missing required columns with NA
  for (col in required_columns) {
    if (!col %in% names(final_data)) {
      final_data[[col]] <- NA
    }
  }
  
  # Select required columns in exact order
  final_data <- final_data %>%
    select(all_of(required_columns))
  
  n_with_census <- sum(!is.na(final_data$census_pop))
  cat(sprintf("Final dataset: %d records (with census data: %d)\n",
              nrow(final_data), n_with_census))
  
  return(final_data)
}

# --- Main ---

main <- function() {
  cat("=== Database Creation ===\n")
  cat("Join direction: shapefile as base (all polygons kept)\n")
  cat("Centroids: pre-computed X_CENTROID/Y_CENTROID from shapefile\n")
  cat("longitude_miles: longitude / 1609.344 (signed, from projected coords)\n")
  cat("Distance conversion: via units package\n\n")
  
  years <- c(1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860)
  all_data <- list()
  
  for (year in years) {
    tryCatch({
      year_data <- process_year_data(year)
      all_data[[as.character(year)]] <- year_data
    }, error = function(e) {
      cat("ERROR processing year", year, ":", e$message, "\n")
    })
  }
  
  if (length(all_data) > 0) {
    database <- bind_rows(all_data)
    
    # Final column order check
    database <- database %>% select(all_of(required_columns))
    
    write_csv(database, "database.csv")
    
    cat("\n=== Database Complete ===\n")
    cat("File created: database.csv\n")
    cat("Total rows:", nrow(database), "\n")
    
    # Summary by year
    summary_stats <- database %>%
      group_by(year) %>%
      summarise(
        total_records = n(),
        with_census = sum(!is.na(census_pop)),
        within_300_miles = sum(abs(distance) <= 300 & !is.na(census_pop), na.rm = TRUE),
        .groups = "drop"
      )
    
    cat("\nData availability by year:\n")
    print(summary_stats)
    
    cat("\nTotal records within 300 miles with census data:\n")
    print(sum(summary_stats$within_300_miles))
    
    return(database)
  } else {
    stop("No data could be processed successfully")
  }
}

# Run
main()
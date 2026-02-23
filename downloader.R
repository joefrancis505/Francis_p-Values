# Complete NHGIS Downloader with Fixed File Organization
# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("sf", "dplyr", "purrr", "readr", "ipumsr", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# --- IPUMS API Setup ---

# Function to get API key from user or environment
get_api_key <- function() {
  api_key_env <- Sys.getenv("IPUMS_API_KEY")
  
  if (nzchar(api_key_env)) {
    cat("Using IPUMS API key found in environment variable IPUMS_API_KEY.\n")
    api_key <- api_key_env
  } else {
    ipums_url <- "https://account.ipums.org/api_keys"
    cat("This script requires an IPUMS API key, which can be obtained at", ipums_url, "\n")
    api_key <- readline(prompt = "Please enter your API key: ")
    
    # Set the key in the current session environment
    if (nzchar(api_key)) {
      Sys.setenv(IPUMS_API_KEY = api_key)
      cat("IPUMS API key has been set for this session.\n")
    }
  }
  
  if (!nzchar(api_key)) {
    stop("API key is required to download NHGIS data. Exiting.")
  }
  cat("IPUMS API key accepted for this session.\n")
  
  return(api_key)
}

# Set the default IPUMS collection
set_ipums_default_collection("nhgis")

# Get API key
api_key <- get_api_key()

# Create necessary directories
required_dirs <- c(
  "Data/Census",
  "Data/Counties"
)

# Create each directory if it doesn't exist
for (dir_path in required_dirs) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

# Function to recursively extract all zip files in a directory
extract_all_zips <- function(directory) {
  # Get all zip files in the directory (recursively)
  zip_files <- list.files(directory, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  
  if (length(zip_files) == 0) {
    return(invisible())
  }
  
  cat(sprintf("Found %d zip files to extract in %s\n", length(zip_files), directory))
  
  for (zip_file in zip_files) {
    extract_dir <- gsub("\\.zip$", "", zip_file)
    if (!dir.exists(extract_dir)) {
      cat("Extracting:", basename(zip_file), "to", extract_dir, "\n")
      dir.create(extract_dir, recursive = TRUE)
      tryCatch({
        utils::unzip(zip_file, exdir = extract_dir)
      }, error = function(e) {
        warning(sprintf("Failed to extract %s: %s", basename(zip_file), e$message))
      })
    }
  }
  
  # Check if new zip files were created during extraction
  new_zip_files <- list.files(directory, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  new_zip_files <- new_zip_files[!new_zip_files %in% zip_files]
  
  if (length(new_zip_files) > 0) {
    extract_all_zips(directory)  # Recursively extract new zip files
  }
}

# FIXED: Function to organize shapefile correctly
organize_shapefile <- function(year, output_dir) {
  cat(sprintf("\n--- Organizing county shapefile for year %s ---\n", year))
  
  counties_dir <- file.path("Data/Counties", paste0(year, "_US_county"))
  if (!dir.exists(counties_dir)) {
    dir.create(counties_dir, recursive = TRUE)
  }
  
  # Look for the specific year's shapefile in the downloads
  # Pattern: look for directories or files containing both "county" and the year
  all_shp_files <- list.files(output_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  cat("Found shapefiles in download:\n")
  for (shp in all_shp_files) {
    cat("  ", shp, "\n")
  }
  
  # Find the shapefile for this specific year
  # Look for files that have the year in the path AND county in the name
  year_shp <- all_shp_files[grepl(year, all_shp_files) & grepl("county", all_shp_files, ignore.case = TRUE)]
  
  if (length(year_shp) == 0) {
    # Fallback: look for any file with the year in the path
    year_shp <- all_shp_files[grepl(year, all_shp_files)]
  }
  
  if (length(year_shp) == 0) {
    warning(sprintf("No shapefile found for year %s", year))
    return(FALSE)
  }
  
  # If multiple files, prefer the one that looks most like a county file
  if (length(year_shp) > 1) {
    county_priority <- year_shp[grepl("county", year_shp, ignore.case = TRUE)]
    if (length(county_priority) > 0) {
      year_shp <- county_priority[1]
    } else {
      year_shp <- year_shp[1]
    }
  } else {
    year_shp <- year_shp[1]
  }
  
  cat(sprintf("Selected shapefile for %s: %s\n", year, year_shp))
  
  # Verify this is the right file by checking record count
  tryCatch({
    test_counties <- read_sf(year_shp)
    cat(sprintf("Shapefile contains %d counties\n", nrow(test_counties)))
    
    # Quick validation - 1790 should have ~290, later years should have more
    expected_min <- if (year == "1790") 250 else 350
    if (nrow(test_counties) < expected_min) {
      warning(sprintf("Shapefile for %s has unexpectedly few counties (%d). Expected at least %d.", 
                      year, nrow(test_counties), expected_min))
    }
    
  }, error = function(e) {
    warning(sprintf("Cannot read shapefile %s: %s", year_shp, e$message))
    return(FALSE)
  })
  
  # Copy all related shapefile components
  base_name <- tools::file_path_sans_ext(year_shp)
  base_dir <- dirname(year_shp)
  original_base_name <- basename(base_name)
  
  # Find all related files (.shp, .dbf, .prj, .shx, etc.)
  related_files <- list.files(
    base_dir, 
    pattern = paste0("^", gsub("([\\[\\]\\(\\)\\{\\}\\+\\*\\?\\^\\$\\|\\\\\\.])", "\\\\\\1", original_base_name), "\\.[a-zA-Z0-9]+$"), 
    full.names = TRUE
  )
  
  cat(sprintf("Found %d related files to copy:\n", length(related_files)))
  
  success_count <- 0
  for (file in related_files) {
    ext <- tools::file_ext(file)
    dest_file <- file.path(counties_dir, paste0("US_county_", year, ".", ext))
    
    if (file.copy(file, dest_file, overwrite = TRUE)) {
      cat(sprintf("  ✓ %s → %s\n", basename(file), basename(dest_file)))
      success_count <- success_count + 1
    } else {
      cat(sprintf("  ✗ Failed to copy %s\n", basename(file)))
    }
  }
  
  # Verify the copy worked
  dest_shp <- file.path(counties_dir, paste0("US_county_", year, ".shp"))
  if (file.exists(dest_shp)) {
    tryCatch({
      final_test <- read_sf(dest_shp)
      cat(sprintf("✓ SUCCESS: Final shapefile has %d counties\n", nrow(final_test)))
      
      # Show sample GISJOIN codes for verification
      if ("GISJOIN" %in% names(final_test)) {
        sample_codes <- head(unique(final_test$GISJOIN), 3)
        cat(sprintf("Sample GISJOIN codes: %s\n", paste(sample_codes, collapse = ", ")))
      }
      
      return(TRUE)
    }, error = function(e) {
      warning(sprintf("Verification failed for %s: %s", dest_shp, e$message))
      return(FALSE)
    })
  } else {
    warning(sprintf("Destination shapefile not created: %s", dest_shp))
    return(FALSE)
  }
}

# Define the census datasets and tables needed for each year
census_specs <- list(
  # 1790 specs
  "1790" = list(
    dataset = "1790_cPop",
    tables = c("NT1", "NT2", "NT6"),
    geog_levels = "county",
    shapefile = "us_county_1790_tl2000",
    output_dir = "Data/Census/1790_Population"
  ),
  # 1800 specs
  "1800" = list(
    dataset = "1800_cPop",
    tables = c("NT1", "NT3", "NT6"),
    geog_levels = "county",
    shapefile = "us_county_1800_tl2000",
    output_dir = "Data/Census/1800_Population"
  ),
  # 1810 specs
  "1810" = list(
    dataset = "1810_cPop",
    tables = c("NT2", "NT3", "NT6"),
    geog_levels = "county",
    shapefile = "us_county_1810_tl2000",
    output_dir = "Data/Census/1810_Population"
  ),
  # 1820 specs
  "1820" = list(
    dataset = "1820_cPop",
    tables = c("NT1", "NT2", "NT10"),
    geog_levels = "county",
    shapefile = "us_county_1820_tl2000",
    output_dir = "Data/Census/1820_Population"
  ),
  # 1830 specs
  "1830" = list(
    dataset = "1830_cPop",
    tables = c("NT1", "NT2", "NT12"),
    geog_levels = "county",
    shapefile = "us_county_1830_tl2000", 
    output_dir = "Data/Census/1830_Population"
  ),
  # 1840 specs
  "1840" = list(
    dataset = "1840_cPopX",
    tables = c("NT1", "NT2", "NT5"),
    geog_levels = "county",
    shapefile = "us_county_1840_tl2000",
    output_dir = "Data/Census/1840_Population"
  ),
  # 1850 specs - separate population and agriculture datasets
  "1850" = list(
    pop_dataset = "1850_cPAX",
    pop_tables = c("NT1", "NT2", "NT6"),
    ag_dataset = "1850_cAg",
    ag_tables = c("NT2", "NT3"),
    geog_levels = "county",
    shapefile = "us_county_1850_tl2000",
    pop_output_dir = "Data/Census/1850_Population",
    ag_output_dir = "Data/Census/1850_Agriculture"
  ),
  # 1860 specs - separate population and agriculture datasets
  "1860" = list(
    pop_dataset = "1860_cPAX",
    pop_tables = c("NT1", "NT2", "NT6"),
    ag_dataset = "1860_cAg",
    ag_tables = c("NT1", "NT2"),
    geog_levels = "county",
    shapefile = "us_county_1860_tl2000",
    pop_output_dir = "Data/Census/1860_Population",
    ag_output_dir = "Data/Census/1860_Agriculture"
  )
)

# Function to download data for a specific year
download_year_data <- function(year, api_key) {
  cat(sprintf("\n=== Downloading data for year: %s ===\n", year))
  
  # Get specs for this year
  year_specs <- census_specs[[year]]
  
  # Setup extract based on year
  if (year %in% c("1850", "1860")) {
    # For 1850 and 1860, we need separate population and agriculture data
    
    # Population data spec
    pop_spec <- ds_spec(
      year_specs$pop_dataset,
      data_tables = year_specs$pop_tables,
      geog_levels = year_specs$geog_levels
    )
    
    # Agriculture data spec  
    ag_spec <- ds_spec(
      year_specs$ag_dataset,
      data_tables = year_specs$ag_tables,
      geog_levels = year_specs$geog_levels
    )
    
    # Combined extract with shapefile
    extract <- define_extract_nhgis(
      description = paste("Census data for", year),
      datasets = list(pop_spec, ag_spec),
      shapefiles = year_specs$shapefile
    )
    
  } else {
    # For other years, single dataset
    data_spec <- ds_spec(
      year_specs$dataset,
      data_tables = year_specs$tables,
      geog_levels = year_specs$geog_levels
    )
    
    # Define extract with shapefile
    extract <- define_extract_nhgis(
      description = paste("Census data for", year),
      datasets = data_spec,
      shapefiles = year_specs$shapefile
    )
  }
  
  # Submit extract and wait
  cat("Submitting extract request...\n")
  submitted_extract <- submit_extract(extract, api_key = api_key)
  
  cat("Waiting for extract to complete...\n")
  ready_extract <- wait_for_extract(submitted_extract, api_key = api_key)
  
  # Create output directory
  output_dir <- "Data/Census"
  
  # Download extract
  cat("Downloading extract files...\n")
  downloaded_files <- download_extract(
    ready_extract, 
    download_dir = output_dir, 
    api_key = api_key, 
    overwrite = FALSE
  )
  
  # Recursively extract all zip files
  cat("Extracting all zip files...\n")
  extract_all_zips(output_dir)
  
  # FIXED: Organize shapefile with improved logic
  cat("Organizing county shapefile...\n")
  shapefile_success <- organize_shapefile(year, output_dir)
  
  if (shapefile_success) {
    cat(sprintf("✓ Data for %s downloaded and organized successfully.\n", year))
  } else {
    warning(sprintf("⚠ Data for %s downloaded but shapefile organization failed.\n", year))
  }
  
  return(list(
    year = year,
    files = downloaded_files,
    dir = output_dir,
    shapefile_success = shapefile_success
  ))
}

# Define years to download
years <- c("1790", "1800", "1810", "1820", "1830", "1840", "1850", "1860")

# Function to check if data for a year is already downloaded
is_year_downloaded <- function(year) {
  # Check for shapefile
  shp_path <- paste0("Data/Counties/", year, "_US_county/US_county_", year, ".shp")
  has_shapefile <- file.exists(shp_path)
  
  # Check for census CSV(s)
  census_dirs <- list.dirs("Data/Census", recursive = TRUE, full.names = TRUE)
  csv_files <- unlist(lapply(census_dirs, function(d) {
    list.files(d, pattern = paste0(".*", year, ".*\\.csv$"), full.names = TRUE, recursive = TRUE)
  }))
  has_census <- length(csv_files) > 0
  
  return(has_shapefile && has_census)
}

# Main function
main <- function() {
  cat("=== Starting IPUMS NHGIS Data Downloader ===\n")
  cat("This script will:\n")
  cat("1. Download census data for years 1790-1860\n")
  cat("2. Download county boundary shapefiles for each year\n")
  cat("3. Organize files for use with database creation script\n\n")
  
  # Download data for all years
  all_downloads <- list()
  successful_years <- c()
  failed_years <- c()
  skipped_years <- c()
  
  for (year in years) {
    # Check if data already exists
    if (is_year_downloaded(year)) {
      cat(sprintf("\n=== Year %s: Data already present, skipping download ===\n", year))
      skipped_years <- c(skipped_years, year)
      successful_years <- c(successful_years, year)
      next
    }
    
    tryCatch({
      download_result <- download_year_data(year, api_key)
      all_downloads[[year]] <- download_result
      
      if (download_result$shapefile_success) {
        successful_years <- c(successful_years, year)
      } else {
        failed_years <- c(failed_years, year)
      }
    }, error = function(e) {
      cat(sprintf("ERROR downloading %s: %s\n", year, e$message))
      failed_years <- c(failed_years, year)
    })
  }
  
  cat("\n=== Download Summary ===\n")
  if (length(skipped_years) > 0) {
    cat("Skipped (already present):", paste(skipped_years, collapse = ", "), "\n")
  }
  cat("Successful years:", paste(successful_years, collapse = ", "), "\n")
  if (length(failed_years) > 0) {
    cat("Failed years:", paste(failed_years, collapse = ", "), "\n")
  }
  
  # Final verification
  cat("\n=== Final Verification ===\n")
  for (year in years) {
    shp_path <- paste0("Data/Counties/", year, "_US_county/US_county_", year, ".shp")
    
    if (file.exists(shp_path)) {
      tryCatch({
        counties <- read_sf(shp_path)
        cat(sprintf("%s: %d counties ✓\n", year, nrow(counties)))
      }, error = function(e) {
        cat(sprintf("%s: ERROR reading shapefile\n", year))
      })
    } else {
      cat(sprintf("%s: Shapefile missing ✗\n", year))
    }
  }
  
  cat("\n=== Next Steps ===\n")
  cat("1. Verify all years downloaded successfully above\n")
  cat("2. Run your database creation script (database_v3.R)\n")
  cat("3. Expected improvements:\n")
  cat("   - 1850: ~1,200+ counties (instead of 267)\n")
  cat("   - All years: Proper historical boundaries\n")
  cat("   - Border analysis: Much larger sample sizes\n")
  
  return(all_downloads)
}

# Run main function
cat("Starting download process...\n")
cat("Note: This may take 10-30 minutes depending on IPUMS server load.\n\n")

main()
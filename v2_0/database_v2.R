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
  "Data/Census/nhgis0129_csv",
  "Data/Census/nhgis0128_csv",
  "Data/Census/nhgis0130_csv",
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

# Define years to download
years <- c("1790", "1800", "1810", "1820", "1830", "1840", "1850", "1860")

# Function to download data for a specific year
download_year_data <- function(year, api_key) {
  cat(sprintf("\n--- Downloading data for year: %s ---\n", year))
  
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
  output_dir <- if (year %in% c("1850", "1860")) {
    # Use parent directory for years with multiple output dirs
    dirname(year_specs$pop_output_dir)
  } else {
    dirname(year_specs$output_dir) 
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
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
  
  # Organize shapefile into Counties directory
  cat("Organizing county shapefile...\n")
  counties_dir <- file.path("Data/Counties", paste0(year, "_US_county"))
  if (!dir.exists(counties_dir)) {
    dir.create(counties_dir, recursive = TRUE)
  }
  
  # Find shapefile in downloaded data
  shp_files <- list.files(output_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  county_shp <- shp_files[grepl("county", shp_files, ignore.case = TRUE)]
  
  if (length(county_shp) > 0) {
    # Get all related files for the shapefile
    for (shp_file in county_shp) {
      base_name <- tools::file_path_sans_ext(shp_file)
      base_dir <- dirname(shp_file)
      
      # Get all files with the same base name
      related_files <- list.files(
        base_dir, 
        pattern = paste0("^", basename(base_name), "\\.[a-zA-Z0-9]+$"), 
        full.names = TRUE
      )
      
      # Copy to Counties directory with standardized naming
      for (file in related_files) {
        ext <- tools::file_ext(file)
        dest_file <- file.path(counties_dir, paste0("US_county_", year, ".", ext))
        if (!file.exists(dest_file)) {
          file.copy(file, dest_file)
          cat("Copied", basename(file), "to", dest_file, "\n")
        }
      }
    }
  } else {
    warning("No county shapefile found for year ", year)
  }
  
  cat(sprintf("Data for %s downloaded and processed.\n", year))
  
  return(list(
    year = year,
    files = downloaded_files,
    dir = output_dir
  ))
}

# Main function
main <- function() {
  cat("=== Starting IPUMS Data Downloader ===\n")
  
  # Download data for all years
  all_downloads <- list()
  for (year in years) {
    all_downloads[[year]] <- download_year_data(year, api_key)
  }
  
  cat("\n=== Data Processing Complete ===\n")
  cat("The following data has been downloaded:\n")
  cat("1. Census data for years:", paste(years, collapse=", "), "\n")
  cat("2. County shapefiles for years:", paste(years, collapse=", "), "\n")
  cat("\nData is ready for processing with the database.R script\n")
}

# Run main function
main()
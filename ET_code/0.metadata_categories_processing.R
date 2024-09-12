# 20240312_metadata_categories_sheets_creation
# Eric Thai
# 3/12/24

# purpose create metadata and categories within each of the GPT processed excel file

library(readxl)
library(readr)
library(writexl)
library(here)
library(fs)
library(purrr)
library(openxlsx)
library(stringr)
library(dplyr)


# 0. preliminaries REQUIRED ----
# change the input and output folder path before running
# Define the path to the folder containing CSV files
input_folder_path <- here("3.gpt_processed_schedule/20230318_USA_redo")  # Replace 'your_input_folder_name' with the name of your input folder

# Define the path to the output folder
output_folder_path <- here("4.metadata_processed_schedule/20240318_USA_redo")

# Ensure the output folder exists
dir_create(output_folder_path)


# 1. create excel sheets (with metadata and categories subsheets) -----
# List all CSV files in the input folder
csv_files <- dir_ls(path = input_folder_path, regexp = "\\.csv$")

# Function to convert CSV to Excel and add metadata and categories sheets
convert_csv_to_excel <- function(file_path) {
  # Extract the base name for the current file without extension for naming the Excel file
  file_base_name <- fs::path_ext_remove(fs::path_file(file_path))
  
  # Define the output Excel file path
  excel_file_path <- file.path(output_folder_path, paste0(file_base_name, ".xlsx"))
  
  # Read the CSV file
  data <- read_csv(file_path)
  
  # Add a new column named 'detail' to the schedule (data) dataframe
  # Assuming you want to initialize this column with NA or some default value
  data$details <- NA
  
  # Create a data frame for metadata with specified values in the first column
  metadata_values <- c("agreement", "country", "partner", "agr_year", "eif_year", "hs_version", "digits", "list_type", "original_cat")
  metadata <- data.frame(metadata_values, stringsAsFactors = FALSE)
  
  # Create a data frame for categories with specified headers
  categories_headers <- c("cat_code", "reduced", "removed", "immediate", "years_delay", "equal_steps", "backloaded", "years_pause", "path", "note")
  categories <- setNames(data.frame(matrix(ncol = length(categories_headers), nrow = 0)), categories_headers)
  
  # Combine data with metadata and categories into a list (name each element as desired sheet names)
  sheets_list <- list(metadata = metadata, schedule = data, categories = categories)
  
  # Write the list to an Excel file, each element as a separate sheet
  write_xlsx(sheets_list, path = excel_file_path)
}


# Loop through each CSV file and convert it to Excel with additional sheets
walk(csv_files, convert_csv_to_excel)


# 2. fill in easy information from file names -----

# List all Excel files in the folder
excel_files <- dir_ls(path = output_folder_path, regexp = "\\.xlsx$")

# Function to parse file name and update metadata in Excel file
update_metadata_from_filename <- function(file_path) {
  # Extract the file name without extension
  file_name <- fs::path_ext_remove(fs::path_file(file_path))
  
  # Use regex to extract agreement, country, and partner from the file name
  # The pattern assumes the format "agreement_country_partner_gpt"
  matches <- str_match(file_name, "^(\\d+)_(\\w+)_(\\w+(?:\\.\\w+)*)_gpt$")
  
  # Assign extracted values to variables
  agreement <- matches[2]
  country <- matches[3]
  # Replace the "." with a comma (or any other separator you prefer) for the partner(s)
  partners <- gsub("\\.", ", ", matches[4])
  
  # Load the existing Excel file
  wb <- loadWorkbook(file_path)
  
  # Read the "schedule" sheet
  schedule_data <- readWorkbook(file_path, sheet = "schedule")
  
  # Extract the "code" column and calculate the maximum number of digits
  code_column <- schedule_data$code
  max_digits <- max(nchar(as.character(code_column)))
  
  # Assuming "Metadata" is the second sheet and data starts from the first row
  writeData(wb, sheet = 1, x = agreement, startRow = 2, startCol = 2, colNames = FALSE)
  writeData(wb, sheet = 1, x = country, startRow = 3, startCol = 2, colNames = FALSE)
  writeData(wb, sheet = 1, x = partners, startRow = 4, startCol = 2, colNames = FALSE)
  writeData(wb, sheet = 1, x = max_digits, startRow = 8, startCol = 2, colNames = FALSE) # draw max_digits from code_column
  writeData(wb, sheet = 1, x = "full", startRow = 9, startCol = 2, colNames = FALSE) # most likely that list_type is full unless it's not when I manually check this...
  writeData(wb, sheet = 1, x = "yes", startRow = 10, startCol = 2, colNames = FALSE) # most likely that original_cat is yes if the schedule assign categories to different tariff lines
  
  
  # Save the workbook
  saveWorkbook(wb, file = file_path, overwrite = TRUE)
}

# Loop through each Excel file in the folder and update the metadata
walk(excel_files, update_metadata_from_filename)


# 3. fill in eif_year and agr_year from list of FTA (file is created separately) -----

# Read the FTA list CSV
fta_list <- read_csv("ET_file/USA_FTA_list.csv") %>%
  distinct(desta_id, agr_year, eif_year)

# Define the path to the folder with the Excel files
# output_folder_path <- here("4.metadata_processed_schedule/USA")

# List all Excel files in the folder
excel_files <- dir_ls(path = output_folder_path, regexp = "\\.xlsx$")

# Function to update metadata from file name and FTA list
update_metadata_from_fta_list <- function(file_path, fta_list) {
  # Extract the file name without extension
  file_name <- fs::path_ext_remove(fs::path_file(file_path))
  
  # Extract agreement number from the file name
  agreement_number <- str_extract(file_name, "^\\d+")
  
  # Match the agreement number with the desta_id in the FTA list
  fta_info <- fta_list %>% 
    filter(desta_id == as.numeric(agreement_number))
  
  if (nrow(fta_info) == 1) {
    # Load the existing Excel file
    wb <- loadWorkbook(file_path)
    
    # Assuming "Metadata" is the second sheet
    writeData(wb, sheet = 1, x = as.character(fta_info$agr_year), startRow = 5, startCol = 2, colNames = FALSE)
    writeData(wb, sheet = 1, x = as.character(fta_info$eif_year), startRow = 6, startCol = 2, colNames = FALSE)
    
    # Save the workbook
    saveWorkbook(wb, file = file_path, overwrite = TRUE)
  } else {
    message("No match found or multiple matches for file: ", file_name)
  }
}

# Loop through each Excel file in the folder and update the metadata
walk(excel_files, ~update_metadata_from_fta_list(.x, fta_list))

# 4. fill in HS version -----

# Load the year and HS version CSV
year_hs <- read_csv("ET_file/year_hs.csv")

# Define the path to the folder with Excel files
# output_folder_path <- here("4.metadata_processed_schedule/USA")

# List all Excel files in the folder
excel_files <- dir_ls(path = output_folder_path, regexp = "\\.xlsx$")

# Function to update HS version in Excel file based on agr_year
update_hs_version <- function(file_path, year_hs) {
  # Load the existing Excel file
  wb <- loadWorkbook(file_path)
  
  # Read the "Metadata" sheet, assuming it's the second sheet
  metadata <- readWorkbook(file_path, sheet = 1)
  
  # Assuming 'agr_year' is in a specific row, 4th row in the 2nd column
  agr_year <- as.numeric(metadata[4, 2])
  
  # Match the agr_year with the year in the CSV to find the corresponding HS version
  hs_version <- filter(year_hs, year == agr_year)$HS
  
  if (length(hs_version) == 1) {
    # Write the HS version to the "Metadata" sheet, assuming it goes to row 7 in the second column
    writeData(wb, sheet = 1, x = hs_version, startRow = 7, startCol = 2, colNames = FALSE)
    
    # Save the workbook
    saveWorkbook(wb, file = file_path, overwrite = TRUE)
  } else {
    message("No match found or multiple matches for HS version in file: ", file_path)
  }
}

# Loop through each Excel file in the folder and update the HS version
walk(excel_files, ~update_hs_version(.x, year_hs))

# 5. fill in unique cat_code from schedule and take out first row from metadata ----

# Define the path to the folder with Excel files
# output_folder_path <- here("4.metadata_processed_schedule/USA")

# List all Excel files in the folder
excel_files <- dir_ls(path = output_folder_path, regexp = "\\.xlsx$")

# Function to update the "Categories" sheet with unique codes from the "schedule" sheet
update_categories_with_unique_codes <- function(file_path) {
  # Load the existing Excel file
  wb <- loadWorkbook(file_path)
  
  # Assuming "schedule" is the name of the second sheet
  # Read the "schedule" sheet to get unique codes
  schedule_data <- readWorkbook(file_path, sheet = 2)  # If 'sheet = "schedule"' does not work, try index
  unique_codes <- distinct(schedule_data, category) %>% pull(category)
  
  # Sort unique codes alphabetically
  unique_codes <- sort(unique_codes)
  
  # Prepare the unique codes for writing; no column name in vector mode
  # Convert to data frame to control the column name in the Excel sheet
  unique_codes_df <- data.frame(cat_code = unique_codes)
  
  # Assuming the "Categories" sheet is the third sheet and already exists
  # Write the unique codes to the "Categories" sheet, overwriting existing content starting from the first row
  writeData(wb, sheet = 3, x = unique_codes_df, startRow = 1, startCol = 1, colNames = TRUE)
  
  # Add filters to the "Schedules" and "Categories" sheets
  addFilter(wb, sheet = 3, cols = 1:10, rows = 1)
  addFilter(wb, sheet = 2, cols = 1:4, rows = 1)
  
  # Save the workbook
  saveWorkbook(wb, file = file_path, overwrite = TRUE)
}

# Loop through each Excel file in the folder and update the "Categories" sheet
walk(excel_files, update_categories_with_unique_codes)

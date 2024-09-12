# 20240324_code_validation
# Eric Thai
# 3/24/24

# purpose: using ET manual collection of data/manual coding of categories and compare it against EVL's cleaned data
# for context: ET and EVL noticed discrepancy in the final data and the tariff schedule (raw pdf).

require(dplyr)
require(countrycode)
require(tidyverse)

# 1. load data -----
et_data <- readRDS(here("ET_data", "allschedules.rds"))

ET_data <- et_data %>%
  rename(reporter_iso3c = country, 
         partner_iso3c = partner) %>%
  select(-code, -hs_original, -HS92, -HS96, -HS07, -HS12, - HS17)

load("~/Library/CloudStorage/Dropbox/1. Research/3. Data/Lieshout PTA Tariff Data/schedules_toshare.rdata")

EVL_data <- dt %>%
  rename(reporter_iso3c = cou, 
         partner_iso3c = par) %>%
  filter(reporter_iso3c == "USA" | partner_iso3c == "USA") %>%
  select(-code, -hs_original, -HS92, -HS96, -HS07, -HS12, - HS17)

comparison_df <- EVL_data %>%
  full_join(ET_data, by=c("reporter_iso3c", "partner_iso3c",  "agr_year", "eif_year", "HS02"))

# 2. check for any missing data on my end (.y)
comparison_df %>%
  count(is.na(reduced_optimist.y), reporter_iso3c, partner_iso3c, agr_year)

# KOR_USA missingness corresponds to some extraneous product codes from EVL's version, probably a result from OCR/RA not checking them. Inconsequential stuff.

# 3. calculating correlation coefficients for all variables, comparing EVL's version (ending with .x) and ET's version (ending with .y) ------
# correlation coefficients
calculate_correlations <- function(comparison_df) {
  # Identify all unique variable base names (strip out the .x and .y suffixes)
  variable_bases <- unique(gsub("\\..$", "", names(comparison_df)))
  
  # Initialize an empty vector to store correlation results
  correlations <- numeric(length(variable_bases))
  
  # Loop through each base variable name
  for (i in seq_along(variable_bases)) {
    var_base <- variable_bases[i]
    
    # Construct the full variable names for the .x and .y comparison_df
    var_x <- paste0(var_base, ".x")
    var_y <- paste0(var_base, ".y")
    
    # Check if both columns exist in the comparison_df frame
    if (var_x %in% names(comparison_df) && var_y %in% names(comparison_df)) {
      # Calculate the Pearson correlation coefficient between the .x and .y variables
      correlations[i] <- cor(comparison_df[[var_x]], comparison_df[[var_y]], use = "complete.obs")
    } else {
      # If either column is missing, set the correlation as NA
      correlations[i] <- NA
    }
    
    # Print the variable name and its correlation
    cat(var_base, "correlation:", correlations[i], "\n")
  }
  
  # Return the correlations vector in case it needs to be used later
  return(correlations)
}


correlation_results <- calculate_correlations(comparison_df)

summary(comparison_df)


# double checking the function by manually taking the correlation on reduced_optimist.x (EVL version) and reduced_optimist.y (ET version)
comparison_df %>%
  select(years_delay_optimist.x, years_delay_optimist.y) %>%
  na.omit() %>% # ideally should omit observations where I've fill in categories for TRQ
  cor()

comparison_df %>%
  filter(reporter_iso3c == "USA" & partner_iso3c == "AUS") %>%
  # select(years_delay_optimist.x, years_delay_optimist.y) %>%
  # na.omit() %>%
  ggplot(aes(x = years_delay_pessimist.x, y = years_delay_pessimist.y)) +
  geom_point()


# 4. compare and make column indicating whether the coding matches ----

# Function to compare .x and .y variables and add a column indicating correctness
compare_variables <- function(df) {
  # Find all unique variable base names (without .x and .y)
  variable_bases <- unique(sub("\\.x$|\\.y$", "", names(df)))
  
  # Loop through each base name to compare .x and .y variables
  for (base in variable_bases) {
    x_var <- paste0(base, ".x")
    y_var <- paste0(base, ".y")
    
    # Check if both .x and .y columns exist for this base variable
    if (x_var %in% names(df) && y_var %in% names(df)) {
      # Create a new column to indicate if .x is equal to .y
      correct_col_name <- paste0(base, "_correct")
      df <- df %>% mutate(!!correct_col_name := if_else(get(x_var) == get(y_var), 1, 0))
    }
  }
  
  return(df)
}

comparison_df_correct <- compare_variables(comparison_df)

# proportion that is correct for each dyad - in most case, it is almost 1 (I coded TRQ stuff as well which may account for some deviation) for the most case
# the only outlier is USA_AUS, as previously discussed in the meeting. 
comparison_df_correct_prop <- comparison_df_correct %>%
  select(reporter_iso3c, partner_iso3c, ends_with("correct")) %>%
  group_by(reporter_iso3c, partner_iso3c) %>%
  summarise_all(mean, na.rm=T)
  



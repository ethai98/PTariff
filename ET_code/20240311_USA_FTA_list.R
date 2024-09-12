# 20240311_USA_FTA_list
# Eric Thai
# 3/11/24

# purpose: identify all US FTA that I need to (re)clean because of inconsistencies in EVL's data...


library(tidyverse)
library(here)
library(concordance)
library(haven)
library(fixest)
library(tidylog)
library(data.table)
library(tidylog)
library(countrycode)
library(stringr)



# custom match for names:
custom_match_iso3n_country.name <- c("900" = "Kosovo", 
                                     "729" = "Sudan", 
                                     "530" = "Netherlands Antilles")


# there's 852 PTAs in DESTA so far
# after factoring in EVL's sample, 782 PTAs left. 
desta_content <- read_csv(here("../../3. Data/DESTA/DESTA content Jan 2022", "desta_content_02_01.csv")) %>%
  select(base_treaty, mar_typedepth) %>%
  rename(desta_id = base_treaty)

desta_index <- read_csv(here("../../3. Data/DESTA/DESTA content Jan 2022", "desta_indices_02_01.csv")) %>%
  select(base_treaty, enforce, enforce01, flexescape, flexrigid, depth_rasch, depth_index) %>%
  rowwise() %>%
  mutate(depth_enf_flex_score = sum(enforce, flexescape, depth_index)) %>%
  ungroup() %>%
  rename(desta_id = base_treaty)

income_classification <- read_csv(here("../../3. Data/World Bank", "income_classification.csv")) %>%
  reshape2::melt(id.vars=c("iso3c", "country")) %>%
  rename(year = variable,
         income = value) %>%
  mutate(n_s = case_when(income == "H"  ~ "North",
                         income == "L" | income == "LM" | income == "UM" ~ "South",
                         TRUE ~ NA),
         year = as.numeric(as.character(year))) 


desta_list <- read_csv(here("../../3. Data/DESTA/DESTA content Jan 2022", "desta_list_of_treaties_02_01.csv"))


USA_FTA_list <- desta_list %>%
  subset(entry_type == "base_treaty") %>%
  select(base_treaty, year, entryforceyear, name, typememb, regioncon, c1:c91) %>%
  reshape2::melt(id.vars=c("base_treaty", "year", "entryforceyear", "name", "typememb", "regioncon")) %>%
  select(-variable) %>%
  drop_na(value) %>%
  rename(desta_id = base_treaty,
         eif_year = entryforceyear,
         agr_year = year,
         iso3n = value) %>%
  left_join(desta_content) %>%
  mutate(iso3c = countrycode(iso3n, origin="iso3n", destination="iso3c"),
         country.name = countrycode(iso3n, origin="iso3n", destination="country.name", custom_match = custom_match_iso3n_country.name),
         bilateral = ifelse(typememb == 1, "bilateral", "not bilateral"),
         fta = ifelse(mar_typedepth == 2, "Free Trade Agreement", "Other"),
         post2000 = ifelse(agr_year >= 2000, "Post-2000", "Pre-2000"),
         eif = ifelse(!is.na(eif_year), "Entered into force", "Not entered into force")) %>%
  # merge in income_classification
  left_join(income_classification, by=c("iso3c" = "iso3c",
                                        "agr_year" = "year")) %>%
  group_by(desta_id, agr_year, eif_year, name, bilateral, regioncon, fta, post2000, eif) %>%
  summarise(iso3c = paste(iso3c, collapse=", "),
            country.name = paste(country.name, collapse=", "),
            n_s = paste(n_s, collapse=" - ")) %>%
  filter(str_detect(iso3c, "USA")) %>%
  left_join(desta_index) %>%
  mutate(Schedule_Exist = NA,
         Downloaded = NA,
         GPT_Cleaned = NA)

write_csv(USA_FTA_list, here("ET_file", "USA_FTA_list.csv"))

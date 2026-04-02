# Complete FTA list
# Eric Thai
# 12/26/24

# purpose: create a list of all FTA, code which was coded by EVL and 
# expand to be reporter_partner_fta

library(tidyverse)
library(here)
library(concordance)
library(haven)
library(fixest)
library(tidylog)
library(data.table)
library(tidylog)
library(countrycode)

# import datasets
## import DESTA ----

desta_list <- read_csv(here("../DESTA/DESTA December 2023", "desta_list_of_treaties_02_02.csv"))

desta_list2 <- desta_list %>%
  subset(entry_type == "base_treaty") %>%
  select(name, base_treaty, year, entryforceyear) %>%
  rename(desta_id = base_treaty,
         eif_year = entryforceyear,
         agr_year = year) 

desta_list_tomerge_EVL <- desta_list %>%
  subset(entry_type == "base_treaty") %>%
  select(name, base_treaty) %>%
  rename(desta_id = base_treaty) 

## import Van Lieshout 2021 -----
load(here("../Lieshout PTA Tariff Data", "schedules_toshare.rdata"))

### load concordance between pair in Van Lieshout (2021) and desta_id
temp_agrs <- read_csv(here("../Lieshout PTA Tariff Data", "temp_agrs.csv")) %>%
  separate(agr, into = c("reporter_iso3c", "partner_iso3c"), sep = c("_"))

# make the reverse in dyad pair
temp_agrs2 <- temp_agrs %>%
  rename(reporter_iso3c = partner_iso3c,
         partner_iso3c = reporter_iso3c) 

# bind with reverse dyad pair
temp_agrs_final <- temp_agrs %>%
  bind_rows(temp_agrs2) 

dt %>%
  filter(is.na(agr_year)) %>%
  distinct(cou, par)

dt %>%
  filter(is.na(eif_year)) %>%
  distinct(cou, par)


rep_par_desta_EVL <- dt %>%
  rename(reporter_iso3c = cou, 
         partner_iso3c = par) %>%
  # fill in missing agr_year
  mutate(agr_year = case_when(reporter_iso3c == "PER" & partner_iso3c == "MEX" & is.na(agr_year) ~ 2011,
                              TRUE ~ agr_year)) %>%
  # fill in missing eif_year
  mutate(eif_year = case_when(reporter_iso3c == "CAN" & partner_iso3c == "EU"  & agr_year == 2016 ~ 2017, # source: https://www.international.gc.ca/trade-commerce/trade-agreements-accords-commerciaux/agr-acc/ceta-aecg/view_timeline-consultez_chronologie.aspx?lang=eng
                              reporter_iso3c == "ECU" & partner_iso3c == "EU"  & agr_year == 2014 ~ 2017, # source: https://trade.ec.europa.eu/access-to-markets/en/content/eu-colombia-peru-ecuador-trade-agreement#:~:text=The%20EU%2DColombia%2DEcuador%2D,measures%2C%20such%20as%20customs%20procedures.
                              reporter_iso3c == "EU"  & partner_iso3c == "ECU" & agr_year == 2014 ~ 2017, # source: https://trade.ec.europa.eu/access-to-markets/en/content/eu-colombia-peru-ecuador-trade-agreement#:~:text=The%20EU%2DColombia%2DEcuador%2D,measures%2C%20such%20as%20customs%20procedures.
                              reporter_iso3c == "SGP" & partner_iso3c == "EU"  & agr_year == 2016 ~ 2019, # source: https://policy.trade.ec.europa.eu/eu-trade-relationships-country-and-region/countries-and-regions/singapore/eu-singapore-agreement_en
                              reporter_iso3c == "EU"  & partner_iso3c == "VNM" & agr_year == 2016 ~ 2020, # source: https://en.wikipedia.org/wiki/European_Union%E2%80%93Vietnam_Free_Trade_Agreement
                              reporter_iso3c == "PER" & partner_iso3c == "MEX" & agr_year == 2011 ~ 2012, # source: http://www.sice.oas.org/tpd/mex_per/MEX_PER_e.asp#Entry
                              reporter_iso3c == "AUS" & partner_iso3c == "CHN" & agr_year == 2015 ~ 2015, # source: https://www.dfat.gov.au/trade/agreements/in-force/chafta/Pages/australia-china-fta
                              reporter_iso3c == "CHN" & partner_iso3c == "AUS" & agr_year == 2015 ~ 2015, # source: https://www.dfat.gov.au/trade/agreements/in-force/chafta/Pages/australia-china-fta
                              reporter_iso3c == "CAN" & partner_iso3c == "UKR" & agr_year == 2016 ~ 2017, # source: https://en.wikipedia.org/wiki/Canada%E2%80%93Ukraine_Free_Trade_Agreement
                              reporter_iso3c == "UKR" & partner_iso3c == "CAN" & agr_year == 2016 ~ 2017, # source: https://en.wikipedia.org/wiki/Canada%E2%80%93Ukraine_Free_Trade_Agreement
                              reporter_iso3c == "CHL" & partner_iso3c == "THA" & agr_year == 2013 ~ 2015, # source: http://www.sice.oas.org/Trade/CHL_THA_Final/CHL_THA_Index_PDF_e.asp
                              reporter_iso3c == "GTM" & partner_iso3c == "PER" & agr_year == 2011 ~ 2013, # source: http://www.sice.oas.org/tpd/cacm_per/GTM_PER_e.asp
                              reporter_iso3c == "PER" & partner_iso3c == "GTM" & agr_year == 2011 ~ 2013, # source: http://www.sice.oas.org/tpd/cacm_per/GTM_PER_e.asp
                              reporter_iso3c == "HND" & partner_iso3c == "PER" & agr_year == 2015 ~ 2017, # source: http://www.sice.oas.org/tpd/cacm_per/HND_PER_e.asp
                              reporter_iso3c == "PER" & partner_iso3c == "HND" & agr_year == 2015 ~ 2017, # source: http://www.sice.oas.org/tpd/cacm_per/HND_PER_e.asp
                              reporter_iso3c == "JPN" & partner_iso3c == "MNG" & agr_year == 2015 ~ 2016, # source: https://www.mofa.go.jp/policy/economy/fta/mongolia.html
                              reporter_iso3c == "MNG" & partner_iso3c == "JPN" & agr_year == 2015 ~ 2016, # source: https://www.mofa.go.jp/policy/economy/fta/mongolia.html
                              reporter_iso3c == "KOR" & partner_iso3c == "NZL" & agr_year == 2015 ~ 2015, # source: https://investmentpolicy.unctad.org/international-investment-agreements/treaties/treaties-with-investment-provisions/3629/korea-republic-of---new-zealand-fta-2015-
                              reporter_iso3c == "NZL" & partner_iso3c == "KOR" & agr_year == 2015 ~ 2015, # source: https://investmentpolicy.unctad.org/international-investment-agreements/treaties/treaties-with-investment-provisions/3629/korea-republic-of---new-zealand-fta-2015-
                              reporter_iso3c == "MEX" & partner_iso3c == "PAN" & agr_year == 2014 ~ 2015, # source: http://www.sice.oas.org/tpd/mex_pan/MEX_PAN_e.ASP
                              reporter_iso3c == "PAN" & partner_iso3c == "MEX" & agr_year == 2014 ~ 2015, # source: http://www.sice.oas.org/tpd/mex_pan/MEX_PAN_e.ASP
                              reporter_iso3c == "COL" & partner_iso3c == "PAN" & agr_year == 2013 ~ NA, # agreement never entered into force, set eif_year as 10000 as a comparison case
                              reporter_iso3c == "PAN" & partner_iso3c == "COL" & agr_year == 2013 ~ NA, # agreement never entered into force, set eif_year as 10000 as a comparison case
                              TRUE ~ eif_year)) %>%
  distinct(reporter_iso3c, partner_iso3c, agr_year, eif_year) %>%
  mutate(coded = "yes",
         source = "EVL") %>%
  # full_join to display the non-coded schedule direction. 
  full_join(temp_agrs_final) %>%
  mutate(coded = ifelse(is.na(coded), "no", "yes")) %>%
  group_by(desta_id) %>%
  tidyr::fill(agr_year, eif_year, .direction = "down")

EVL_coded_list <- rep_par_desta_EVL %>%
  distinct(desta_id) %>%
  pull(desta_id)


rep_par_desta_EVL %>%
  filter(is.na(eif_year)) %>%
  distinct(reporter_iso3c, partner_iso3c)
# 240_COL-PAN never entered into force. 
  
# code the rest of desta

# custom match for names:
custom_match_iso3n_country.name <- c("900" = "Kosovo", 
                                     "729" = "Sudan", 
                                     "530" = "Netherlands Antilles")


# there's 852 PTAs in DESTA so far
# after factoring in EVL's sample, 782 PTAs left. 
desta_content <- read_csv(here("../DESTA/DESTA December 2023", "desta_version_02_02.csv"))  %>%
  select(base_treaty, mar_typedepth) %>%
  rename(desta_id = base_treaty)

desta_index <- read_csv(here("../DESTA/DESTA December 2023", "desta_indices_version_02_02.csv")) %>%
  select(base_treaty, enforce, enforce01, flexescape, flexrigid, depth_rasch, depth_index) %>%
  rowwise() %>%
  mutate(depth_enf_flex_score = sum(enforce, flexescape, depth_index)) %>%
  ungroup() %>%
  rename(desta_id = base_treaty)

# income_classification <- read_csv(here("../World Bank", "income_classification.csv")) %>%
#   reshape2::melt(id.vars=c("iso3c", "country")) %>%
#   rename(year = variable,
#          income = value) %>%
#   mutate(n_s = case_when(income == "H"  ~ "North",
#                          income == "L" | income == "LM" | income == "UM" ~ "South",
#                          TRUE ~ NA),
#          year = as.numeric(as.character(year))) 

complete_pta_to_code_list <- desta_list %>%
  # subset to base_treaty observations
  subset(entry_type == "base_treaty") %>%
  # subset important variables
  select(base_treaty, year, entryforceyear, name, typememb, regioncon, c1:c91) %>%
  # change so we have one member per row
  reshape2::melt(id.vars=c("base_treaty", "year", "entryforceyear", "name", "typememb", "regioncon")) %>%
  # take out variable since it's just c1:c91
  select(-variable) %>%
  # take out missing value where there's gaps. 
  drop_na(value) %>%
  # rename variables
  rename(desta_id = base_treaty,
         eif_year = entryforceyear,
         agr_year = year,
         iso3n = value) %>%
  left_join(desta_content) %>%
  mutate(iso3c = countrycode(iso3n, origin="iso3n", destination="iso3c"),
         country.name = countrycode(iso3n, origin="iso3n", destination="country.name", custom_match = custom_match_iso3n_country.name),
         bilateral = case_when(typememb == 1 ~ "bilateral", 
                               typememb == 2 ~ "plurilateral", 
                               typememb == 3 ~ "Plurilateral & third country", 
                               typememb == 4 ~ "Region-region", 
                               typememb == 5 ~ "Accession", 
                               typememb == 6 ~ "Accession to an agreement as a result of membership in a regional agreement", 
                               typememb == 7 ~ "Withdrawal", ),
         # market access 2 = (full) free trade agreement
         fta = case_when(mar_typedepth == 1 ~ "Partial Scope Agreement",
                          mar_typedepth == 2 ~ "Free Trade Agreement", 
                          mar_typedepth == 3 ~ "Customs Union", 
                          mar_typedepth == 4 ~ "Services Agreement", 
                          mar_typedepth == 5 ~ "Framework Agreement; no specific provisions"),
         post2000 = ifelse(agr_year >= 2000, "Post-2000", "Pre-2000"),
         eif = ifelse(!is.na(eif_year), "Entered into force", "Not entered into force")) %>%
  # merge in income_classification
  # left_join(income_classification, by=c("iso3c" = "iso3c",
  #                                       "agr_year" = "year")) %>%
  group_by(desta_id, agr_year, eif_year, name, bilateral, regioncon, fta, post2000, eif) %>%
  summarise(iso3c = paste(iso3c, collapse=", "),
            country.name = paste(country.name, collapse=", ")) %>%
  # code EVL's agreements
  mutate(team = ifelse(desta_id %in% EVL_coded_list, "EVL", NA),
         coded = ifelse(team == "EVL", TRUE, NA),
         RA = NA) %>%
  select(coded, team, RA, desta_id, agr_year, eif_year, name, country.name, iso3c, everything())
  
write_csv(complete_pta_to_code_list, here("ET_file", "ALL_FTA_LIST.csv"), na="")





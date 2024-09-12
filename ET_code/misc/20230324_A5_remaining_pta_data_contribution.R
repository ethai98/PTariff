# A3.TradePolicy/remaining_pta_data_contribution
# Eric Thai
# created: 3/24/23
# updated: 10/4/23

# purpose: empirically track PTAs that have been done by Baccini et al. (2018) and Van Lieshout (2021) 
# so that I know which PTA to collect data from. 

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

desta_list <- read_csv(here("../../3. Data/DESTA/DESTA content Jan 2022", "desta_list_of_treaties_02_01.csv"))

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

## import Baccini et al. 2018 (not using) ----
# 
# baccini_pta <- read_dta(here("../../3. Data/Baccini, Dur, Elsig 2018/Replication_ISQ", "Dataset_main.dta"))
# 
# ### harmonize baccini_pta with entryforceyear from DESTA -----
# baccini_pta2 <- baccini_pta %>%
#   mutate(years_delay = exp(TimeToZero)) %>% # use exp() to create the liberalization year count. 
#   rename(tariff_cut_pct = TariffCut) %>% # rename TariffCut as tariff_cut_pct
#   mutate(reporter_iso3c = countrycode::countrycode(country_a, origin="country.name", destination="iso3c", warn = T),
#          partner_iso3c = countrycode::countrycode(country_b, origin="country.name", destination="iso3c", warn = T),
#          reporter_iso3c = ifelse(is.na(reporter_iso3c), country_a, reporter_iso3c),
#          partner_iso3c = ifelse(is.na(partner_iso3c), country_b, partner_iso3c)) %>%
#   rename(baccini_year = year) %>% 
#   left_join(desta_list2) %>%
#   mutate(pair = paste0(reporter_iso3c, " - ", partner_iso3c)) %>%
#   select(reporter_iso3c, partner_iso3c, pair, desta_id, eif_year) %>%
#   mutate(data_source = "Baccini")


## import Van Lieshout 2021 -----
load(here("../../3. Data/Lieshout PTA Tariff Data", "schedules_toshare.rdata"))

### load concordance between pair in Van Lieshout (2021) and desta_id
temp_agrs <- read_csv(here("../../3. Data/Lieshout PTA Tariff Data", "temp_agrs.csv")) %>%
  separate(agr, into = c("reporter_iso3c", "partner_iso3c"), sep = c("_"))

temp_agrs2 <- temp_agrs %>%
  rename(reporter_iso3c = partner_iso3c,
         partner_iso3c = reporter_iso3c) 

temp_agrs_final <- temp_agrs %>%
  bind_rows(temp_agrs2) %>%
  left_join(desta_list_tomerge_EVL)

lieshout_pta <-dt %>%
  rename(reporter_iso3c = cou, 
         partner_iso3c = par) %>%
  # fill in missing agr_year
  mutate(agr_year = case_when(reporter_iso3c == "PER" & partner_iso3c == "MEX" & is.na(agr_year) ~ 2011,
                              TRUE ~ agr_year)) %>%
  # fill in missing eif_year
  mutate(eif_year = case_when(reporter_iso3c == "CAN" & partner_iso3c == "EU" & agr_year == 2016 ~ 2017, # source: https://www.international.gc.ca/trade-commerce/trade-agreements-accords-commerciaux/agr-acc/ceta-aecg/view_timeline-consultez_chronologie.aspx?lang=eng
                              reporter_iso3c == "ECU" & partner_iso3c == "EU" & agr_year == 2014 ~ 2017, # source: https://trade.ec.europa.eu/access-to-markets/en/content/eu-colombia-peru-ecuador-trade-agreement#:~:text=The%20EU%2DColombia%2DEcuador%2D,measures%2C%20such%20as%20customs%20procedures.
                              reporter_iso3c == "EU" & partner_iso3c == "ECU" & agr_year == 2014 ~ 2017, # source: https://trade.ec.europa.eu/access-to-markets/en/content/eu-colombia-peru-ecuador-trade-agreement#:~:text=The%20EU%2DColombia%2DEcuador%2D,measures%2C%20such%20as%20customs%20procedures.
                              reporter_iso3c == "SGP" & partner_iso3c == "EU" & agr_year == 2016 ~ 2019, # source: https://policy.trade.ec.europa.eu/eu-trade-relationships-country-and-region/countries-and-regions/singapore/eu-singapore-agreement_en
                              reporter_iso3c == "EU" & partner_iso3c == "VNM" & agr_year == 2016 ~ 2020, # source: https://en.wikipedia.org/wiki/European_Union%E2%80%93Vietnam_Free_Trade_Agreement
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
  mutate(pair = paste0(reporter_iso3c, " - ", partner_iso3c))%>%
  # full_join will provide observations where agr_year and eif_year are NAs on the flipped pair. e.g., if we have tariff data for EU-VNM, but no data for VNM-EU, there will be one observation of the latter but eif_year and agr_year are NA. 
  left_join(temp_agrs_final) %>%
  select(reporter_iso3c, partner_iso3c, pair, HS02, agr_year, eif_year,  name, desta_id)

table(is.na(lieshout_pta$desta_id))
# check to make sure that all desta_id was merged in correctly. 

# examine missing reporter_partner tariff schedule (supposedly there is 24 (as they are rows ONLY in y in the full_join))
# NOTE: these are the list of missing tariff schedule from Van Lieshout (2021), based on the PTA 

lieshout_pta_missing_schedule <- lieshout_pta %>%
  full_join(temp_agrs_final) %>%
  # filter so that I get observations that doesn't have product-level data yet (pair was created in creating lieshout_pta)
  filter(is.na(pair)) %>%
  mutate(pair = paste0(reporter_iso3c, " - ", partner_iso3c))%>%
  distinct(pair, desta_id, name) 
  
write_csv(lieshout_pta_missing_schedule, here("A5.DataContribution/remaining_pta_list", "lieshout_pta_missing_schedule.csv"))

# examine the universe of tariff sample in PTAs that are already coded...

# identify EU-KOR ----
lieshout_pta %>%
  subset(reporter_iso3c == "EU") %>%
  distinct(pair)


# Lieshout only (70) -----
available_tariff_data_desta_id_lieshout <- lieshout_pta %>%
  mutate(desta_id_data = paste0(desta_id, "_", data_source)) %>%
  distinct(desta_id)

nrow(available_tariff_data_desta_id_lieshout)

# Baccini only (61) -----

available_tariff_data_desta_id_baccini <- baccini_pta2 %>%
  mutate(desta_id_data = paste0(desta_id, "_", data_source)) %>%
  distinct(desta_id) 

nrow(available_tariff_data_desta_id_baccini)

# total PTA coded: 103 (intersection)
available_tariff_data_desta_id_intersection <- lieshout_pta %>%
  bind_rows(baccini_pta2) %>%
  mutate(desta_id_data = paste0(desta_id, "_", data_source)) %>%
  distinct(desta_id)

nrow(available_tariff_data_desta_id_intersection)

# Van Lieshout OR Baccini coded 131 total PTAs (union) 
# coded using creating a string of desta_id and data_source, There are some overlaps, the string help account for that.
available_tariff_data_desta_id_union <- lieshout_pta %>%
  bind_rows(baccini_pta2) %>%
  mutate(desta_id_data = paste0(desta_id, "_", data_source)) %>%
  distinct(desta_id_data) 

nrow(available_tariff_data_desta_id_union)



# find other DESTA PTAs that have not been coded
# here I'm going to subset based on Lieshout's list since the Baccini et al. (2018) code procedure was very opaque.

coded_pta_list <- available_tariff_data_desta_id_lieshout %>%
  pull()

desta_list_remaining_pta_lieshout <- desta_list %>%
  subset(entry_type == "base_treaty") %>%
  select(base_treaty, year, entryforceyear, name) %>%
  rename(desta_id = base_treaty,
         eif_year = entryforceyear,
         agr_year = year) %>%
  subset(!desta_id %in% coded_pta_list)

# save csv of PTA that is remaining
write_csv(desta_list_remaining_pta_lieshout, here("A5.DataContribution/remaining_pta_list", "desta_list_remaining_pta_lieshout.csv"))


# pull country.names -----

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

complete_pta_to_code_list <- desta_list %>%
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
  subset(!desta_id %in% coded_pta_list) %>%
  left_join(desta_index)

write_csv(complete_pta_to_code_list, here("A5.DataContribution/remaining_pta_list", "complete_pta_to_code_list.csv"))

table(complete_pta_to_code_list$bilateral, 
      complete_pta_to_code_list$fta, 
      complete_pta_to_code_list$post2000, 
      complete_pta_to_code_list$eif)
# among 782 PTAs, 515 is bilateral
# among the 515 that are bilateral, 249 are free trade agreements. 
# among PTAs that are post 2000 (inclusive of 2000), 123 bilateral FTAs remain to be coded
# among bilateral FTAs that are post 2000, 120 entered into force. 


desta_list_bilateral <- complete_pta_to_code_list %>%
  subset(fta == "Free Trade Agreement" & bilateral == "bilateral" & post2000 == "Post-2000" & eif == "Entered into force")

# in sum, 121 FTAs, 

write_csv(desta_list_bilateral, here("A5.DataContribution/remaining_pta_list", "desta_list_bilateral.csv"))

table(desta_list_bilateral$n_s)


# OLD. MISC.
# # find DESTA PTAs to be RECODED from baccini et al. (2018), only PTAs that have NOT been done by Lieshout
# available_tariff_data_desta_id_lieshout <- lieshout_pta %>%
#   mutate(desta_id_data = paste0(desta_id, "_", data_source)) %>%
#   distinct(desta_id) %>%
#   pull()
# 
# baccini_recode <- lieshout_pta %>%
#   bind_rows(baccini_pta2) %>%
#   mutate(desta_id_data = paste0(desta_id, "_", data_source)) %>%
#   distinct(desta_id) %>%
#   subset(!desta_id %in% available_tariff_data_desta_id_lieshout) 
#   
# 
# # recode 33 PTAs from baccini, since Lieshout has some intersections. 
# nrow(baccini_recode)
# 
# write_csv(baccini_recode, here("A5.DataContribution/remaining_pta_list", "baccini_recode.csv"))
# 

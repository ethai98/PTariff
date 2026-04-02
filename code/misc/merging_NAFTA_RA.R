# NAFTA merging from RA work

library(tidyverse)
library(tidylog)

original_636_USA_MEX_CAN_gpt_handcoded <- read_excel("ET_data/6.USA_Bush_FTAs_indiv_spreadsheet/636_USA_MEX.CAN_gpt_handcoded.xlsx", 
                                             sheet = "schedule")

RA_636_USA_MEX_CAN_gpt_handcoded_v2 <- read_excel("~/Library/CloudStorage/Dropbox/FTariff Assistance/636_USA_MEX.CAN_gpt_handcoded_v2.xlsx", 
                                                  sheet = "schedule") %>%
  rename(corrected_category = category,
         corrected_details = details)


merged_636_USA_MEX_CAN_gpt_handcoded <- original_636_USA_MEX_CAN_gpt_handcoded %>%
  left_join(RA_636_USA_MEX_CAN_gpt_handcoded_v2) %>%
  # double check whether the original category and corrected_category is the for most coded products
  mutate(correct = ifelse(category == corrected_category, "yes", "no")) %>%
  select(-category, -details) %>%
  rename(category = corrected_category,
        details = corrected_details)
  
write_csv(merged_636_USA_MEX_CAN_gpt_handcoded, "ET_data/6.USA_Bush_FTAs_indiv_spreadsheet/misc/RAmerged_636_USA_MEX_CAN_gpt_handcoded.csv", na = "")



# 01 create clean data (ET)


# Clean all spreadsheets ---------------------------------------------------

## manual check needed: presence of 1, 3, 5 digit codes 
## and HS digits indicated and observed do not match


## need to add some code somewhere that adds the iso3c country code

require(here)

source("ET_code/functions/ET_cleanschedule.R") # this version saves all 8 digits cleaned schedules into 7.USA_8digits_schedules

# list all the spreadsheets to be feed into for loops
all.spreadsheets <- list.files("ET_data/indiv_spreadsheets", recursive = T)

for(s in 1:length(all.spreadsheets)){
  
  print(paste("Starting schedule #", s, ": ", all.spreadsheets[s], sep=""))
  
  # using the function in cleanschedule.R
  cleanschedule(all.spreadsheets[s])
  
}

# Collapse into six digits ------------------------------------------------

source("EVL_code/functions/sixdigits_new.R")

all.cleanedschedules <- list.files("ET_data/cleaned_schedules")

for(s in 1:length(all.cleanedschedules)){
  
  print(paste("Starting schedule #", s, ": ", all.cleanedschedules[s], sep=""))
  
  sixdigits(all.cleanedschedules[s])
  
}

# Merging on different HS codes -------------------------------------------

source("EVL_code/functions/allhs.R")

all.sixdigitschedules <- list.files("ET_data/sixdigit_schedules")

for(s in 1:length(all.sixdigitschedules)){
  
  print(paste("Starting schedule #", s, ": ", all.sixdigitschedules[s], sep=""))
  
  allhs(all.cleanedschedules[s])
  
}


# # Merge on MFN baserates --------------------------------------------------
# 
# ## manual check needed: where are MFN rates unavailable?
# 
# source("code/functions/baserates.R")
# 
# all.hscorschedules <- list.files("data/hscor_schedules", recursive = T)
# 
# for(s in 1:length(all.hscorschedules)){
#   
#   print(paste("Starting schedule #", s, ": ", all.hscorschedules[s], sep=""))
#   
#   baserates(all.hscorschedules[s])
#   
# }

# Saving into final -------------------------------------------------------

# to allow easier adding of cleaning steps, set up one folder of final schedules
file.copy(paste("ET_data/hscor_schedules/", 
                list.files("ET_data/hscor_schedules"), sep=""), 
          "ET_data/final_schedules", overwrite = T)

# that's it! the next file will create merged datasets for easier loading.


# merge all files into 1 dataset -----

all.cleaned.schedules <- list.files("ET_data/final_schedules")

scheds <- data.frame()

for(s in 1:length(all.cleaned.schedules)){
  
  print(paste("Adding schedule #", s, " of ", length(all.cleaned.schedules), sep=""))
  
  load(paste("ET_data/final_schedules/", all.cleaned.schedules[s], sep=""))
  
  scheds <- bind_rows(scheds, sched)
  
}

# reorder columns!
scheds <- scheds %>%
  select(country, partner, agreement, code, category, agr_year, eif_year, hs_original, starts_with("HS"), everything(), -X)

saveRDS(scheds, here("ET_data", "allschedules.rds"), compress = F)
rm(scheds, sched)

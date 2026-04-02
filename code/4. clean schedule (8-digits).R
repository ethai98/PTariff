# USA_8digits_schedules_cleanschedules.R
# Eric Thai
# 3/12/24


require(here)

# 1 clean schedule

# 1.1. multiple treatment per 8-digit product ------
# Clean NAFTA and CAFTA sheets separately since it has multiple treatment per 8-digit product.
source("PTariff/ET_code/functions/ET_scheduleclean_MultiTreat.R")

multitreat.spreadsheets <- list.files("ET_data/6.MultiTreat_indiv_spreadsheet", pattern="^[^~].*\\.xlsx$", recursive=FALSE)

for(s in 1:length(multitreat.spreadsheets)){
  
  print(paste("Starting schedule #", s, ": ", multitreat.spreadsheets[s], sep=""))
  
  # using the function in cleanschedule.R
  cleanschedule_MultiTreat(multitreat.spreadsheets[s])
  
}

# 1.2. TPP -----
source("PTariff/ET_code/functions/ET_scheduleclean_TPP.R")

tpp.spreadsheets <- list.files("ET_data/6.TPP", pattern="^[^~].*\\.xlsx$", recursive=FALSE)

for(s in 1:length(tpp.spreadsheets)){
  
  print(paste("Starting schedule #", s, ": ", tpp.spreadsheets[s], sep=""))
  
  # using the function in cleanschedule.R
  cleanschedule_TPP(tpp.spreadsheets[s])
  
}


# 1.3. all other ------
# list all the spreadsheets to be feed into for loops
source("PTariff/ET_code/functions/ET_cleanschedule_USFTA.R") # this version saves all 8 digits cleaned schedules into 7.USA_8digits_schedules

all.spreadsheets <- list.files("ET_data/6.USA_FTAs_indiv_spreadsheet", recursive = T)

usa.spreadsheets <- all.spreadsheets[grep("USA", all.spreadsheets)]
usa.spreadsheets

for(s in 1:length(usa.spreadsheets)){
  
  print(paste("Starting schedule #", s, ": ", usa.spreadsheets[s], sep=""))
  
  # using the function in cleanschedule.R
  cleanschedule(usa.spreadsheets[s])
  
}

# maintain 8 digits, but merge in various other HS version based on HS_6d. -------

source("PTariff/ET_code/functions/ET_8digits_allhs_USFTA.R")

all.8digits.usfta<- list.files("ET_data/7.USA_8digits_schedules")

for(s in 1:length(all.8digits.usfta)){
  
  print(paste("Starting schedule #", s, ": ", all.8digits.usfta[s], sep=""))
  
  allhs(all.8digits.usfta[s])
  
}


# merge US FTAs --------------------------------------------------------

all.usfta.schedules <- list.files("ET_data/8.USFTA")

scheds <- data.frame()

for(s in 1:length(all.usfta.schedules)){
  
  print(paste("Adding schedule #", s, " of ", length(all.usfta.schedules), sep=""))
  
  load(paste("ET_data/8.USFTA/", all.usfta.schedules[s], sep=""))
  
  scheds <- bind_rows(scheds, sched)
  
}

# reorder columns!
scheds <- scheds %>%
  select(country, partner, agreement, code, category, agr_year, eif_year, hs_original, starts_with("HS"), everything(), -HS_6d, -X)

saveRDS(scheds, here("ET_data", "USA_FTAs_april2025.rds"), compress = F)
rm(scheds, sched)


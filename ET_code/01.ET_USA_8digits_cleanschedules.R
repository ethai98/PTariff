# USA_8digits_schedules_cleanschedules.R
# Eric Thai
# 3/12/24


require(here)

source("ET_code/functions/ET_cleanschedule_USFTA.R") # this version saves all 8 digits cleaned schedules into 7.USA_8digits_schedules

# list all the spreadsheets to be feed into for loops
all.spreadsheets <- list.files("ET_data/6.USA_Bush_FTAs_indiv_spreadsheet", recursive = T)

usa.spreadsheets <- all.spreadsheets[grep("USA", all.spreadsheets)]
usa.spreadsheets

for(s in 1:length(usa.spreadsheets)){
  
  print(paste("Starting schedule #", s, ": ", usa.spreadsheets[s], sep=""))
  
  # using the function in cleanschedule.R
  cleanschedule(usa.spreadsheets[s])
  
}

# maintain 8 digits, but merge in various other HS version based on HS_6d. -------

source("ET_code/functions/ET_8digits_allhs_USFTA.R")

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

saveRDS(scheds, here("ET_data", "USA_FTAs.rds"), compress = F)
rm(scheds, sched)


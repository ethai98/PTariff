# ET_scheduleclean_NAFTA.R


cleanschedule_TPP <- function(whichsched){
  # whichsched <- "899_USA_TPP_gpt_handcoded.xlsx"
  require(openxlsx)
  require(tidyverse)
  require(countrycode)
  
  # Loading files
  sched.meta <- read.xlsx(paste("ET_data/6.TPP/", 
                                whichsched, sep=""), 
                          sheet = 1, colNames=TRUE, rowNames=TRUE) # read sheet 1: metadata (updated 3/12/24 ET)
  sched.full <- read.xlsx(paste("ET_data/6.TPP/", 
                                whichsched, sep=""), 
                          sheet = 2) # read sheet 2: the actual product-level tariff schedule
  sched.cat <- read.xlsx(paste("ET_data/6.TPP/", 
                               whichsched, sep=""), 
                         sheet = 3, colNames=TRUE, rowNames=FALSE) # read sheet 3: categories (updated 3/12/24 ET)
  
  
  sched.full <- sched.full %>%
    # fill in non-remarks with iso2c code of all TPP partners 
    mutate(remarks = ifelse(is.na(remarks), "AU, BR, CA, CL, JP,MX, MY, NZ, PE,SG, VN", remarks)) %>%
    # separate rows to expand dataset for each trade partner
    separate_rows(code, remarks, sep = ",") %>%
    mutate(remarks = str_replace_all(remarks, " ", "")) %>%
    mutate(partner_iso3c = countrycode(remarks, origin = "iso2c", destination = "iso3c")) %>%
    # correct Brunei 
    mutate(partner_iso3c = ifelse(partner_iso3c == "BRA", "BRN", partner_iso3c))
    
  
  # should generate 11 observation for each code (one for each partner)
  sched.full %>%
    count(code) %>%
    filter(n != 11)
  
  sched <- sched.full
  
  if(is.na(sched.meta["hs_version",])){
    print("Warning: HS version is missing from metadata")
  } 
  hs.sched <- sched.meta["hs_version",]
  hs.cor <- read.xlsx("EVL/EVL_data/hs_codes/hs_correspondence.xlsx")
  hs.full <- unique(hs.cor[,hs.sched]) # get the column that correspond to hs_version
  hs.full <- subset(hs.full, hs.full != "NULL")
  rm(hs.cor)
  
  # Some checks that everything looks alright with the HS codes
  sched$code <- gsub(" ", "", sched$code)
  
  # check 1: are there any NA codes?
  if(any(is.na(sched$code))){
    print("Warning: There are NA HS codes in the schedule")
  }
  
  # check 2: does the number of digits always match what is indicated
  digs <- sched.meta["digits",]
  
  if(!is.na(suppressWarnings(as.numeric(digs))) & !is.na(sched[1,1])){
    digs <- as.numeric(digs)
    if(all(nchar(sched$code) == digs)){
      # all good!
    } else{
      print("Warning: HS code digits indicated and observed do not match")
    }
  }
  
  # If codes shorter than 6 digits included, expand to subcategories
  if(is.na(sched[1,1])){
    ## this check is not needed for schedules where all tariffs are removed
  } else if(min(nchar(sched$code)) < 6){
    
    shortcodes <- sched$code[nchar(sched$code) < 6]
    longcodes <- sched$code[nchar(sched$code) >= 6]
    
    # these should all be either 2 or 4 digits - double-check that
    if(any(nchar(shortcodes) == 1) | any(nchar(shortcodes)==3) | any(nchar(shortcodes)==5)){
      print("Warning: There are 1, 3, or 5 digit HS codes in the schedule")
    }
    
    for(d in unique(nchar(shortcodes))){
      shortcodes.use <- shortcodes[nchar(shortcodes)==d]
      for(c in 1:length(shortcodes.use)){
        # finding codes that fall under this 2 or 4 digit code
        codes.to.add <- hs.full[substr(hs.full, 1, d) == shortcodes.use[c]]
        # and which are not already present in the schedule
        codes.to.add <- codes.to.add[!(codes.to.add %in% substr(longcodes, 1, 6))]
        if(length(codes.to.add)==0){
          next()
        }
        # adding those rows to the schedule
        cats.to.add <- sched[sched$code == shortcodes.use[c], "category"]
        for(r in 1:length(cats.to.add)){
          rows.to.add <- data.frame(code = codes.to.add,
                                    baserate = NA,
                                    category = cats.to.add[r],
                                    details = NA)
          sched <- rbind(sched, rows.to.add)
        }
      }
    }
    
    # removing the short code entries from the schedule
    sched <- sched[!(sched$code %in% shortcodes),]
    
  }
  # Note: there will also be codes longer than 6 digits,
  # for maximum flexibility that issue is dealt with in a separate function
  
  
  
  ### Category
  
  # check: does the category file have the right column names?
  if(!("reduced" %in% colnames(sched.cat))){
    print("Warning: Reduced variable missing in category classification")
  }
  
  # prepping categories for merging (drop path and notes columns) + backload (USA specific FTA)
  cat.formerge <- data.frame(category = sched.cat$cat_code,
                             sched.cat[,2:9]) # adjust here if has more sched.cat column to add
  
  # merging details about what a category means to the schedule
  sched <- merge(sched, cat.formerge, by = "category", all.x=TRUE)
  
  # 4/3/25 correct cdf
  
  sched <- sched %>%
    mutate(cdf = ifelse(baserate == "0" & category == "EIF", 1, 0)) %>%
    mutate(years_delay = ifelse(cdf == 1, NA, years_delay)) %>%
    select(-remarks)
    
  
  
  # adding rows and category info for positive and negative lists
  if(sched.meta["list_type",] == "positive"){
    # select the codes not in the schedule (incl. no more-than-6-digit version)
    omitted.codes <- hs.full[!(hs.full %in% substr(sched$code, 1, 6))]
    # add to schedule, indicating that these tariffs will not be removed
    rows.to.add <- data.frame(category = "absent_poslist",
                              code = omitted.codes,
                              baserate = NA, details = NA,
                              reduced = 0, removed = 0,
                              immediate = NA, years_delay = NA, equal_steps = NA)
    sched <- rbind(sched, rows.to.add)
  }
  if(sched.meta["list_type",] == "negative"){
    # select the codes not in the schedule (incl. no more-than-6-digit version)
    omitted.codes <- hs.full[!(hs.full %in% substr(sched$code, 1, 6))]
    # add to schedule, indicating that these tariffs will not be removed
    rows.to.add <- data.frame(category = "absent_neglist",
                              code = omitted.codes,
                              baserate = NA, details = NA,
                              reduced = 1, removed = 1,
                              immediate = 1, years_delay = NA, equal_steps = NA)
    sched <- rbind(sched, rows.to.add)
  }
  
  # how rare is a category designation?
  sched$category <- as.character(sched$category)
  sched$delayed <- ifelse(sched$immediate == 1, 0, 1)
  cattab <- table(sched$category)
  sched$cat_share <- cattab[sched$category] / sum(cattab)
  cattabdel <- table(sched$category[sched$delayed == 1])
  sched$cat_share_del <- ifelse(sched$delayed == 1, 
                                cattabdel[sched$category] / sum(cattabdel), NA)
  
  
  ### Meta info
  meta.formerge <- trimws(t(sched.meta)[1:6])
  sched <- cbind(sched, t(meta.formerge))
  colnames(sched) <- c(colnames(sched)[1:16],  # maybe need to change this if I'm including backloaded and years_delay, add the number of rows
                       rownames(sched.meta)[1:5],
                       "hs_original")
  
  ### Setting column classes correctly
  for(v in c("category", "code", "baserate", "details",
             "agreement", "country", "partner", "hs_original")){
    sched[,v] <- as.character(sched[,v])
  }
  for(v in c("reduced", "removed", "immediate", "years_delay", "equal_steps", "backloaded", "years_pause", "cdf", # added backloaded, years_pause, and cdf
             "agr_year", "eif_year", "delayed", "cat_share", "cat_share_del")){
    sched[,v] <- as.numeric(as.character(sched[,v]))
  }
  
  sched <- sched %>%
    select(-partner) %>%
    rename(partner = partner_iso3c)
  
  
  # Remove "_gpt_handcoded" from whichsched, if present
  whichsched_clean = gsub("_gpt_handcoded", "", whichsched)
  
  # Remove directory paths and ".xlsx", then append "_cleaned"
  filename = paste("ET_data/7.USA_8digits_schedules/", 
                   gsub(".xlsx", "", gsub(".*/", "", whichsched_clean)), 
                   "_cleaned.Rdata", sep="")
  
  ### Saving
  save(sched, file = filename)
  
  
}

# Writing my own code, supplement EVL, using dplur ----
# Eric Thai
# 7/13/23

# load packages

require(tidyverse)


# Step 1: Clean Raw Schedule (corresponds with EVL's cleanschedule.R) -----

# list indiv_spreadsheet files in the folder - cleaned manually by RA or me
whichsched <- list.files(here("data/indiv_spreadsheets"))

cleanschedule <- function(whichsched){
  
  # Loading packages
  require(openxlsx)
  
  # Loading files
  sched.meta <- read.xlsx(paste("data/indiv_spreadsheets/", 
                                whichsched, sep=""), 
                          sheet = 1, colNames=F, rowNames=T) # read sheet 1: metadata
  sched.full <- read.xlsx(paste("data/indiv_spreadsheets/", 
                                whichsched, sep=""), 
                          sheet = 2) # read sheet 2: the actual product-level tariff schedule
  sched.cat <- read.xlsx(paste("data/indiv_spreadsheets/", 
                               whichsched, sep=""), 
                         sheet = 3) # read sheet 3: categories
  
  # Setting up the schedule file to be further filled in
  sched <- sched.full
  
  ### HS Codes
  
  # Full list of all HS codes in relevant version
  if(is.na(sched.meta["hs_version",])){
    print("Warning: HS version is missing from metadata")
  } 
  hs.sched <- sched.meta["hs_version",]
  hs.cor <- read.xlsx("data/hs_codes/hs_correspondence.xlsx")  # load in correspondence
  hs.full <- unique(hs.cor[,hs.sched]) # unique is to eliminate any duplicates
  hs.full <- subset(hs.full, hs.full != "NULL")
  rm(hs.cor)
  
  # Some checks that everything looks alright with the HS codes
  sched$code <- gsub(" ", "", sched$code) # global substitution gsub, looking for " " (single space), and replace it with "" (empty string)
  # check 1: are there any NA codes?
  if(any(is.na(sched$code))){
    print("Warning: There are NA HS codes in the schedule")
  }
  # check 2: does the number of digits always match what is indicated
  digs <- sched.meta["digits",] # gather from metadata on the number of digits is recorded. 
  if(!is.na(suppressWarnings(as.numeric(digs))) & !is.na(sched[1,1])){ # if digs is not NA (AKA was recorded in meta data) and sched's code's first line is not NA (which is a weird condition)
    digs <- as.numeric(digs) # then convert dig to numeric
    if(all(nchar(sched$code) == digs)){ # if the number of character in a column is the same as digs, then all good, otherwise, print
      # all good!
    } else{
      print("Warning: HS code digits indicated and observed do not match")
    }
  }
  
  # If codes shorter than 6 digits included, expand to subcategories
  if(is.na(sched[1,1])){
    ## this check is not needed for schedules where all tariffs are removed
  } else if(min(nchar(sched$code)) < 6){ # check what is the minimum number of characters in code column
    
    shortcodes <- sched$code[nchar(sched$code) < 6]
    longcodes <- sched$code[nchar(sched$code) >= 6]
    
    # these should all be either 2 or 4 digits - double-check that
    # ET: um why are we ignoring 2 or 4 digits?
    if(any(nchar(shortcodes) == 1) | any(nchar(shortcodes)==3) | any(nchar(shortcodes)==5)){
      print("Warning: There are 1, 3, or 5 digit HS codes in the schedule")
    }
    d = 5
    shortcodes.use = c("15179")
    for(d in unique(nchar(shortcodes))){
      shortcodes.use <- shortcodes[nchar(shortcodes)==d] # find the code that is shorter than digs
      for(c in 1:length(shortcodes.use)){
        # finding codes that fall under this 2 or 4 digit code
        codes.to.add <- hs.full[substr(hs.full, 1, d) == shortcodes.use[c]] # substr function extract the first 5 character of the list of hs.full, find any that matches the 5 digit code name
        # and which are not already present in the schedule
        codes.to.add <- codes.to.add[!(codes.to.add %in% substr(longcodes, 1, 6))] # ChatGPT explanation: the code filters the codes.to.add variable by removing elements that have a match (in terms of their first 6 characters) in the longcodes variable. The resulting codes.to.add will only contain elements that do not have a corresponding match in the substrings extracted from longcodes
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
  
  # prepping categories for merging (drop path and notes columns)
  cat.formerge <- data.frame(category = sched.cat$cat_code,
                             sched.cat[,2:6])
  
  # merging details about what a category means to the schedule
  sched <- merge(sched, cat.formerge, by = "category", all.x=TRUE)
  
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
  colnames(sched) <- c(colnames(sched)[1:12], 
                       rownames(sched.meta)[1:5],
                       "hs_original")
  
  ### Setting column classes correctly
  for(v in c("category", "code", "baserate", "details",
             "agreement", "country", "partner", "hs_original")){
    sched[,v] <- as.character(sched[,v])
  }
  for(v in c("reduced", "removed", "immediate", "years_delay", "equal_steps",
             "agr_year", "eif_year", "delayed", "cat_share", "cat_share_del")){
    sched[,v] <- as.numeric(as.character(sched[,v]))
  }
  
  ### Saving
  save(sched, file = paste("data/cleaned_schedules/", 
                           gsub(".xlsx", "", gsub(".*/", "", whichsched)), 
                           ".Rdata", sep=""))
  
} 

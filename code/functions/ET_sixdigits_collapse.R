
# sixdigits <- function(whichsched){
whichsched <- "643_USA_PAN_cleaned.Rdata"
  
  require(dplyr)
  
  # load data
  load(paste("ET_data/cleaned_schedules/", whichsched, sep=""))
  
  # possible approaches to collapsing decision codes
  ## pessimist (only removed if all removed)
  ## halfsies (removed if at least half removed)
  ## optimist (removed if any removed)
  
  # check whether collapsing is needed
  if(any(nchar(sched$code) < 6)){
    stop("Codes less than 6 digits detected")
  } 
  # if exactly six digits and all codes are unique, then append "pessimist", "optimist", "halfsies" to specified variables. 
  if(all(nchar(sched$code) == 6) & all(table(sched$code) == 1)){
    for(v in c("reduced", "removed", "immediate", "years_delay", 
               "equal_steps", "backloaded", "years_pause", "cat_share", "cat_share_del")){
      sched[,paste(v,c("pessimist", "optimist", "halfsies"),sep="_")] <- sched[,v]
      sched[,v] <- NULL
    }
    save(sched, file = paste("data/sixdigit_schedules/", whichsched, sep=""))
    return()
  } 

  codecount <- sched %>% group_by(code) %>% transmute(howmany = length(code))
  
  # setting up new schedule dataframe
  sched.new <- subset(sched, nchar(code) == 6 & codecount$howmany == 1)
  for(v in c("reduced", "removed", "immediate", "years_delay", 
             "equal_steps", "cat_share", "cat_share_del")){
    if(nrow(sched.new) > 0){
      sched.new[,paste(v,c("pessimist", "optimist", "halfsies"),sep="_")] <- sched.new[,v]
    }
    sched.new[,v] <- NULL
  }
  
  # selecting the relevant 6 digit codes to collapse to
  toolong <- sched$code[nchar(sched$code) > 6 | codecount$howmany > 1]
  sixcollapse <- unique(substr(toolong, 1, 6))
  
  # collapsing
  for(sd in 1:length(sixcollapse)){
    
    # selecting all subcodes to be collapsed
    subcodes <- subset(sched, substr(code,1,6) == sixcollapse[sd])
    
    # setting up new row to add
    addcode <- subcodes[1,]
    addcode$code <- sixcollapse[sd]
    addcode$baserate <- ifelse(length(unique(subcodes$baserate)==1),
                               subcodes$baserate[1], "mixed")
    
    # if all the same category, then it's simple
    # otherwise, need to consider different possible rules separately
    if(length(unique(subcodes$category)) == 1){
      
      for(v in c("reduced", "removed", "immediate", "years_delay", 
                 "equal_steps", "cat_share", "cat_share_del")){
        addcode[,paste(v,c("pessimist", "optimist", "halfsies"),sep="_")] <- addcode[,v]
        addcode[,v] <- NULL
      }
      
    } else{
      
      addcode$category <- "mixed"
      
      # pessimist: code only 1 if all subcodes have a 1
      addcode$reduced_pessimist <- ifelse(all(subcodes$reduced == 1), 1, 0)
      addcode$removed_pessimist <- ifelse(all(subcodes$removed == 1), 1, 0)
      addcode$immediate_pessimist <- ifelse(all(subcodes$immediate == 1), 1, 
                                            ifelse(addcode$reduced == 1, 0, NA))
      addcode$years_delay_pessimist <- ifelse(addcode$reduced == 0 | addcode$immediate == 1, NA,
                                              suppressWarnings(ifelse(all(is.na(subcodes$years_delay)), NA,
                                                                      max(subcodes$years_delay, na.rm=T))))
      addcode$equal_steps_pessimist <- ifelse(addcode$reduced == 0 | addcode$immediate == 1, NA,
                                              ifelse(all(subcodes$equal_steps == 1), 1, 0))
      addcode$cat_share_pessimist <- min(subcodes$cat_share, na.rm=T)
      addcode$cat_share_del_pessimist <- min(subcodes$cat_share, na.rm=T)
      
      # optimist: code 1 if any of the subcodes have a 1    
      addcode$reduced_optimist <- ifelse(any(subcodes$reduced == 1), 1, 0)
      addcode$removed_optimist <- ifelse(any(subcodes$removed == 1), 1, 0)
      addcode$immediate_optimist <- ifelse(any(subcodes$immediate == 1), 1, 
                                           ifelse(addcode$removed == 1, 0, NA))
      addcode$years_delay_optimist <- ifelse(addcode$reduced == 0 | addcode$immediate == 1, NA,
                                             suppressWarnings(ifelse(all(is.na(subcodes$years_delay)), NA, 
                                                                     min(subcodes$years_delay, na.rm=T))))
      addcode$equal_steps_optimist <- ifelse(addcode$reduced == 0 | addcode$immediate == 1, NA,
                                             ifelse(any(subcodes$equal_steps == 1), 1, 0))
      addcode$cat_share_optimist <- max(subcodes$cat_share, na.rm=T)
      addcode$cat_share_del_optimist <- max(subcodes$cat_share, na.rm=T)
      
      # halfsies: code 1 if at least half of subcodes have a 1
      addcode$reduced_halfsies <- ifelse(mean(subcodes$reduced) >= 0.5, 1, 0)
      addcode$removed_halfsies <- ifelse(mean(subcodes$removed) >= 0.5, 1, 0)
      addcode$immediate_halfsies <- ifelse(mean(subcodes$immediate, na.rm=T) >= 0.5, 1, 
                                           ifelse(addcode$reduced == 1, 0, NA))
      # addcode$years_delay_halfsies <- ifelse(addcode$reduced == 0 | addcode$immediate == 1, NA,
      #                                        suppressWarnings(ifelse(all(is.na(subcodes$years_delay)), NA,
      #                                                                mean(subcodes$years_delay, na.rm=T))))
      addcode$years_delay_halfsies <- suppressWarnings(ifelse(all(is.na(subcodes$years_delay)), NA,
                                                                     mean(subcodes$years_delay, na.rm=T)))
      addcode$equal_steps_halfsies <- ifelse(addcode$reduced == 0 | addcode$immediate == 1, NA,
                                             ifelse(mean(subcodes$equal_steps, na.rm=T) >= 0.5, 1, 0))
      addcode$cat_share_halfsies <- mean(subcodes$cat_share, na.rm=T)
      addcode$cat_share_del_halfsies <- mean(subcodes$cat_share, na.rm=T)
      
      for(v in c("reduced", "removed", "immediate", "years_delay", 
                 "equal_steps", "cat_share", "cat_share_del")){
        addcode[,v] <- NULL
      }
      
    }
    
    sched.new <- rbind(sched.new, addcode)
    
  }
  
  sched <- sched.new
  
  save(sched, file = paste("data/sixdigit_schedules/", whichsched, sep=""))
  
  }

sched.final <- sched %>%
  mutate(HS_6d = substr(code, 1, 6)) %>%
  filter(!is.na(years_delay)) %>%
  reframe(years_delay = mean(years_delay, na.rm=T), .by=HS_6d)

# misc. my own coding -----

sched.final <- sched %>%
  mutate(HS_6d = substr(code, 1, 6)) %>%
  # pessimist
  reframe(reduced_pessimist = ifelse(all(reduced == 1), 1, 0),
          removed_pessimist = ifelse(all(removed == 1), 1, 0),
          immediate_pessimist = ifelse(all(immediate == 1), 1, 
                                       ifelse(reduced == 1, 0,
                                              NA)), 
          years_delay_pessimist = suppressWarnings(max(years_delay, na.rm=T)),
          equal_steps_pessimist = ifelse(all(equal_steps == 1, na.rm=T), 1, 0),
          backloaded_pessimist = ifelse(is.na(years_delay), NA,
                                        ifelse(all(backloaded == 1, na.rm = T), 1, 0)),
          years_pause_pessimist = suppressWarnings(max(years_pause, na.rm=T)),
          
          
          
          years_delay_optimist = suppressWarnings(min(years_delay, na.rm=T)),
          years_delay_halfsies = suppressWarnings(mean(years_delay, na.rm=T)),
          
          # take out -Inf
          years_delay_pessimist = ifelse(is.infinite(years_delay_pessimist), NA, years_delay_pessimist),
          # take out -Inf
          years_delay_optimist = ifelse(is.infinite(years_delay_optimist), NA, years_delay_optimist),
          # take out NaN
          years_delay_halfsies = ifelse(is.nan(years_delay_halfsies), NA, years_delay_halfsies),
          
          .by = HS_6d)



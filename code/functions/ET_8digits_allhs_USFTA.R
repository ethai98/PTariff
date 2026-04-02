
allhs <- function(whichsched){
  require(dplyr)
  require(here)
  require(stringr)

  # loading datta
  load(paste("ET_data/7.USA_8digits_schedules/", whichsched, sep=""))
  
  sched <- sched %>%
    mutate(HS_6d = str_sub(code, 1, 6))
  
  # obtain original schedule info
  hs.orig <- sched$hs_original[1]
  sched[,hs.orig] <- sched$HS_6d
  
  if(hs.orig == "HS17"){
    corfls <- c("HS17_HS12", "HS17_HS07", "HS17_HS02", "HS17_HS96", "HS17_HS92")
  } else if(hs.orig == "HS12"){
    corfls <- c("HS17_HS12", "HS12_HS07", "HS12_HS02", "HS12_HS96", "HS12_HS92")
  } else if(hs.orig == "HS07"){
    corfls <- c("HS17_HS07", "HS12_HS07", "HS07_HS02", "HS07_HS96", "HS07_HS92")
  } else if(hs.orig == "HS02"){
    corfls <- c("HS17_HS02", "HS12_HS02", "HS07_HS02", "HS02_HS96", "HS02_HS92")
  } else if(hs.orig == "HS96"){
    corfls <- c("HS17_HS96", "HS12_HS96", "HS07_HS96", "HS02_HS96", "HS96_HS92")
  } else if(hs.orig == "HS92"){
    corfls <- c("HS17_HS92", "HS12_HS92", "HS07_HS92", "HS02_HS92", "HS96_HS92")
  } else{
    print("Error: non-existent original HS code system")
    return()
  }
  
  for(i in 1:5){
    hscor <- read.csv(paste("EVL/EVL_data/hs_codes/", corfls[i], ".csv", sep=""),
                      colClasses = "character")
    
    ## if doing forward concordance, there's some imperfect mapping
    ## right now, just picking the first match (not ideal)
    hscor <- hscor[!duplicated(hscor[,hs.orig]),]
    
    sched <- merge(sched, hscor, all.x=T)

    
  }
  
  # saving
  save(sched, file = paste("ET_data/8.USFTA/", whichsched, sep=""))
  
}

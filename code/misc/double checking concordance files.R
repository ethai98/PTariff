# double checking concordance files
# Eric Thai
# 11/28/24

library(here)
library(concordance)
library(tidyverse)
library(tidylog)
# purpose: double check concordance file EVL provided with that from concordance package
# confirm (11/28/24) EVL's HS concordances are consistent with Rconcordance files. 

# HS[year] and HS[revision]
# HS88/92 = HS0 = 1988-1995
# HS96 = HS1 = 1996-2001
# HS02 = HS2 = 2002-2006
# HS07 = HS3 = 2007-2011
# HS12 = HS4 = 2012-2016
# HS17 = HS5 = 2017-2021
# HS22 = HS6 = 2022-


# EVL hs files directory -----
EVL_directory = here("EVL_data/hs_codes")
setwd(EVL_directory)
temp = list.files(pattern = "\\.csv$")

# Import each file as a data.frame with its file name (without extension)
for (file_name in temp) {
  # Extract the base name without extension
  base_name <- tools::file_path_sans_ext(file_name)
  
  # Read the CSV file into a data frame
  file_path <- file.path(EVL_directory, file_name)
  data <- read.csv(file_path) %>%
    mutate(across(everything(), ~ sprintf("%06s", as.character(.))))  # Format with leading zeros
  
  # Assign the data frame to a variable named after the file
  assign(base_name, data)
}

# Concordance ----
# note: you can only concord backward.

# HS[year] and HS[revision]
# HS88/92 = HS0 = 1988-1995
# HS96 = HS1 = 1996-2001
# HS02 = HS2 = 2002-2006
# HS07 = HS3 = 2007-2011
# HS12 = HS4 = 2012-2016
# HS17 = HS5 = 2017-2021
# HS22 = HS6 = 2022-


# HS17_HS12 -----
data("hs5_hs4")

hs5_hs4 <- hs5_hs4 %>%
  select(HS5_6d, HS4_6d) %>%
  rename(HS17 = HS5_6d,
         HS12 = HS4_6d)

# check
hs5_hs4_check <- hs5_hs4 %>%
  left_join(HS17_HS12)

# HS17_HS07 -----
data("hs5_hs3")

hs5_hs3 <- hs5_hs3 %>%
  select(HS5_6d, HS3_6d) %>%
  rename(HS17 = HS5_6d,
         HS07 = HS3_6d)

# check
hs5_hs3_check <- hs5_hs3 %>%
  left_join(HS17_HS07)

# HS17_HS02 -----
data("hs5_hs2")

hs5_hs2 <- hs5_hs2 %>%
  select(HS5_6d, HS2_6d) %>%
  rename(HS17 = HS5_6d,
         HS02 = HS2_6d)

# check
hs5_hs2_check <- hs5_hs2 %>%
  left_join(HS17_HS02)

# HS17_HS96 -----
data("hs5_hs1")

hs5_hs1 <- hs5_hs1 %>%
  select(HS5_6d, HS1_6d) %>%
  rename(HS17 = HS5_6d,
         HS96 = HS1_6d)

# check
hs5_hs1_check <- hs5_hs1 %>%
  left_join(HS17_HS96)

# HS17_HS88/92 -----
data("hs5_hs0")

hs5_hs0 <- hs5_hs0 %>%
  select(HS5_6d, HS0_6d) %>%
  rename(HS17 = HS5_6d,
         HS92 = HS0_6d)

# check
hs5_hs0_check <- hs5_hs0 %>%
  left_join(HS17_HS92)



# HS12_HS07 -----
data("hs4_hs3")

hs4_hs3 <- hs4_hs3 %>%
  select(HS4_6d, HS3_6d) %>%
  rename(HS12 = HS4_6d,
         HS07 = HS3_6d)

# check
hs4_hs3_check <- hs4_hs3 %>%
  left_join(HS12_HS07)

# HS12_HS02 -----
data("hs4_hs2")

hs4_hs2 <- hs4_hs2 %>%
  select(HS4_6d, HS2_6d) %>%
  rename(HS12 = HS4_6d,
         HS02 = HS2_6d)

# check
hs4_hs2_check <- hs4_hs2 %>%
  left_join(HS12_HS02)

# HS12_HS96 -----
data("hs4_hs1")

hs4_hs1 <- hs4_hs1 %>%
  select(HS4_6d, HS1_6d) %>%
  rename(HS12 = HS4_6d,
         HS96 = HS1_6d)

# check
hs4_hs1_check <- hs4_hs1 %>%
  left_join(HS12_HS96)

# HS12_HS88/92 -----
data("hs4_hs0")

hs4_hs0 <- hs4_hs0 %>%
  select(HS4_6d, HS0_6d) %>%
  rename(HS12 = HS4_6d,
         HS92 = HS0_6d)

# check
hs4_hs0_check <- hs4_hs0 %>%
  left_join(HS12_HS92)

# HS07_HS02 -----
data("hs3_hs2")

hs3_hs2 <- hs3_hs2 %>%
  select(HS3_6d, HS2_6d) %>%
  rename(HS07 = HS3_6d,
         HS02 = HS2_6d)

# check
hs3_hs2_check <- hs3_hs2 %>%
  left_join(HS07_HS02)

# HS07_HS96 -----
data("hs3_hs1")

hs3_hs1 <- hs3_hs1 %>%
  select(HS3_6d, HS1_6d) %>%
  rename(HS07 = HS3_6d,
         HS96 = HS1_6d)

# check
hs3_hs1_check <- hs3_hs1 %>%
  left_join(HS07_HS96)

# HS07_HS88/92 -----
data("hs3_hs0")

# taking out "0000NA"
HS07_HS92 <- HS07_HS92 %>%
  filter(HS07 != "0000NA")

hs3_hs0 <- hs3_hs0 %>%
  select(HS3_6d, HS0_6d) %>%
  rename(HS07 = HS3_6d,
         HS92 = HS0_6d)

# check
hs3_hs0_check <- hs3_hs0 %>%
  full_join(HS07_HS92)

# HS02_HS96 -----
data("hs2_hs1")

hs2_hs1 <- hs2_hs1 %>%
  select(HS2_6d, HS1_6d) %>%
  rename(HS02 = HS2_6d,
         HS96 = HS1_6d)

HS02_HS96 <- HS02_HS96 %>%
  filter(HS02 != "0000NA")

# check
hs2_hs1_check <- hs2_hs1 %>%
  left_join(HS02_HS96)

# HS02_HS88/92 -----
data("hs2_hs0")

hs2_hs0 <- hs2_hs0 %>%
  select(HS2_6d, HS0_6d) %>%
  rename(HS02 = HS2_6d,
         HS92 = HS0_6d)

# check
hs2_hs0_check <- hs2_hs0 %>%
  left_join(HS02_HS92)

# HS96_HS88/92 -----
data("hs1_hs0")

hs1_hs0 <- hs1_hs0 %>%
  select(HS1_6d, HS0_6d) %>%
  rename(HS96 = HS1_6d,
         HS92 = HS0_6d)

HS96_HS92 <- HS96_HS92 %>%
  filter(HS96 != "0000NA")

# check
hs1_hs0_check <- hs1_hs0 %>%
  left_join(HS96_HS92)


## 1. checking HS17_HS12 -----

data("hs5_hs4")

hs5_hs4 <- hs5_hs4 %>%
  select(HS5_6d, HS4_6d) %>%
  rename(HS17 = HS5_6d,
         HS12 = HS4_6d)

# check
hs5_hs4_check <- hs5_hs4 %>%
  left_join(HS17_HS12)


data("hs5_hs4")
data("hs5_hs3")
data("hs5_hs2")
data("hs5_hs1")
data("hs5_hs0")

data("hs4_hs3")
data("hs4_hs2")
data("hs4_hs1")
data("hs4_hs0")

data("hs3_hs2")
data("hs3_hs1")
data("hs3_hs0")

data("hs2_hs1")
data("hs2_hs0")

data("hs1_hs0")





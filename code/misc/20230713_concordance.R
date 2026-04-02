# Use ISK's Concordance package to create hs_correspondence file. 
# Eric Thai
# 7/13/23

library(concordance)
library(tidyverse)

# HS92 = HS0
# HS96 = HS1
# HS02 = HS2
# HS07 = HS3
# HS12 = HS4
# HS17 = HS5
# HS23 = HS6


# concordance from HS6 to previous versions ----
data("hs6_hs5")
hs6_hs5 <- force(hs6_hs5) %>%
  select(HS6_6d, HS5_6d)

data("hs6_hs4")
hs6_hs4 <- force(hs6_hs4) %>%
  select(HS6_6d, HS4_6d)

data("hs6_hs3") 
hs6_hs3 <-force(hs6_hs3) %>%
  select(HS6_6d, HS3_6d)

data("hs6_hs2") 
hs6_hs2 <-force(hs6_hs2) %>%
  select(HS6_6d, HS2_6d)

data("hs6_hs1")
hs6_hs1 <-force(hs6_hs1) %>%
  select(HS6_6d, HS1_6d)

data("hs6_hs0")
hs6_hs0 <-force(hs6_hs0) %>%
  select(HS6_6d, HS0_6d)

full_hs_concordance_hs6 <- hs6_hs5 %>%
  full_join(hs6_hs4) %>%
  full_join(hs6_hs3) %>%
  full_join(hs6_hs2) %>%
  full_join(hs6_hs1) %>%
  full_join(hs6_hs0)

# concordance from HS5 to previous versions ----
data("hs5_hs4")
hs5_hs4 <- force(hs5_hs4) %>%
  select(HS5_6d, HS4_6d)

data("hs5_hs3") 
hs5_hs3 <-force(hs5_hs3) %>%
  select(HS5_6d, HS3_6d)

data("hs5_hs2") 
hs5_hs2 <-force(hs5_hs2) %>%
  select(HS5_6d, HS2_6d)

data("hs5_hs1")
hs5_hs1 <-force(hs5_hs1) %>%
  select(HS5_6d, HS1_6d)

data("hs5_hs0")
hs5_hs0 <-force(hs5_hs0) %>%
  select(HS5_6d, HS0_6d)

full_hs_concordance_hs5 <- hs5_hs4 %>%
  full_join(hs5_hs3) %>%
  full_join(hs5_hs2) %>%
  full_join(hs5_hs1) %>%
  full_join(hs5_hs0)

# concordance from HS4 to previous versions ----
data("hs4_hs3") 
hs4_hs3 <-force(hs4_hs3) %>%
  select(HS4_6d, HS3_6d)

data("hs4_hs2") 
hs4_hs2 <-force(hs4_hs2) %>%
  select(HS4_6d, HS2_6d)

data("hs4_hs1")
hs4_hs1 <-force(hs4_hs1) %>%
  select(HS4_6d, HS1_6d)

data("hs4_hs0")
hs4_hs0 <-force(hs4_hs0) %>%
  select(HS4_6d, HS0_6d)

full_hs_concordance_hs4 <- hs4_hs3 %>%
  full_join(hs4_hs2) %>%
  full_join(hs4_hs1) %>%
  full_join(hs4_hs0)

# concordance from HS3 to previous versions ----

data("hs3_hs2") 
hs3_hs2 <-force(hs3_hs2) %>%
  select(HS3_6d, HS2_6d)

data("hs3_hs1")
hs3_hs1 <-force(hs3_hs1) %>%
  select(HS3_6d, HS1_6d)

data("hs3_hs0")
hs3_hs0 <-force(hs3_hs0) %>%
  select(HS3_6d, HS0_6d)

full_hs_concordance_hs3 <- hs3_hs2 %>%
  full_join(hs3_hs1) %>%
  full_join(hs3_hs0)


# concordance from HS2 to previous versions ----

data("hs2_hs1")
hs2_hs1 <-force(hs2_hs1) %>%
  select(HS2_6d, HS1_6d)

data("hs2_hs0")
hs2_hs0 <-force(hs2_hs0) %>%
  select(HS2_6d, HS0_6d)

full_hs_concordance_hs2 <- hs2_hs1 %>%
  full_join(hs2_hs0)

# concordance from HS1 to previous versions ----

data("hs1_hs0")
hs1_hs0 <-force(hs1_hs0) %>%
  select(HS1_6d, HS0_6d)


# full join everything -----

full_hs_concordance <- full_hs_concordance_hs6 %>%
  full_join(full_hs_concordance_hs5) %>%
  full_join(full_hs_concordance_hs4) %>%
  full_join(full_hs_concordance_hs3) %>%
  full_join(full_hs_concordance_hs2) %>%
  full_join(hs1_hs0)


# compare to EVL's hs_correspondence
require(openxlsx)
hs.cor <- read.xlsx("data/hs_codes/hs_correspondence.xlsx") %>%
  select(HS92:HS17) %>%
  unique()

# conclusion: I'll continue to use EVL's hs_correspondence. 


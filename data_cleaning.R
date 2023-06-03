# downloaded from: https://www150.statcan.gc.ca/n1/pub/56m0003x/56m0003x2020001-eng.htm

# This code converts the SAS format data into csv file
library(haven)
library(dplyr)
data <- read_sas("data/cius2020_pumf.sas7bdat")

# The full data set contains thousands of variables, we will limit oursevles
# to a fewer variables
selected_df <- raw_df %>% 
  select(GENDER, # Non-binary are excluded
         EMP, # Employment status
         ED_G10, # Are they currently in school
         DV_010A, # use a smartphone to access the internet in the past 3 months
         DV_010B, # use a laptop to access the internet in the past 3 months
         DV_010C, # use a tablet to access the internet in the past 3 months
         DV_010D, # use a desktop to access the internet in the past 3 months
         DV_010G, # use wearables to access the internet in the past 3 months
         UI_055A, # Time spend playing online video games
         UI_060A, # Time spend using Internet
         UI_045A) # Time spend watching content

# we need to recode some observations based off the data dictionary provided
# by Statistics Canada
newvar_df <- selected_df %>% 
  mutate(video_game_time = case_when(UI_055A == 1 ~ 0,
                                     UI_055A == 2 ~ 2.5,
                                     UI_055A == 3 ~ 7.5,
                                     UI_055A == 4 ~ 15,
                                     UI_055A == 5 ~ 30,
                                     UI_055A == 6 ~ 40,
                                     TRUE ~ NA)) %>% 
  mutate(other_internet_time = case_when(UI_060A == 1 ~ 0,
                                         UI_060A == 2 ~ 2.5,
                                         UI_060A == 3 ~ 7.5,
                                         UI_060A == 4 ~ 15,
                                         UI_060A == 5 ~ 30,
                                         UI_060A == 6 ~ 40,
                                         TRUE ~ NA)) %>% 
  mutate(watch_time = case_when(UI_045A == 1 ~ 0,
                                UI_045A == 2 ~ 2.5,
                                UI_045A == 3 ~ 7.5,
                                UI_045A == 4 ~ 15,
                                UI_045A == 5 ~ 30,
                                UI_045A == 6 ~ 40,
                                TRUE ~ NA)) %>% 
  mutate(total_internet_time = video_game_time + 
           watch_time + 
           other_internet_time) %>% 
  mutate(use_smartphone = case_when(DV_010A == 1 ~ TRUE,
                                    DV_010A == 2 ~ FALSE,
                                    TRUE ~ NA)) %>% 
  mutate(use_laptop = case_when(DV_010B == 1 ~ TRUE,
                                DV_010B == 2 ~ FALSE,
                                TRUE ~ NA)) %>% 
  mutate(use_tablet = case_when(DV_010C == 1 ~ TRUE,
                                DV_010C == 2 ~ FALSE,
                                TRUE ~ NA)) %>% 
  mutate(use_desktop = case_when(DV_010D == 1 ~ TRUE,
                                 DV_010D == 2 ~ FALSE,
                                 TRUE ~ NA)) %>% 
  mutate(use_wearables = case_when(DV_010G == 1 ~ TRUE,
                                   DV_010G == 2 ~ FALSE,
                                   TRUE ~ NA)) %>% 
  mutate(is_in_school = case_when(ED_G10 == 1 ~ TRUE,
                                  ED_G10 == 2 ~ FALSE,
                                  TRUE ~ NA)) %>%
  mutate(is_employed = case_when(EMP == 1 ~ TRUE,
                                 EMP == 2 ~ FALSE,
                                 TRUE ~ NA)) %>%
  mutate(gender = case_when(GENDER == 1 ~ "male",
                            GENDER == 2 ~ "female",
                            TRUE ~ NA)) %>% 
  # remove the old columns
  select(-GENDER,
         -EMP,
         -ED_G10,
         -DV_010A,
         -DV_010B,
         -DV_010C,
         -DV_010D,
         -DV_010G,
         -UI_055A,
         -UI_060A,
         -UI_045A,
         -video_game_time,
         -watch_time,
         -other_internet_time)

# save the cleaned csv file
write.csv(newvar_df, "data/cius_2020_pumf_clean.csv", row.names = FALSE)

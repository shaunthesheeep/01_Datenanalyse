# simonbeta_clean.R
#
# Created on 02/09/23 by Stephanie Homan, stephanie dot homan at bli dot uzh dot ch
# Adapted for the Master Thesis by Sofia Michel, sofia dot michel at students dot unibe dot ch
#-----------------------------------------------------------------------

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir()
getwd()
source("simonbeta_func.R")


# Load raw data files
## Screening
df_t0_orig  <- read_excel("../preproc/simonbeta_screening_211220.xls")
## Baseline
df_t1_orig1  <- read_excel("../preproc/simonbeta_baseline_211220.xls", 1)
df_t1_orig2  <- read_excel("../preproc/simonbeta_baseline_211220.xls", 2)
df_t1_orig3  <- read_excel("../preproc/simonbeta_baseline_211220.xls", 3)
## EMA
df_ema_orig  <- read_excel("../preproc/simonbeta_ema_211220.xlsx")






#----------1. Clean screening data frame (t0)-----------------------------------

# Create new inverted variables for PSS
## reversing the scores of items 4, 5, 7, and 8
df_t0_temp <- df_t0_orig %>%
  mutate(across(c("PSS4", "PSS5", "PSS7", "PSS8"), 
                ~ dplyr::recode(.,
                                "5" = 1,
                                "4" = 2,
                                "3" = 3,
                                "2" = 4,
                                "1" = 5),
                .names = "{.col}_r"))


# Calculate total and sub-scores for all questionnaires
df_t0_sum <- df_t0_temp %>%
  rowwise() %>%
  mutate(
    # PSS Perceived helplessness subscale (PH)
    PSS_PH = sum(PSS1, PSS2, PSS3, PSS6, PSS9, PSS10, na.rm = TRUE),
    # PSS perceived self-efficacy (PSE)
    PSS_PSE = sum(PSS4, PSS5, PSS7, PSS8, na.rm = TRUE),
    # PSS_total: sum of all PH and reversed PSE items
    PSS_total = sum(PSS_PH, PSS4_r, PSS5_r, PSS7_r, PSS8_r, na.rm = TRUE)
  )


# save df
write_xlsx(df_t0_sum, "../preproc/simonbeta_t0_preproc.xlsx")


# Select only data from SIMONbeta 
## exclude data from the ReApp study
df_t0_subset <- df_t0_sum[!grepl("RE", df_t0_sum$id), ]
## exclude data from the SeApp study
df_t0_subset <- df_t0_subset[!grepl("SE", df_t0_subset$id), ]
## exclude unknown ID (RS...)
df_t0_subset <- df_t0_subset[!grepl("RS", df_t0_subset$id), ]
## drop NA
df_t0_subset <- df_t0_subset %>% 
  drop_na(id)
  

# Recode questionnaires
df_t0_temp <- df_t0_subset %>%
  mutate(gender = dplyr::recode(gender, 
                                "1" = "male", 
                                "2" = "female")) %>% 
  mutate(education = dplyr::recode(education,
                        "1" = "Bachelor",
                        "2" = "Master",
                        "3" = "PhD",
                        "4" = "other")) %>%
  mutate(student = dplyr::recode(studies,
                        "1" = "Yes",
                        "2" = "No")) %>%
  mutate(nationality = dplyr::recode(nationality,
                        "1" = "Switzerland",
                        "2" = "Germany", 
                        "3" = "Austria",
                        "4" = "other European country", 
                        "5" = "outside Europe")) %>%
  mutate(relationship = dplyr::recode(relationship,
                        "1" = "Single",
                        "2" = "Dating",
                        "3" = "In relationship, but living alone",
                        "4" = "In relationship and living with partner",
                        "5" = "Married")) %>% 
  mutate(treatment = dplyr::recode(treatment,
                        "6" = "No",
                        "7" = "Yes")) %>% 
  mutate(diagnosis = dplyr::recode(diagnosis,
                        "6" = "No",
                        "7" = "Yes"))


n_screening <- length(df_t0_temp$id)
# 96




# 2. Clean baseline data frame (t1)
#----------2. Clean baseline data frame (t1)------------------------------------

# Merge single sheets into one df
## merge sheet 1 and 2 
df_t1_orig12 <- merge(df_t1_orig1, df_t1_orig2, by = "id")
## remove empty ids
df_t1_orig12_clean <- df_t1_orig12 %>% drop_na("id")
## merge cleaned df_12 with sheet 3
df_t1_orig123   <- merge(df_t1_orig12_clean, df_t1_orig3, by = "id")
## remove empty ids
df_t1_orig <- df_t1_orig123 %>% drop_na("id")

## select all from SIMONbeta + control group from SeApp & Reapp
## RE018, RE019, RE027, RE028, RE029, RE035, RE045, RE046, 
## RE048, RE055, RE059, RE069, RE075, RE076, RE078, RE085, RE086, RE088, 
## RE095, RE096, RE106, RE107, RE108, RE109, RE117, RE118, RE127, RE136, 
## RE137, RE146, RE147, RE148, RE149, RE155, RE163, RE170, RE173, RE174, RE182
#df_t1_subset <- df_t1_orig %>% 
 # filter(SEREgroup == "include" | SEREgroup == "control")


# Select only data from SIMONbeta 
## exclude data from the ReApp study
df_t1_subset <- df_t1_orig[!grepl("RE", df_t1_orig$id), ]
## exclude data from the SeApp study
df_t1_subset <- df_t1_subset[!grepl("SE", df_t1_subset$id), ]
## exclude unknown ID (RS...)
df_t1_subset <- df_t1_subset[!grepl("RS", df_t1_subset$id), ]
## drop NA
df_t1_subset <- df_t1_subset %>% 
  drop_na(id)

n_t0_simonb <- length(unique(df_t1_subset$id))
#119



# select only variables needed
df_t1_temp <- df_t1_subset %>% 
  select(id, SEREgroup, matches("^BDI|^PHQ|^PSQ"))
  

# Recode questionnaires
df_t1_temp <- df_t1_temp %>%
  # Beck Depression Inventory (BDI)
  mutate(across(starts_with("BDI") & !ends_with("16") & !ends_with("18"), 
                ~ dplyr::recode(.,
                                "1" = 0,
                                "2" = 1,
                                "3" = 2,
                                "4" = 3))) %>%
  mutate(BDI16 = dplyr::recode(BDI16,
                               "1" = 0,
                               "2" = 1,  # 1a
                               "3" = 1,  # 1b
                               "4" = 2,  # 2a
                               "5" = 2,  # 2b
                               "6" = 3,  # 3a
                               "7" = 3)) %>% # 3b
  mutate(BDI18 = dplyr::recode(BDI18,
                               "1" = 0,
                               "2" = 1,  # 1a
                               "3" = 1,  # 1b
                               "4" = 2,  # 2a
                               "5" = 2,  # 2b
                               "6" = 3,  # 3a
                               "7" = 3)) # 3b
  # Patient Health Questionnaire (PHQ) - no recoding needed
  # Pittsburgh Sleep Quality Inventory (PSQ) - separate recoding later
  

 
# Calculate total and sub-scores for BDI and PHQ (PSQI will be calculated later)
df_t1_sum <- df_t1_temp %>%
   rowwise() %>%
   mutate(
     # BDI: sum of all items
     BDI_total = sum(BDI1, BDI2, BDI3, BDI4, BDI5, BDI6, BDI7, BDI8, 
                     BDI9, BDI10, BDI11, BDI12, BDI13, BDI14, BDI15, 
                     BDI16, BDI17, BDI18, BDI19, BDI20, BDI21, 
                     na.rm = TRUE),
     # Patient Health Questionnaire (PHQ)
     ## PHQ depression
     PHQ_depression = sum(PHQ14, PHQ15, PHQ16, PHQ17, PHQ18, PHQ19, PHQ20, 
                          PHQ21, PHQ22, na.rm = TRUE),
     # PSQI: will be treated later as manually adjustments were necessary 
     )




# 3. Clean EMA data frame 
#----------3. Clean EMA data frame----------------------------------------------

## Rename variables and create final data frame

# Rename variables
df_ema_renamed <- df_ema_orig %>%
    dplyr::rename(
    id = "ema_id",
    startdate = "startdate",
    period = period,
    # Sleep Condition Indicator items
    Sleep_latency = "EMAsleeplatency",
    Sleep_wakefullness = "EMAwakefullness[SQ001]",
    Sleep_quality = "EMAsleepquality[SQ001]",
    # Depression
    Interest = "EMAinterest[SQ001]", 
    Depressed = "EMAdepressed[SQ001]", 
    # Perceived Stress Scale (PSS)
    PSS_control = "EMApss2[SQ001]", ## ...nicht in der Lage zu sein, die wichtigen Dinge in Ihrem Leben kontrollieren zu können?
    PSS_capable = "EMApss4[SQ001]", ## ...fähig sind, ihre persönlichen Probleme zu bewältigen?
    PSS_favorable = "EMApss5[SQ001]",  ## ...Dinge zu Ihren Gunsten entwickeln?
    PSS_overwhelmed = "EMApss10[SQ001]", # ...so viele Schwierigkeiten angehäuft haben, dass Sie diese nicht überwinden konnten?
    # Passive Suicide
    Not_worth_living = "EMAworthliving[SQ001]") %>%
  tidyselect::all_of(.) %>% 
  # change names of id 
  dplyr::mutate(ema_id = str_replace_all(id, "[\r\n]" , "")) %>%
  # drop rows with no subject id
  drop_na("id") %>% 
  # only keep variables of interest 
  dplyr::select(
    id, startdate, period, 
    Sleep_latency, Sleep_wakefullness, Sleep_quality, 
    Interest, Depressed, 
    PSS_control, PSS_capable, PSS_favorable, PSS_overwhelmed, 
    Not_worth_living) %>%
  # arrange by id
  arrange(id)

  


#---
# Remove unnecessary information
## remove subjects of other studies (ReApp and SeApp)
df_ema_origfin <- df_ema_renamed[!startsWith(as.character(df_ema_renamed$id), 
                                             "3"), ]

## remove subjects of other studies (ReApp and SeApp)
df_ema_origfin <- df_ema_origfin[!startsWith(as.character(df_ema_origfin$id), 
                                             "4"), ]

## remove subjects of other studies (ReApp and SeApp)
df_ema_origfin <- df_ema_origfin[df_ema_origfin$id > 100, ]

## remove pilot subjects
df_ema_origfin <- df_ema_origfin[!startsWith(as.character(df_ema_origfin$id), 
                                             "9"), ]

## remove test run
df_ema_origfin <- df_ema_origfin[!grepl("default_code", 
                                        df_ema_origfin$id), ]

## remove test subject 290
df_ema_origfin <- df_ema_origfin[!grepl("290", 
                                        df_ema_origfin$id), ]

## remove test subject 299
df_ema_origfin <- df_ema_origfin[!grepl("299", 
                                        df_ema_origfin$id), ]

## drop rows without ID
df_ema_removed_na <- df_ema_origfin %>%
  drop_na(id)


## remove repeated data entries selecting the one with more data available
## those rows are characterized by having no value for items such as "Interest"
df_ema_removed_double <- df_ema_removed_na %>%
  drop_na(Interest)


# Add columns for time (days, weeks, hours in study)
## change character > date
df_ema_removed_double$startdate <- as.POSIXct(df_ema_removed_double$startdate)

## create columns
df_ema_time <- df_ema_removed_double %>%
  group_by(id) %>%
  arrange(id, startdate) %>%
  mutate(absmeas = cumsum(!is.na(startdate)),
         # get total amount of measurements
         totmeas = sum(!is.na(startdate)),
         # get first day of ema
         firstdate = min(startdate),
         # from that calculate the number of weeks, days, & hours
         week = floor(difftime(startdate, firstdate, unit = "weeks")) + 1,
         # calculated manually by RA
         day = floor(difftime(startdate, firstdate, unit = "days")) + 1,
         hour = floor(difftime(startdate, firstdate, unit = "hours")),
         # get total amount of days
         totdays = length(unique(day)),
         totweeks = length(unique(week))
  ) %>%
  group_by(id, day) %>%
  mutate(
    # get total amount of measurements per day
    measperday = cumsum(!is.na(day))
  ) %>%
  ungroup()

n_ema_simonb <- length(unique(df_ema_time$id))
# 104



#---
# Recode Questionnaires 

df_ema_time <- df_ema_time %>%
  # PSS: reverse coded items PSS_capable, PSS_favorable
  mutate(PSS_capable = 100 - PSS_capable,
         PSS_favorable = 100 - PSS_favorable) %>%
  
  # SCI: Sleep_latency, Sleep_wakefullness
  ## Sleep_latency: instead of __ minutes --> 0-4
  mutate(Sleep_latencyr = case_when(
    is.na(Sleep_latency) ~ NA_real_,  # Keep NAs as NA
    Sleep_latency <= 15 ~ 4,
    Sleep_latency >= 16 & Sleep_latency <= 30 ~ 3,
    Sleep_latency >= 31 & Sleep_latency <= 45 ~ 2,
    Sleep_latency >= 46 & Sleep_latency <= 60 ~ 1,
    TRUE ~ 0 )) %>%

  ## Sleep_wakefullness: : instead of 0-61 minutes --> 0-4
  mutate(Sleep_wakefullnessr = case_when(
    is.na(Sleep_latency) ~ NA_real_,  # Keep NAs as NA
    Sleep_wakefullness <= 15 ~ 4,
    Sleep_wakefullness >= 16 & Sleep_wakefullness <= 30 ~ 3,
    Sleep_wakefullness >= 31 & Sleep_wakefullness <= 45 ~ 2,
    Sleep_wakefullness >= 46 & Sleep_wakefullness <= 60 ~ 1,
    TRUE ~ 0))



#---
# Rescale values

## Perceived Stress Scale (PSS)
### EMA: 1) PSS_control, 2) PSS_capable, 3) PSS_favorable, 4) PSS_overwhelmed
### instead of 0-100 --> 1 - 5
PSS_control <- df_ema_time$PSS_control
PSS_capable <- df_ema_time$PSS_capable
PSS_favorable <- df_ema_time$PSS_favorable
PSS_overwhelmed <- df_ema_time$PSS_overwhelmed


### EMA: Not_worth_living
### instead of 0-100 --> 0 - 4
Not_worth_living <- df_ema_time$Not_worth_living


## Depression (PHQ Depression Screener)
### EMA: 1) Interest & 2) Depressed
### instead of 0-100 --> 0 - 3
Interest <- df_ema_time$Interest
Depressed <- df_ema_time$Depressed


## Sleep (SCI)
### EMA: Sleep_quality
### instead of 0-100 --> 0-4
Sleep_quality <- df_ema_time$Sleep_quality
### Sleep_latency and Sleep_wakefullness were already rescaled 


# This is called a MinMax rescale, this allows us to choose what boundary 
# we want and does a linear transformation to preserve all information.

# Orig = original vector
# lower = new lower bound
# upper = new upper bound. 


rescale_vector <- function(Orig, lower, upper) {
  # Handle NA values by removing them for the calculation
  non_na_values <- na.omit(Orig)
  # Find the minimum and maximum of the non-NA values
  min_val <- min(non_na_values)
  max_val <- max(non_na_values)
  # Rescale non-NA values
  scaled_vector <- (Orig - min_val) / (max_val - min_val) * (upper - lower) + lower
  # Retain the original NA positions
  scaled_vector[is.na(Orig)] <- NA
  return(scaled_vector)
}


# PSS_controlr
original_vector <- PSS_control # exchange for every vector that needs rescaling
new_lower <- 1
new_upper <- 5
PSS_controlr <- rescale_vector(original_vector, new_lower, new_upper)

# PSS_capabler
original_vector <- PSS_capable 
new_lower <- 1
new_upper <- 5
PSS_capabler <- rescale_vector(original_vector, new_lower, new_upper)

# PSS_favorabler
original_vector <- PSS_favorable 
new_lower <- 1
new_upper <- 5
PSS_favorabler <- rescale_vector(original_vector, new_lower, new_upper)

# PSS_overwhelmedr
original_vector <- PSS_overwhelmed
new_lower <- 1
new_upper <- 5
PSS_overwhelmedr <- rescale_vector(original_vector, new_lower, new_upper)

# Not_worth_livingr
original_vector <- Not_worth_living
new_lower <- 0
new_upper <- 4
Not_worth_livingr <- rescale_vector(original_vector, new_lower, new_upper)

# Interestr
original_vector <- Interest
new_lower <- 0
new_upper <- 3
Interestr <- rescale_vector(original_vector, new_lower, new_upper)

# Depressedr
original_vector <- Depressed
new_lower <- 0
new_upper <- 3
Depressedr <- rescale_vector(original_vector, new_lower, new_upper)

# Sleep_latencyr was already rescaled 
# Sleep_wakefullnessr was already rescaled 
# Sleep_qualityr
original_vector <- Sleep_quality
new_lower <- 0
new_upper <- 4
Sleep_qualityr <- rescale_vector(original_vector, new_lower, new_upper)



## Include again in df
df_ema_time <- cbind(df_ema_time, 
                     PSS_controlr, PSS_capabler, PSS_favorabler, PSS_overwhelmedr, 
                     Not_worth_livingr,
                     Interestr, Depressedr, 
                     # Sleep_latencyr, Sleep_wakefullnessr are alredy included in df_ema_time
                     Sleep_qualityr)



#---
# Calculate sub-scales
df_ema_sum <- df_ema_time %>% 
  rowwise() %>% 
  mutate(
    # PSS-4: score ranges 0-16 (0 best, 16 worst)
    Stress = sum(c(PSS_controlr, PSS_capabler, PSS_favorabler, PSS_overwhelmedr), na.rm = TRUE),
    # PHQ-2: score ranges from 0-6 (0 best, 6 worst); a score of 3 > major depressive disorder is likely (Kroenke et al.,2003)
    Depression = sum(c(Interestr, Depressedr), na.rm = TRUE),
    # Passive_SI: score ranges from 0-4 (0 best, 4 worst)
    Passive_SI = Not_worth_livingr,  
    # SCI: score ranges from 0-12 (0 worst, 12 best); a score of <= 6 indicating insomnia  
    Sleep = ifelse(all(complete.cases(c(Sleep_latencyr, Sleep_qualityr, Sleep_wakefullnessr))),
                   sum(c(Sleep_latencyr, Sleep_qualityr, Sleep_wakefullnessr), na.rm = TRUE),
                   NA)
  ) %>%
  ungroup() 
head(df_ema_sum)


# Write final data frame to disk
#----------4. Write final data frame to disk------------------------------------
# Screening data
write.csv(df_t0_temp, "../data/simonbeta_t0_data.csv", 
          row.names = FALSE, na = "")
write_xlsx(df_t0_temp, "../data/simonbeta_t0_data.xlsx")

# Baseline data
write.csv(df_t1_sum, "../data/simonbeta_t1_data.csv", 
          row.names = FALSE, na = "")
write_xlsx(df_t1_sum, "../data/simonbeta_t1_data.xlsx")

# EMA data
write.csv(df_ema_sum, "../data/simonbeta_ema_data.csv",
          row.names = FALSE, na = "")
write_xlsx(df_ema_sum, "../data/simonbeta_ema_data.xlsx")



#----------5. Prepare PSQI------------------------------------------------------
# Prepare PSQI 

# Upload PSQI Sheet - PSQ1, PSQ2 und PSQ4 were partially manually adapted
df_t1_PSQI <- read_xlsx("../data/simonbeta_t1_data_PSQI.xlsx")
names(df_t1_PSQI)

# recode scala (0-3)
df_t1_PSQI <- df_t1_PSQI %>%
  mutate(
    across(c(PSQ5:PSQ13, PSQ15:PSQ23), ~ dplyr::recode(., 
                                                       "1" = 0,
                                                       "2" = 1,
                                                       "3" = 2,
                                                       "4" = 3))
  )

# 1. PSQIDURAT(DURATION OF SLEEP): Minimum Score = 0 (better); Maximum Score = 3 (worse) 
df_t1_PSQI$PSQIDURAT <- with(df_t1_PSQI, ifelse(PSQ4_c >= 7, 0,
                                                ifelse(PSQ4_c < 7 & PSQ4_c >= 6, 1,
                                                       ifelse(PSQ4_c < 6 & PSQ4_c >= 5, 2, 3))))


# 2. PSQIDISTB (SLEEP DISTURBANCE): 
# Beim BSQ5J wurde nicht nach der Häufigkeit gefragt "andere Gründe"
df_t1_PSQI <- df_t1_PSQI %>%
  rowwise() %>%
  mutate(PSQIDISTB = sum(c_across(PSQ6:PSQ13), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(PSQIDISTB = case_when(
    PSQIDISTB == 0 ~ 0,
    PSQIDISTB >= 1 & PSQIDISTB <= 9 ~ 1,
    PSQIDISTB > 9 & PSQIDISTB <= 18 ~ 2,
    PSQIDISTB > 18 ~ 3
  ))


# 3. PSQILATEN (Sleep Latency):
df_t1_PSQI <- df_t1_PSQI %>%
  # Schritt 1: Bereinigung und Umkodierung von PSQ2_c
  mutate(PSQ2_new = gsub("[^0-9.,-]", "", PSQ2_c),    # Bereinigung von PSQ2_c
         PSQ2_new = as.numeric(PSQ2_new),            # Umwandlung in numerisch
         PSQ2_new = case_when(                       # Umkodierung basierend auf den Bedingungen
           PSQ2_new <= 15 ~ 0,
           PSQ2_new > 15 & PSQ2_new <= 30 ~ 1,
           PSQ2_new > 30 & PSQ2_new <= 60 ~ 2,
           PSQ2_new > 60 ~ 3
         )) %>%
  # Schritt 2: Summierung von PSQ2_new und PSQ5, Umkodierung von PSQILATEN
  rowwise() %>%                                       # Für zeilenweise Berechnung
  mutate(
    PSQILATEN = sum(PSQ5, PSQ2_new, na.rm = TRUE),   # Summierung der beiden Variablen
    PSQILATEN = case_when(                           # Umkodierung basierend auf den Bedingungen
      PSQILATEN == 0 ~ 0,
      PSQILATEN >= 1 & PSQILATEN <= 2 ~ 1,
      PSQILATEN >= 3 & PSQILATEN <= 4 ~ 2,
      PSQILATEN >= 5 & PSQILATEN <= 6 ~ 3
    )) %>%
  ungroup()  


# 4. PSQIDAYDYS (DAY DYSFUNCTION DUE TO SLEEPINESS)
df_t1_PSQI <- df_t1_PSQI %>%
  rowwise() %>%                                       # Für zeilenweise Berechnung
  mutate(
    PSQIDAYDYS = sum(PSQ17, PSQ18, na.rm = TRUE),   # Summierung der beiden Variablen
    PSQIDAYDYS = case_when(                           # Umkodierung basierend auf den Bedingungen
      PSQIDAYDYS == 0 ~ 0,
      PSQIDAYDYS >= 1 & PSQIDAYDYS <= 2 ~ 1,
      PSQIDAYDYS >= 3 & PSQIDAYDYS <= 4 ~ 2,
      PSQIDAYDYS >= 5 & PSQIDAYDYS <= 6 ~ 3
    )) %>%
  ungroup()  


# 5. PSQIHSE (SLEEP EFFICIENCY)
df_t1_PSQI <- df_t1_PSQI %>%
  # Schritt 1: Bereinigung und Umkodierung von PSQ1_c
  mutate(PSQ1_new = gsub("[^0-9.:]", "", PSQ1_c), # Bereinigung von PSQ1_c
         PSQ1_new = ifelse(PSQ1_new == "24" | PSQ1_new == "24:00" | PSQ1_new == "24.00", "00:00", PSQ1_new),  # 0 oder 00:00 auf 24 setzen       
         PSQ1_new = gsub("\\.", ":", PSQ1_new),  # Punkt in Doppelpunkt umwandeln
         PSQ1_new = ifelse(grepl(":", PSQ1_new), 
                           PSQ1_new,  # Wenn Doppelpunkt vorhanden, bleibt der Wert unverändert
                           paste0(PSQ1_new, ":00")), # Sonst füge :00 hinzu
         PSQ1_new = gsub("^(\\d):", "0\\1:", PSQ1_new))   # füge eine 0 hinzu, falls nicht zweistellig
df_t1_PSQI$PSQ1_new

#Schritt 2: Bereinigung und Umkodierung von PSQ3_c
df_t1_PSQI <- df_t1_PSQI %>% 
  mutate(PSQ3_new = gsub("[^0-9.:]", "", PSQ3_c), # Bereinigung von PSQ1_c
         PSQ3_new = gsub("\\.", ":", PSQ3_new),  # Punkt in Doppelpunkt umwandeln
         PSQ3_new = ifelse(grepl(":", PSQ3_new), 
                           PSQ3_new,  # Wenn Doppelpunkt vorhanden, bleibt der Wert unverändert
                           paste0(PSQ3_new, ":00")),  # Sonst füge :00 hinzu
         PSQ3_new = gsub("^(\\d):", "0\\1:", PSQ3_new))  

#Schritt 3: Bereinigung und Umkodierung von PSQ4_c
df_t1_PSQI <- df_t1_PSQI %>% 
  mutate(PSQ4_new = as.numeric(gsub("[^0-9.]", "", PSQ4_c)))
df_t1_PSQI$PSQ4_new

#Schritt 4: Berechnung von PSQIHSE
df_t1_PSQI <- df_t1_PSQI %>%
  mutate(
    # Konvertiere PSQ1_new und PSQ3_new in Zeiten
    PSQ1_time = strptime(paste0(PSQ1_new, ":00"), format = "%H:%M:%S"),
    PSQ3_time = strptime(paste0(PSQ3_new, ":00"), format = "%H:%M:%S"),
    
    # Berechne die Differenz in Sekunden
    Diffsec = as.numeric(difftime(PSQ3_time, PSQ1_time, units = "secs")),
    
    # Berechne die Differenz in Stunden (positive Werte für Zeitspannen über Mitternacht)
    Diffsec = ifelse(Diffsec < 0, Diffsec + 86400, Diffsec),  # Addiere 24 Stunden, wenn Differenz negativ
    Diffhour = Diffsec / 3600,  # Konvertiere in Stunden
    
    # Berechne newtib basierend auf der Bedingung
    newtib = ifelse(Diffhour > 24, Diffhour - 24, Diffhour),
    
    # Berechne tmphse
    tmphse = (PSQ4_new / newtib) * 100,
    
    # Wende die neuen Bedingungen auf tmphse an
    PSQIHSE = case_when(
      tmphse >= 85 ~ 0,       
      tmphse < 85 & tmphse >= 75 ~ 1,  
      tmphse < 75 & tmphse >= 65 ~ 2, 
      tmphse < 65 ~ 3        
    )
  )


# 6. OVERALL SLEEP QUALITY (PSQISLPQUAL)
df_t1_PSQI <- df_t1_PSQI %>% 
  mutate(PSQISLPQUAL = PSQ6)

# 7. NEED MEDS TO SLEEP (PSQINMS)
df_t1_PSQI <- df_t1_PSQI %>% 
  mutate(PSQINMS = PSQ7)

# TOTAL 
df_t1_PSQI <- df_t1_PSQI %>% 
  rowwise() %>%
  mutate(
    PSQI_Total = sum(PSQIDURAT, PSQIDISTB, PSQILATEN,PSQIDAYDYS, PSQIHSE,PSQISLPQUAL, PSQINMS)
  ) 
df_t1_PSQI$PSQI_Total


# write excel
write.csv(df_t1_PSQI, "../data/simonbeta_PSQI_data_clean.csv", 
          row.names = FALSE, na = "")
write_xlsx(df_t1_PSQI, "../data/simonbeta_PSQI_data_clean.xlsx")

#----------6. merge screening and BL--------------------------------------------
# Screening
df_t0_sum <- read_xlsx("../data/simonbeta_t0_data.xlsx")
names(df_t0_sum)

# only keep relevant variables for t0
df_t0 <- df_t0_sum %>%
  select(id, age, gender, PSS_total) #i porbably need to include treatment or diagnosis 
names(df_t0)


# Baseline
df_t1_sum <- read_xlsx("../data/simonbeta_t1_data.xlsx")
names(df_t1_sum)

# only keep relevant variables for t1
df_t1 <- df_t1_sum %>%
  select(id, PHQ_depression) # or BDI_total
names(df_t1)

# PSQI: 
df_t1_PSQI <- read_xlsx("../data/simonbeta_PSQI_data_clean.xlsx")
df_t1_PSQI_clean <- df_t1_PSQI %>%
  select(id, PSQI_Total)


# merge t0 and t1 (as both df have only one data point) 
df_bl <- merge(df_t1, df_t0, by = "id")
# merge with PSQI (PSQI was treated as manual adjustments were necessary)
df_bl <- merge(df_bl, df_t1_PSQI_clean, by = "id")
names(df_bl)

# write final baseline excel
write_xlsx(df_bl,"../data/simonbeta_bl.xlsx" )
# merge t0 and t1 (as both df have only one data point) 
df_bl <- merge(df_t1, df_t0, by = "id")
# merge with PSQI (PSQI was treated as manual adjustments were necessary)
df_bl <- merge(df_bl, df_t1_PSQI_clean, by = "id")
names(df_bl)

# write final baseline excel
write_xlsx(df_bl,"../data/simonbeta_bl.xlsx" )

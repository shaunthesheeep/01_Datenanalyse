# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir()
getwd()
source("simonbeta_func.R")

# BL 
df_bl <- read_xlsx("../data/simonbeta_bl.xlsx")
names(df_bl)[names(df_bl)=="PHQ_depression"] <- "bl_depression"


# Ema
df_ema_sum <- read_xlsx("../data/simonbeta_ema_data.xlsx")

## only keep relevant EMA data
df_ema <- df_ema_sum %>% 
  select(id, startdate, period,firstdate, day, measperday,
         Stress, Sleep, Depression, Passive_SI)
names(df_ema)


# combine bl and ema
df_combined <- merge(df_ema, df_bl, by = "id")

#---------------Hypothesis 1A) linear regression--------------------------------
# medication is omitted as i still have problems with this variable
#---
# 1. Null-Model for a linar regression 
## this mode does only contain the intercept (mean value of dependent variable), without a predictor (independent variable) 

Ar_lm_null <- lm(Depression ~ 1, 
                 data = df_combined)
summary(Ar_lm_null)

# 2. Null model for linear regression with control variables
Ar_lm_null_controlled <- lm(Depression ~ gender + bl_depression, 
                            data = df_combined)
summary(Ar_lm_null_controlled)

# 3. extended linar regression including sleep quality as predictor
Ar_lm_extended <- lm(Depression ~ gender + bl_depression + Sleep, 
                     data = df_combined)
summary(Ar_lm_extended)


#---------------Hypothesis 1A) mixed-effects model-----------------------------
# without medication as i still have issues with this variable

require(nlme)

# 1. Null-Model for a hierarchical mixed-effects model 
## this model includes only the intercept but accounts for the hierarchical structure of the data 
## includes variability between person and within person between days

Am_lmm_null <- lme(Depression ~ 1, 
                   random = ~ 1 | id/day, data = df_combined)
summary(Am_lmm_null)


# 2. Null model for hierarchical mixed-effects model with control variables
Am_lme_null_c <- lme(Depression ~ gender  + bl_depression, 
                   random = ~ 1 | id/day, data = df_combined) 
summary(Am_lme_null_c)


# 3. extended hierarchical mixed-effects model including sleep quality as predictor
## calculate daily depression mean score
{ 
  # Create daily mean for Stress and Depression
  df_combined.1 <- df_combined %>%
    group_by(id, day) %>%
    mutate(Daily_Stress = mean(Stress), na.rm = TRUE, 
           Daily_Depression = mean(Depression), na.rm = TRUE) %>%
    ungroup() %>%
    select(id,Daily_Stress, Daily_Depression, day, 
           gender, bl_depression) %>%
    unique()
  
  # Create df with Sleep and Passive_SI 
  df_combined.2 <- df_combined %>%
    select(id, day, Sleep, Passive_SI)  %>%
    group_by(id, day) %>%
    dplyr::summarize(
      Sleep = sum(Sleep, na.rm = TRUE),
      Passive_SI = sum(Passive_SI, na.rm = TRUE),
      .groups = "drop"
    )
  
  # merge both ema.1 and ema.2 
  df_combined.3 <- left_join(df_combined.1, df_combined.2, by = c('id', 'day'))
}

Am_lme_extended <- lme(Daily_Depression ~ gender + bl_depression + Sleep, 
                       random = ~ 1 | id/day, data = df_combined.3)
summary(Am_lme_extended)


# 4. extended hierarchical mixed-effects model with random slope
Am_lme_extended_rs <- lme(Daily_Depression ~ gender + bl_depression + Sleep, 
                          random = ~ Sleep | id/day, data = df_combined.3)

summary(Am_lme_extended_rs)
#---



#---------------Hypothesis 1B) -------------------------------------------------
# Null-Model for Hypothsis 1B) 
# without medication as i still have issues with this variable

require(nlme)

# make sure that variables which were not measured several times a day are correctly listed 
{
  # variables relevant on daily level 
  df_daily <- df_combined %>%
    select(id, day, Sleep,Passive_SI, gender, bl_depression) %>%
    group_by(id, day) %>%
    dplyr::summarize(Sleep = sum(Sleep, na.rm = TRUE),
                     Passive_SI = sum(Passive_SI, na.rm = TRUE),
                     gender = first(gender),
                     bl_depression = first(bl_depression),
                     .groups = "drop")
  
  
  # timpoint specific variables
  df_timepoints <- df_combined %>%
    select(id, day, measperday, Depression, Stress)
  
  # merge both df 
  df_combined_clean <- df_timepoints %>%
    left_join(df_daily, by = c("id", "day"))
  
  } 

# 1. Null-Model for a hierarchical mixed-effects model 
## this model includes only the intercept but accounts for the hierarchical structure of the data
## includes variability between person and within person between days
Bm_lme_null <- lme(Depression ~ 1, 
                   random = ~ 1 | id/day, 
                   data = df_combined)
summary(Bm_lme_null)

# 2. Null-Model for a hierarchical mixed-effects model accounting for timepoints
Bm_lme_null_t <- lme(Depression ~ measperday, 
                     random = ~ 1 | id/day, 
                     data = df_combined)
summary(Bm_lme_null_t)

# 3. Extended model with control variables
Bm_lme_null_time_tc <- lme(Depression ~ measperday + gender + bl_depression, 
                           random = ~ 1 | id/day, 
                           data = df_combined)
summary(Bm_lme_null_time_tc)

# 4. Extended model with sleep as predictor
Bm_lme_extended <- lme(Depression ~ measperday + gender + bl_depression + Sleep, 
                       random = ~ 1 | id/day, 
                       data = df_combined_clean)
summary(Bm_lme_extended)

# 5. Extended model with interaction between Sleep and measperday
Bm_lme_extended_interaction <- lme(Depression ~ measperday * Sleep + gender + bl_depression, 
                                   random = ~ 1 | id/day, 
                                   data = df_combined_clean)
summary(Bm_lme_extended_interaction)

#---
# visualisation
require(ggplot2)
ggplot(df_combined_clean, aes(x = measperday, y = Depression, color = Sleep)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", aes(group = Sleep), se = FALSE) +
  geom_smooth(method = "lm", color = "red", lwd = 1, se = FALSE) +
  labs(title = "Effect of Sleep Quality on Depression Throughout the Day",
       x = "Measurement Timepoints (measperday)",
       y = "Depression Score") +
  theme_minimal()


#---------------Hypothesis 2C -------------------------------------------------


# 1. Null-Model: intercept only, with hirarchical structure (person/day)
Cm_lme_null <- lme(Depression ~ 1, 
                     random = ~ 1 | id/day, 
                     data = df_combined_clean)
summary(Cm_lme_null)


# 2. include control variables 
Cm_lme_null_c <- lme(Depression ~ gender + bl_depression, 
                             random = ~ 1 | id/day, 
                             data = df_combined_clean)
summary(Cm_lme_null_c)


# 3. Moderations Analysis 
Cm_lme_moderation_full <- lme(Depression ~ Stress * Sleep + gender + bl_depression, 
                           random = ~ 1 | id/day, 
                           data = df_combined_clean)
summary(Cm_lme_moderation_full)


#---


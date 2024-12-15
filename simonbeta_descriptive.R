# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir()
getwd()
source("simonbeta_func.R")

# BL and Screening
df_bl <- read_xlsx("../data/simonbeta_bl.xlsx")

# Ema
df_ema_sum <- read_xlsx("../data/simonbeta_ema_data.xlsx")


# ----------------BL Characteristics Table--------------------------------------
# Baseline Characteristics Table 
library(gtsummary)
library(gt)


# make sure that variables are treated as numeric 
df_bl <- df_bl %>%
  mutate(
    PHQ_depression = as.numeric(PHQ_depression),
    PSS_total = as.numeric(PSS_total),
    PSQI_Total = as.numeric(PSQI_Total),
    age = as.numeric(age)
  )

# Create table
tbl1 <- df_bl %>%
  select(age, PSQI_Total, PHQ_depression,PSS_total, gender) %>%
  gtsummary::tbl_summary(
    by = gender, 
    # Define labels for the variables
    label = list(
      age ~ "Age", 
      PHQ_depression ~ "Depression Score (PHQ-9)", 
      PSS_total ~ "Perceived Stress (PSS-10)",
      PSQI_Total ~ "Sleep Quality (PSQI)" 
     ), 
    # Define variable types
    type = list(
      age ~ "continuous",
      PHQ_depression ~ "continuous",
      PSS_total ~ "continuous",
      PSQI_Total ~ "continuous"
      ), 
    statistic = all_continuous() ~ "{mean} ({sd})"
  ) %>%
  
  # More details
  add_overall() %>%
  modify_caption("**Table 1.** Baseline characteristics") %>%
  bold_labels() 


# Convert to gt table
tbl1_gt <- tbl1 %>%
  as_gt()

tbl1_gt <- tbl1_gt %>%
  # change font
  gt::tab_options(table.font.names = "Times New Roman")  %>%
  # Add a note at the bottom of the table in APA style
  gt::tab_footnote(
    footnote = htmltools::HTML("<i>Note</i> . 
    PSQI with scores ranging from 0 (better) to 21 (worse) with a score > 5 indicating poor sleep quality. 
    PHQ-9 with scores ranging from 9 (better) to 36 (worse). 
    PSS-10 with scores ranging from 10 (better) to 50 (worse)"),
    locations = cells_body(columns = everything()))
tbl1_gt

# save file as html
tbl1_gt %>%
  gt::gtsave(filename = "../tables/baseline_characteristics.html")




#-----------------BL Normativity------------------------------------------------
#Baseline Normalverteilung
library(nortest)

variables <- df_bl %>%
  select(PHQ_depression,PSS_total, PSQI_Total, age)

# Verzeichnis für die Dateien
output_dir <- "../tables/tables_bl/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Ergebnisse initialisieren
results <- list()

# Schleife über die Spalten im DataFrame
for (var_name in colnames(variables)) {
  cat("\n===== Ergebnisse für Variable:", var_name, "=====\n")
  
  # Zugriff auf die jeweilige Spalte
  data_column <- variables[[var_name]]
  
  # 1. Kolmogorov-Smirnov-Test (mit Lilliefors-Korrektur)
  cat("Kolmogorov-Smirnov-Test (mit Lilliefors-Korrektur):\n")
  ks_result <- lillie.test(data_column)
  print(ks_result)
  results[[paste0(var_name, "_ks")]] <- ks_result
  
  # 2. Shapiro-Wilk-Test
  cat("\nShapiro-Wilk-Test:\n")
  sw_result <- shapiro.test(data_column)
  print(sw_result)
  results[[paste0(var_name, "_shapiro")]] <- sw_result
  
  
  # 3. Histogramm speichern
  hist_file <- file.path(output_dir, paste0(var_name, "_histogram_with_normal_curve.png"))
  png(hist_file)
  hist(data_column, breaks = 10, probability = TRUE, 
       main = paste("Histogramm für", var_name),
       xlab = var_name, col = "skyblue", border = "blue")
  curve(dnorm(x, mean = mean(data_column, na.rm = TRUE), 
              sd = sd(data_column, na.rm = TRUE)), 
        add = TRUE, col = "black", lwd = 2)
  dev.off()
  
  # 4. Dichteplot speichern
  density_file <- file.path(output_dir, paste0(var_name, "_density.png"))
  png(density_file)
  plot(density(data_column, na.rm = TRUE), 
       main = paste("Dichteplot für", var_name), 
       xlab = var_name, 
       ylab = "Dichte")
  dev.off()
  
  # 5. QQ-Plot speichern
  qqplot_file <- file.path(output_dir, paste0(var_name, "_qqplot.png"))
  png(qqplot_file)
  qqnorm(data_column, main = paste("QQ-Plot für", var_name))
  qqline(data_column, col = "skyblue")
  dev.off()
}

# Fazit: Alle BL Variablen sind nicht Normalverteilt 


#-----------------EMA-----------------------------------------------------------

# only keep relevant EMA data
df_ema <- df_ema_sum %>% 
  select(id, startdate, period,firstdate, day, measperday,
         Stress, Sleep, Depression, Passive_SI)
names(df_ema)

# Create daily mean for Stress and Depression
ema.1 <- df_ema %>%
  group_by(id, day) %>%
  mutate(Daily_Stress = mean(Stress), na.rm = TRUE, 
         Daily_Depression = mean(Depression), na.rm = TRUE) %>%
  ungroup() %>%
  select(id,Daily_Stress, Daily_Depression, day) %>%
  unique()

# Create df with Sleep and Passive_SI 
ema.2 <- df_ema %>%
  select(id, day, Sleep, Passive_SI)  %>%
  group_by(id, day) %>%
  dplyr::summarize(
    Sleep = sum(Sleep, na.rm = TRUE),
    Passive_SI = sum(Passive_SI, na.rm = TRUE),
    .groups = "drop"
  )
 

# merge both ema.1 and ema.2 
ema.3 <- left_join(ema.1, ema.2, by = c('id', 'day'))



# Plot of daily Scores Over Time for Each Participant
ema_plt_overview <- ema.3 %>%
  # Skalierung nur der numerischen Variablen
  mutate(across(c(Sleep, Passive_SI, Daily_Stress, Daily_Depression), scale)) %>% 
  # Pivot_longer
  pivot_longer(cols = c("Sleep", "Passive_SI", "Daily_Stress", 
                        "Daily_Depression"), 
               names_to = "Scales", values_to = "Scores") %>% 
  # Create Plot
  ggplot(aes(x = day, y = Scores, color = Scales)) + 
  geom_point(na.rm = TRUE) +
  labs(
    title = "Daily Scores Over Time for Each Participant",
    x = "Time (days)",
    y = "Scale Scores"
  ) +
  facet_wrap(~ id, ncol = 8) + 
  theme_bw() 

# Save Plot
ggsave("../tables/daily_scores_plot.png", ema_plt_overview, 
       width = 40, height = 80, dpi = 300, limitsize = FALSE)


#---



#------------------------------------------------------------------

















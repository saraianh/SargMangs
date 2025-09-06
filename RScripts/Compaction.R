# Substrate Compaction/Decomposition Analysis
# Author: Sarai Hutchinson
# Date: 9/6/2025

# Methodology was flawed so I chose not to use the quantitative information. I suggest to use visual markers within the pots to better track compaction.

# Load required libraries
library(tidyverse)
library(broom)

# Color palettes
okabe_ito <- c("#009E73", "#D55E00", "#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#CC79A7")

surv <- readr::read_csv("C:\\Users\\sarai\\OneDrive\\Email attachments\\Documents\\UVI\\Thesis.Work\\SargMangs\\OutputData\\survivors_tp0-tp6.csv",
                        guess_max = 10000, show_col_types = FALSE) %>%
  clean_names()

# Process the surv dataframe
surv <- surv %>%
  mutate(
    treatment_name = factor(
      treatment_name,
      levels = c("Soil", "Crushed glass", "25% SG", "75% SG", "100% Sargassum (SG)") # ordered from least to most Sargassum
    )
  )

# Coerce to numeric safely
num_cols <- c("rhma_height_1","rhma_height_2","rhma_epi_length",
              "lara_height_1","lara_height_2","number_branches")
num_cols <- intersect(num_cols, names(surv))

surv <- surv %>%
  mutate(
    across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))),
    time_period = suppressWarnings(as.numeric(time_period)),
    # Fix date parsing - use mdy() instead of ymd() for MM/DD/YYYY format
    monitor_date = suppressWarnings(mdy(date_monitored)),
    uploaded_date = suppressWarnings(mdy(date_uploaded))
  )

# Assuming your data is loaded as 'surv'
# Filter to final time period (TP6) only - adjust time_period value as needed
final_data <- surv %>%
  filter(time_period == 6) %>%  # Adjust this to your final time period
  # Filter for pots with valid height measurements
  filter(
    (species == "RHMA" & !is.na(rhma_height_1) & !is.na(rhma_height_2)) |
      (species == "LARA" & !is.na(lara_height_1) & !is.na(lara_height_2))
  )

# Calculate substrate metrics for each pot
substrate_analysis <- final_data %>%
  mutate(
    # Calculate current substrate height for each species
    # Current substrate height = Height from pot rim to substrate surface
    # For RHMA: substrate height = rhma_height_1 - rhma_height_2 
    #   (substrate to base) - (pot to base) = pot to substrate
    # For LARA: substrate height = lara_height_1 - lara_height_2
    #   (substrate to top) - (pot to top) = pot to substrate
    current_substrate_height_cm = case_when(
      species == "RHMA" & !is.na(rhma_height_1) & !is.na(rhma_height_2) ~ 
        rhma_height_1 - rhma_height_2,
      species == "LARA" & !is.na(lara_height_1) & !is.na(lara_height_2) ~ 
        lara_height_1 - lara_height_2,
      TRUE ~ NA_real_
    ),
    
    # Convert to inches
    current_substrate_height_in = current_substrate_height_cm / 2.54,
    
    # Calculate substrate change from initial 7 inches
    initial_substrate_height_in = 7,
    substrate_change_in = initial_substrate_height_in - current_substrate_height_in,
    substrate_change_cm = substrate_change_in * 2.54,
    
    # Calculate percentage change
    substrate_change_pct = (substrate_change_in / initial_substrate_height_in) * 100,
    
    # Calculate monthly rate (over 6 months)
    monthly_rate_in = substrate_change_in / 6,
    monthly_rate_cm = substrate_change_cm / 6
  )

# Summary statistics by treatment (averaged within replicates)
replicate_summaries <- substrate_analysis %>%
  group_by(treatment_name, replicate, species) %>%
  summarise(
    n_pots = n(),
    mean_substrate_change_in = mean(substrate_change_in, na.rm = TRUE),
    mean_substrate_change_cm = mean(substrate_change_cm, na.rm = TRUE),
    mean_substrate_change_pct = mean(substrate_change_pct, na.rm = TRUE),
    mean_monthly_rate_in = mean(monthly_rate_in, na.rm = TRUE),
    mean_monthly_rate_cm = mean(monthly_rate_cm, na.rm = TRUE),
    .groups = 'drop'
  )

# Average across species within each replicate (as you requested)
replicate_summaries_combined <- replicate_summaries %>%
  group_by(treatment_name, replicate) %>%
  summarise(
    total_pots = sum(n_pots),
    substrate_change_in = mean(mean_substrate_change_in, na.rm = TRUE),
    substrate_change_cm = mean(mean_substrate_change_cm, na.rm = TRUE),
    substrate_change_pct = mean(mean_substrate_change_pct, na.rm = TRUE),
    monthly_rate_in = mean(mean_monthly_rate_in, na.rm = TRUE),
    monthly_rate_cm = mean(mean_monthly_rate_cm, na.rm = TRUE),
    .groups = 'drop'
  )

# Final treatment summaries (n=4 replicates per treatment)
treatment_summaries <- replicate_summaries_combined %>%
  group_by(treatment_name) %>%
  summarise(
    n_replicates = n(),
    total_pots = sum(total_pots),
    
    # Mean substrate change
    mean_change_in = mean(substrate_change_in),
    se_change_in = sd(substrate_change_in) / sqrt(n()),
    
    mean_change_cm = mean(substrate_change_cm),
    se_change_cm = sd(substrate_change_cm) / sqrt(n()),
    
    mean_change_pct = mean(substrate_change_pct),
    se_change_pct = sd(substrate_change_pct) / sqrt(n()),
    
    # Mean monthly rate
    mean_rate_in_month = mean(monthly_rate_in),
    se_rate_in_month = sd(monthly_rate_in) / sqrt(n()),
    
    mean_rate_cm_month = mean(monthly_rate_cm),
    se_rate_cm_month = sd(monthly_rate_cm) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  # Rank by amount of change
  arrange(desc(mean_change_in)) %>%
  mutate(rank_change = row_number())

# Display results
print("=== SUBSTRATE COMPACTION/DECOMPOSITION RESULTS ===")
print(treatment_summaries)

# Create summary table for presentation
results_table <- treatment_summaries %>%
  select(
    Treatment = treatment_name,
    `Rank` = rank_change,
    `Mean Change (in)` = mean_change_in,
    `SE (in)` = se_change_in,
    `Mean Change (%)` = mean_change_pct,
    `SE (%)` = se_change_pct,
    `Rate (in/month)` = mean_rate_in_month,
    `SE Rate` = se_rate_in_month
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

print("\n=== FORMATTED RESULTS TABLE ===")
print(results_table)

# Statistical analysis - One-way ANOVA
anova_model <- aov(substrate_change_in ~ treatment_name, data = replicate_summaries_combined)
anova_results <- tidy(anova_model)

print("\n=== ANOVA RESULTS ===")
print(anova_results)

# Post-hoc test if significant
if(anova_results$p.value[1] < 0.05) {
  posthoc_results <- TukeyHSD(anova_model)
  print("\n=== POST-HOC TUKEY HSD RESULTS ===")
  print(posthoc_results)
}

# Define custom aesthetics (add your okabe_ito palette first if not defined)
# okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Define shapes for each Treatment_Name (match your factor levels)
shape_vals <- c(
  "Soil"                = 0,  # open square
  "Crushed glass"       = 17, # filled triangle
  "100% Sargassum (SG)" = 15, # filled square
  "25% SG"              = 18, # filled diamond
  "75% SG"              = 8   # asterisk
)

# Uses your okabe_ito vector (7 colors). Pick 5 distinct hues.
color_vals <- c(
  "Soil"                = okabe_ito[3], # green  (#009E73)
  "Crushed glass"       = okabe_ito[5], # blue   (#0072B2)
  "25% SG"              = okabe_ito[1], # orange (#E69F00)
  "75% SG"              = okabe_ito[6], # vermil (#D55E00)
  "100% Sargassum (SG)" = okabe_ito[7]  # purple (#CC79A7)
)

# Visualization
library(ggplot2)

# Plot 1: Mean substrate change by treatment with custom colors
p1 <- ggplot(treatment_summaries, aes(x = reorder(treatment_name, -mean_change_in), 
                                      y = mean_change_in,
                                      fill = treatment_name)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_change_in - se_change_in, 
                    ymax = mean_change_in + se_change_in),
                width = 0.2, color = "black") +
  scale_fill_manual(values = color_vals) +
  labs(title = "Substrate Change by Treatment (6 months)",
       subtitle = "Higher values = more compaction/decomposition",
       x = "Treatment",
       y = "Mean Change from Initial (inches)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove legend since x-axis labels are sufficient

# Plot 2: Individual replicate data points with custom aesthetics
p2 <- ggplot(replicate_summaries_combined, aes(x = treatment_name, y = substrate_change_in,
                                               color = treatment_name, fill = treatment_name)) +
  geom_jitter(aes(shape = treatment_name), width = 0.2, alpha = 0.7, size = 3) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "black", shape = 21) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black", linewidth = 1) +
  scale_color_manual(values = color_vals) +
  scale_fill_manual(values = color_vals) +
  scale_shape_manual(values = shape_vals) +
  labs(title = "Substrate Change: Individual Replicates",
       subtitle = "Black points show means ± SE, colored points show individual replicates",
       x = "Treatment",
       y = "Substrate Change (inches)",
       color = "Treatment",
       fill = "Treatment",
       shape = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

# Plot 3: Alternative scatter plot showing treatment ranking
p3 <- ggplot(treatment_summaries, aes(x = rank_change, y = mean_change_in,
                                      color = treatment_name, shape = treatment_name)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_change_in - se_change_in, 
                    ymax = mean_change_in + se_change_in),
                width = 0.1) +
  geom_text(aes(label = treatment_name), hjust = -0.1, vjust = 0) +
  scale_color_manual(values = color_vals) +
  scale_shape_manual(values = shape_vals) +
  scale_x_continuous(breaks = 1:5, labels = paste("Rank", 1:5)) +
  labs(title = "Treatment Ranking by Substrate Change",
       subtitle = "Rank 1 = Most compaction/decomposition",
       x = "Rank",
       y = "Mean Substrate Change (inches)") +
  theme_minimal() +
  theme(legend.position = "none")

# Display plots
print(p1)
print(p2)

# Export results
# write_csv(treatment_summaries, "substrate_treatment_summaries.csv")
# write_csv(replicate_summaries_combined, "substrate_replicate_data.csv")
# 
# print("\n=== ANALYSIS COMPLETE ===")
# print("Results saved to CSV files")


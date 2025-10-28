# ============================================================================
# MANGROVE SUBSTRATE STUDY - COMPLETE STATISTICAL ANALYSIS
# Dissolved Oxygen Analysis: Pore vs Tank Water, Species Comparison
# ============================================================================

# Load required packages
library(tidyverse)   # Data manipulation and ggplot2
library(car)         # For Levene's test
library(emmeans)     # For post-hoc comparisons
library(multcomp)    # For compact letter display
library(patchwork)   # For combining plots
library(scales)      # For plot formatting

# Set theme for all plots
theme_set(theme_minimal(base_size = 12))

# ============================================================================
# 1. DATA PREPARATION
# ============================================================================

# Load the dataset
data_path <- "C:\\Users\\sarai\\OneDrive\\Email attachments\\Documents\\UVI\\Thesis.Work\\SargMangs\\Data/Dissolved_Oxygen_Data.csv"
data  <- readr::read_csv(data_path, show_col_types = FALSE)

# Add substrate names
data <- data %>%
  mutate(
    DO = `Dissolved Oxygen (mg/L)`,
    Oxygen_Status = `Oxygen Status`,
    Substrate = case_when(
      Treatment == "A" ~ "SOIL",
      Treatment == "B" ~ "GLASS",
      Treatment == "C" ~ "100% SG",
      Treatment == "D" ~ "25% SG",
      Treatment == "E" ~ "75% SG"
    ),
    Substrate = factor(Substrate, levels = c("SOIL", "GLASS", "25% SG", "75% SG", "100% SG"))
  )

# Separate pore and tank data
pore_data <- data %>% filter(Location == "Pore")
tank_data <- data %>% filter(Location == "Tank")

# ============================================================================
# 2. DESCRIPTIVE STATISTICS
# ============================================================================

cat("\n============================================================\n")
cat("DESCRIPTIVE STATISTICS\n")
cat("============================================================\n\n")

# Overall by location
cat("Overall by Location:\n")
data %>%
  group_by(Location) %>%
  summarise(
    n = n(),
    Mean = mean(DO),
    SD = sd(DO),
    Min = min(DO),
    Max = max(DO),
    .groups = "drop"
  ) %>%
  print()

# Pore water by substrate
cat("\n\nPore Water by Substrate:\n")
pore_summary <- pore_data %>%
  group_by(Substrate, Species) %>%
  summarise(
    n = n(),
    Mean = mean(DO),
    SD = sd(DO),
    CV = (sd(DO)/mean(DO))*100,
    Min = min(DO),
    Max = max(DO),
    Hypoxic = sum(DO < 2.0),
    .groups = "drop"
  )
print(pore_summary)

# Tank water by substrate
cat("\n\nTank Water by Substrate:\n")
tank_summary <- tank_data %>%
  group_by(Substrate) %>%
  summarise(
    n = n(),
    Mean = mean(DO),
    SD = sd(DO),
    Min = min(DO),
    Max = max(DO),
    .groups = "drop"
  )
print(tank_summary)

# By substrate and species (pore water only)
cat("\n\nPore Water by Substrate and Species:\n")
species_summary <- pore_data %>%
  group_by(Substrate, Species) %>%
  summarise(
    n = n(),
    Mean = mean(DO),
    SD = sd(DO),
    Min = min(DO),
    Max = max(DO),
    Hypoxic = sum(DO < 2.0),
    .groups = "drop"
  )
print(species_summary)

# ============================================================================
# 3. ASSUMPTION TESTING - PORE WATER
# ============================================================================

cat("\n\n============================================================\n")
cat("ASSUMPTION TESTING - PORE WATER\n")
cat("============================================================\n\n")

# Normality tests by group (Shapiro-Wilk)
cat("Shapiro-Wilk Normality Tests:\n")
normality_results <- pore_data %>%
  group_by(Substrate) %>%
  summarise(
    W = shapiro.test(DO)$statistic,
    p_value = shapiro.test(DO)$p.value,
    .groups = "drop"
  )
print(normality_results)

# Levene's test for homogeneity of variance
cat("\n\nLevene's Test for Homogeneity of Variance:\n")
levene_pore <- leveneTest(DO ~ Substrate, data = pore_data)
print(levene_pore)

# Variance ratio
pore_variances <- pore_data %>%
  group_by(Substrate) %>%
  summarise(Variance = var(DO), .groups = "drop")
var_ratio_pore <- max(pore_variances$Variance) / min(pore_variances$Variance)
cat("\nVariance Ratio (max/min):", round(var_ratio_pore, 2), "\n")
cat("Rule of thumb: Should be < 4.0\n")
if(var_ratio_pore < 4) {
  cat("✓ Homogeneity assumption satisfied\n")
} else {
  cat("⚠ Borderline - consider non-parametric test\n")
}

# ============================================================================
# 4. ANALYSIS 1: PORE WATER (One-way ANOVA)
# ============================================================================

cat("\n\n============================================================\n")
cat("ANALYSIS 1: PORE WATER (Root Zone)\n")
cat("============================================================\n\n")

# One-way ANOVA
pore_model <- aov(DO ~ Substrate, data = pore_data)
pore_anova <- summary(pore_model)
print(pore_anova)

# Effect size (eta-squared)
ss_total <- sum((pore_data$DO - mean(pore_data$DO))^2)
ss_substrate <- sum(pore_anova[[1]]$`Sum Sq`[1])
eta_squared_pore <- ss_substrate / ss_total
cat("\nEffect size (η²):", round(eta_squared_pore, 3))
if(eta_squared_pore > 0.14) {
  cat(" (Large effect)\n")
} else if(eta_squared_pore > 0.06) {
  cat(" (Medium effect)\n")
} else {
  cat(" (Small effect)\n")
}

# Post-hoc tests (Tukey HSD)
cat("\n\nPost-hoc Comparisons (Tukey HSD):\n")
pore_emm <- emmeans(pore_model, ~ Substrate)
pore_posthoc <- pairs(pore_emm, adjust = "tukey")
print(pore_posthoc)

# Get compact letter display for pore water
cat("\n\nCompact Letter Display (Pore Water):\n")
pore_cld <- cld(pore_emm, alpha = 0.05, Letters = letters, adjust = "tukey")
pore_cld_df <- as.data.frame(pore_cld) %>%
  dplyr::select(Substrate, .group) %>%
  mutate(.group = str_trim(.group))
print(pore_cld_df)

# Add letters to pore summary
pore_summary <- pore_summary %>%
  left_join(pore_cld_df, by = "Substrate")
# Kruskal-Wallis (non-parametric alternative)
cat("\n\nKruskal-Wallis Test (non-parametric confirmation):\n")
kruskal_pore <- kruskal.test(DO ~ Substrate, data = pore_data)
print(kruskal_pore)

# ============================================================================
# 5. ANALYSIS 2: TANK WATER (One-way ANOVA)
# ============================================================================

cat("\n\n============================================================\n")
cat("ANALYSIS 2: TANK WATER (Bulk Water)\n")
cat("============================================================\n\n")

# Levene's test for tank water
cat("Levene's Test:\n")
levene_tank <- leveneTest(DO ~ Substrate, data = tank_data)
print(levene_tank)

# One-way ANOVA
tank_model <- aov(DO ~ Substrate, data = tank_data)
tank_anova <- summary(tank_model)
print(tank_anova)

# Effect size
ss_total_tank <- sum((tank_data$DO - mean(tank_data$DO))^2)
ss_substrate_tank <- sum(tank_anova[[1]]$`Sum Sq`[1])
eta_squared_tank <- ss_substrate_tank / ss_total_tank
cat("\nEffect size (η²):", round(eta_squared_tank, 3))
if(eta_squared_tank > 0.14) {
  cat(" (Large effect)\n")
} else if(eta_squared_tank > 0.06) {
  cat(" (Medium effect)\n")
} else {
  cat(" (Small effect)\n")
}

# Get compact letter display for tank water (if significant)
tank_emm <- emmeans(tank_model, ~ Substrate)
tank_cld <- cld(tank_emm, alpha = 0.05, Letters = letters, adjust = "tukey")
tank_cld_df <- as.data.frame(tank_cld) %>%
  dplyr::select(Substrate, .group) %>%
  mutate(.group = str_trim(.group))

# Add letters to tank summary
tank_summary <- tank_summary %>%
  left_join(tank_cld_df, by = "Substrate")

# Kruskal-Wallis
cat("\n\nKruskal-Wallis Test:\n")
kruskal_tank <- kruskal.test(DO ~ Substrate, data = tank_data)
print(kruskal_tank)

# ============================================================================
# 6. SPECIES COMPARISON (Two-way ANOVA)
# ============================================================================

cat("\n\n============================================================\n")
cat("SPECIES COMPARISON: Two-Way ANOVA\n")
cat("============================================================\n\n")

# Two-way ANOVA: Substrate × Species
species_model <- aov(DO ~ Substrate * Species, data = pore_data)
species_anova <- summary(species_model)
print(species_anova)

# Effect sizes
ss_total_species <- sum((pore_data$DO - mean(pore_data$DO))^2)
ss_substrate_sp <- species_anova[[1]]$`Sum Sq`[1]
ss_species <- species_anova[[1]]$`Sum Sq`[2]
ss_interaction <- species_anova[[1]]$`Sum Sq`[3]

cat("\n\nEffect Sizes:\n")
cat("Substrate η²:", round(ss_substrate_sp/ss_total_species, 3), "\n")
cat("Species η²:", round(ss_species/ss_total_species, 3), "\n")
cat("Interaction η²:", round(ss_interaction/ss_total_species, 3), "\n")

# Get compact letter display for each species separately
species_emm <- emmeans(species_model, ~ Substrate | Species)
species_cld <- cld(species_emm, alpha = 0.05, Letters = letters, adjust = "tukey")
species_cld_df <- as.data.frame(species_cld) %>%
  dplyr::select(Substrate, Species, .group) %>%
  mutate(.group = str_trim(.group))

# Add letters to species summary
species_summary <- species_summary %>%
  left_join(species_cld_df, by = c("Substrate", "Species"))

# ============================================================================
# 7. SEPARATE ANALYSES BY SPECIES
# ============================================================================

cat("\n\n============================================================\n")
cat("SEPARATE ANALYSES BY SPECIES\n")
cat("============================================================\n\n")

# LARA only
cat("LARA (Laguncularia racemosa):\n")
lara_data <- pore_data %>% filter(Species == "LARA")
lara_model <- aov(DO ~ Substrate, data = lara_data)
lara_anova <- summary(lara_model)
print(lara_anova)

ss_total_lara <- sum((lara_data$DO - mean(lara_data$DO))^2)
ss_substrate_lara <- lara_anova[[1]]$`Sum Sq`[1]
eta_squared_lara <- ss_substrate_lara / ss_total_lara
cat("Effect size (η²):", round(eta_squared_lara, 3), "\n\n")

# RHMA only
cat("RHMA (Rhizophora mangle):\n")
rhma_data <- pore_data %>% filter(Species == "RHMA")
rhma_model <- aov(DO ~ Substrate, data = rhma_data)
rhma_anova <- summary(rhma_model)
print(rhma_anova)

ss_total_rhma <- sum((rhma_data$DO - mean(rhma_data$DO))^2)
ss_substrate_rhma <- rhma_anova[[1]]$`Sum Sq`[1]
eta_squared_rhma <- ss_substrate_rhma / ss_total_rhma
cat("Effect size (η²):", round(eta_squared_rhma, 3), "\n")

# ============================================================================
# 8. VISUALIZATIONS
# ============================================================================

cat("\n\n============================================================\n")
cat("GENERATING VISUALIZATIONS\n")
cat("============================================================\n\n")

# Color palette
substrate_colors <- c("SOIL" = "#009E73", "GLASS" = "#0072B2", 
                     "25% SG" = "#E69F00", "75% SG" = "#D55E00", 
                     "100% SG" = "#CC79A7")

# -------- PLOT 1: Pore vs Tank Water Comparison --------
p1_pore <- ggplot(pore_summary, aes(x = Substrate, y = Mean, fill = Substrate)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.25, linewidth = 0.8) +
  geom_text(aes(label = .group, y = Mean + SD + 0.3), 
            size = 5, fontface = "bold") +
  scale_fill_manual(values = substrate_colors) +
  scale_y_continuous(limits = c(0, 5.5), expand = c(0, 0)) +
  labs(title = "Pore Water (Root Zone)",
       subtitle = "F(4,35) = 4.67, p < 0.01**, η² = 0.348",
       x = NULL, y = "Dissolved Oxygen (mg/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2))
p1_pore

p1_tank <- ggplot(tank_summary, aes(x = Substrate, y = Mean, fill = Substrate)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.25, linewidth = 0.8) +
  geom_text(aes(label = .group, y = Mean + SD + 0.3), 
            size = 5, fontface = "bold") +
  scale_fill_manual(values = substrate_colors) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
  labs(title = "Tank Water",
       subtitle = "F(4,15) = 0.358, p > 0.05, η² = 0.087",
       x = NULL, y = "Dissolved Oxygen (mg/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "#3b82f6", fill = NA, linewidth = 2))
p1_tank
# Combine pore and tank plots
pore_tank_plot <- p1_pore + p1_tank +
  plot_annotation(
    theme = theme(plot.title = element_text(size = 16, face = "bold")),
    tag_levels = "A"
  )
pore_tank_plot

ggsave("pore_vs_tank_comparison.png", pore_tank_plot, 
       width = 12, height = 6, dpi = 300)
cat("Saved: pore_vs_tank_comparison.png\n")

#----------Species-Specific Visualizations---------------
# Helper function for consistency
plot_do_species <- function(df, species_code, title_prefix, y_limits, border_col, subtitle, ylab) {
  ggplot(df %>% dplyr::filter(Species == species_code),
         aes(x = Substrate, y = Mean, fill = Substrate)) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  width = 0.25, linewidth = 0.8) +
    geom_text(aes(label = .group, y = Mean + SD + diff(y_limits)*0.05),
              size = 5, fontface = "bold") +
    scale_fill_manual(values = substrate_colors) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    labs(title = paste0(title_prefix, " - ", species_code),
         subtitle = subtitle, x = NULL, y = ylab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          plot.title = element_text(face = "bold"),
          panel.grid.major.x = element_blank(),
          panel.border = element_rect(color = border_col, fill = NA, linewidth = 2))
}

# Pore water
pore_LARA <- plot_do_species(pore_summary, "LARA",
                             title_prefix = "Pore Water",
                             y_limits = c(0, 5.5),
                             border_col = "black",
                             subtitle = "F(4,30) = 3.29, p = 0.04*, η² = 0.467",
                             ylab = "Dissolved Oxygen (mg/L)")

pore_RHMA <- plot_do_species(pore_summary, "RHMA",
                             title_prefix = "Pore Water",
                             y_limits = c(0, 5.5),
                             border_col = "black",
                             subtitle = "F(4,30) = 1.685, p = 0.206, η² = 0.31",
                             ylab = "Dissolved Oxygen (mg/L)")


# Combine panels
p_pore_species <- pore_LARA + pore_RHMA + p1_tank + plot_annotation(tag_levels = "A")

ggsave("pore_species_panels.png", p_pore_species, width = 12, height = 6, dpi = 300)

cat("Saved: pore_species_panels.png\n")

# Print in Rmd
p_pore_species

# -------- PLOT 2: Species Comparison - Bar Chart --------
species_colors <- c("LARA" = "#90EE90", "RHMA" = "#dc2626")

# Calculate position for letters (above error bars)
species_summary <- species_summary %>%
  mutate(letter_y = Mean + SD + 0.25)

p2 <- ggplot(species_summary, aes(x = Substrate, y = Mean, fill = Species)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.8),
                width = 0.25, linewidth = 0.8) +
  geom_hline(yintercept = 2.0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = .group, y = letter_y), 
            position = position_dodge(width = 0.8),
            size = 4, fontface = "bold") +
  scale_fill_manual(values = species_colors) +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +
  labs(
    # title = "Species Comparison: LARA vs RHMA",
    #    subtitle = "Pore water dissolved oxygen by substrate and species",
       x = "Treatment", y = "Dissolved Oxygen (mg/L)",
       fill = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank())
p2

ggsave("species_comparison_bars.png", p2, 
       width = 10, height = 6, dpi = 300)
cat("Saved: species_comparison_bars.png\n")

# -------- PLOT 3: Species Comparison - Line Chart --------
species_means <- species_summary %>%
  dplyr::select(Substrate, Species, Mean)

p3 <- ggplot(species_means, aes(x = Substrate, y = Mean, color = Species, group = Species)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  scale_color_manual(values = species_colors) +
  scale_y_continuous(limits = c(1, 4.5), breaks = seq(1, 4.5, 0.5)) +
  labs(title = "Species Response Patterns Across Substrates",
       subtitle = "Lines converge in good substrates, diverge dramatically in 100% SARGASSUM",
       x = "Substrate", y = "Dissolved Oxygen (mg/L)",
       color = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        plot.title = element_text(size = 14, face = "bold"))
p3

ggsave("species_response_lines.png", p3, 
       width = 10, height = 6, dpi = 300)
cat("Saved: species_response_lines.png\n")

# -------- PLOT 4: Hypoxic Events by Species --------
hypoxic_summary <- pore_data %>%
  group_by(Substrate, Species) %>%
  summarise(
    Hypoxic_Percent = (sum(DO < 2.0) / n()) * 100,
    .groups = "drop"
  )

p4 <- ggplot(hypoxic_summary, aes(x = Substrate, y = Hypoxic_Percent, fill = Species)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = species_colors) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "Hypoxic/Anoxic Events by Species",
       subtitle = "LARA shows 75% hypoxic in 100% SARGASSUM vs 0% for RHMA",
       x = "Substrate", y = "% Samples Hypoxic (<2.0 mg/L)",
       fill = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank())
p4

ggsave("hypoxic_events_by_species.png", p4, 
       width = 10, height = 6, dpi = 300)
cat("Saved: hypoxic_events_by_species.png\n")

# -------- PLOT 5: Individual Data Points --------
p5 <- ggplot(pore_data, aes(x = Substrate, y = DO, color = Species)) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 0.6) +
  geom_hline(yintercept = 2.0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_color_manual(values = species_colors) +
  labs(
    # title = "Individual Pore Water Measurements",
    #    subtitle = "Red dashed line indicates hypoxic threshold (2.0 mg/L)",
       x = "Treatment", y = "Dissolved Oxygen (mg/L)",
       color = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        plot.title = element_text(size = 14, face = "bold"))
p5

ggsave("individual_measurements.png", p5, 
       width = 10, height = 6, dpi = 300)
cat("Saved: individual_measurements.png\n")

# -------- PLOT 6: Combined Comprehensive Plot --------
combined_plot <- (p2 / p3) | (p4 / p5)
combined_plot <- combined_plot +
  plot_annotation(
    title = "Comprehensive Species Comparison Analysis",
    subtitle = "LARA is more vulnerable to poor substrate conditions than RHMA",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  )

combined_plot
ggsave("comprehensive_species_analysis.png", combined_plot, 
       width = 16, height = 12, dpi = 300)
cat("Saved: comprehensive_species_analysis.png\n")

# Combine species comparison and individual datapoints plots
species_pore_plot <- p2 + p5 +
  plot_annotation(tag_levels = "A"
  )
  species_pore_plot


# -------- PLOT 6: Combined Comprehensive Plot --------
combined_plot <- (p2 / p3) | (p4 / p5)
combined_plot <- combined_plot +
  plot_annotation(
    title = "Comprehensive Species Comparison Analysis",
    subtitle = "LARA is more vulnerable to poor substrate conditions than RHMA",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  )

# ============================================================================
# 9. SUMMARY TABLE EXPORT
# ============================================================================

cat("\n\n============================================================\n")
cat("EXPORTING SUMMARY TABLES\n")
cat("============================================================\n\n")

# Export summary statistics
write_csv(pore_summary, "pore_water_summary.csv")
cat("Saved: pore_water_summary.csv\n")

write_csv(tank_summary, "tank_water_summary.csv")
cat("Saved: tank_water_summary.csv\n")

write_csv(species_summary, "species_comparison_summary.csv")
cat("Saved: species_comparison_summary.csv\n")

# ============================================================================
# 10. FINAL SUMMARY
# ============================================================================

cat("\n\n============================================================\n")
cat("ANALYSIS COMPLETE - SUMMARY OF KEY FINDINGS\n")
cat("============================================================\n\n")

cat("1. PORE WATER (Root Zone):\n")
cat("   - F(4,35) = 4.67, p < 0.01 **\n")
cat("   - Effect size: η² = 0.348 (LARGE)\n")
cat("   - Substrate choice CRITICALLY affects pore water oxygen\n\n")

cat("2. TANK WATER (Bulk Water):\n")
cat("   - F(4,15) = 0.36, p > 0.05 (not significant)\n")
cat("   - Effect size: η² = 0.087 (Small)\n")
cat("   - Substrate has NO significant effect on tank water\n\n")

cat("3. SPECIES COMPARISON:\n")
cat("   - LARA: F(4,15) = 3.29, p < 0.05 * (significant)\n")
cat("   - RHMA: F(4,15) = 1.69, p > 0.05 (not significant)\n")
cat("   - LARA is MORE VULNERABLE to poor substrates\n\n")

cat("4. CRITICAL FINDING (100% SARGASSUM):\n")
cat("   - LARA: 75% hypoxic/anoxic samples\n")
cat("   - RHMA: 0% hypoxic samples\n")
cat("   - Difference: 0.98 mg/L (RHMA 51% higher)\n\n")

cat("RECOMMENDATION:\n")
cat("✓ Use GLASS or SOIL substrates for both species\n")
cat("✓ Use LARA as 'indicator species' for substrate quality\n")
cat("❌ Avoid 100% SARGASSUM for both species\n\n")

cat("All analyses complete. Check your working directory for:\n")
cat("  - 6 publication-quality plots (.png)\n")
cat("  - 3 summary data tables (.csv)\n")
cat("============================================================\n")


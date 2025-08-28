##Author: Sarai Hutchinson
##Date: July 30 2025
##Title: Creating a dataframe with plants that survived and only keeping their information from month 1 to 6

#This script creates a dataframe with plants that survived and only keeping their information from month 1 to 6

setwd("C:/Users/sarai/OneDrive/Email attachments/Documents/UVI/Thesis.Work/Data")

#Load libraries
library(dplyr)

# Read the data
ALLDATA <- read.csv("~/UVI/Thesis.Work/Data/Hutchinson_MonthlyMonitoring-MASTER3.csv")

# Step 1: Filter to Alive plants in Time_Period 1 or 6
alive_tp1_6 <- ALLDATA %>%
  filter(Time_Period %in% c(1, 6), Survivorship == "A")


unique(ALLDATA$Time_Period)
unique(ALLDATA$Survivorship)


# Step 2: Keep only Prop_IDs that are alive in BOTH time periods
survived_both <- alive_tp1_6 %>%
  group_by(Prop_ID) %>%
  filter(n_distinct(Time_Period) == 2) %>%  # Ensures alive in both 1 and 6
  ungroup()

# Step 3: Create separate lists for RHMA and LARA
RHMA_survivors <- survived_both %>%
  filter(Species == "RHMA") %>%
  select(Prop_ID) %>%
  distinct()

LARA_survivors <- survived_both %>%
  filter(Species == "LARA") %>%
  select(Prop_ID) %>%
  distinct()

# Step 4 (optional): If you want to save or view them in one long dataframe with a species label
final_long_list <- bind_rows(
  RHMA_survivors %>% mutate(Species = "RHMA"),
  LARA_survivors %>% mutate(Species = "LARA")
)

# View final output
print(final_long_list)

##This part gives me a list with both instead of separated.

# Step 1: Filter to Alive plants in Time_Period 1 or 6
alive_tp1_6 <- ALLDATA %>%
  filter(Time_Period %in% c(1, 6), Survivorship == "A")

# Step 2: Identify Prop_IDs that survived both TP 1 AND TP 6
survivor_ids <- alive_tp1_6 %>%
  group_by(Prop_ID) %>%
  filter(n_distinct(Time_Period) == 2) %>%
  pull(Prop_ID) %>%
  unique()

# Step 3: Pull all rows (all time periods) for just those Prop_IDs
survivor_full_history <- ALLDATA %>%
  filter(Prop_ID %in% survivor_ids)

# View or use
print(survivor_full_history)

# Export the full dataset of survivors (all time periods) to CSV
write.csv(survivor_full_history, "survivors_tp1_to_tp6.csv", row.names = FALSE)

##Create a dataframe with the intake information of those that survived

#Import LARA and RHMA data
LARA_Intake <- read.csv("~/UVI/Thesis.Work/Data/LARA_IntakeData.csv")
RHMA_Intake <- read.csv("~/UVI/Thesis.Work/Data/RHMA_IntakeData.csv")

# Step 1: Get list of Prop_IDs that survived at Time_Period 6
survivors_tp6 <- survivor_full_history %>%
  filter(Time_Period == 6) %>%
  pull(Prop_ID) %>%
  unique()

print(survivors_tp6)

# Step 2: Filter intake data for only those survivors
# Filter LARA intake data
intake_lara_survivors <- LARA_Intake %>%
  filter(Prop_ID %in% survivors_tp6)

# Filter RHMA intake data
intake_rhma_survivors <- RHMA_Intake %>%
  filter(Prop_ID %in% survivors_tp6)

# Combine both sets of survivors while preserving all columns
combined_intake_survivors <- full_join(intake_lara_survivors, intake_rhma_survivors, by = intersect(names(intake_lara_survivors), names(intake_rhma_survivors)))

# Remove the Prop_ID column
combined_intake_survivors <- combined_intake_survivors %>%
  select(-Prop.ID)






# # Which rows are in RHMA_survivors but not in intake_rhma_survivors
# anti_join(RHMA_survivors, intake_rhma_survivors, by = "Prop_ID")


##oF THOSE THAT SURVIVED, HOW MANY ARE FROM A PARTICULAR SITE if there are 3 sites for each species and 100 plants per site?

# Count number of survivors per site
site_counts <- combined_intake_survivors %>%
  group_by(Species, Site) %>%
  summarise(
    Num_Survived = n()
  ) %>%
  arrange(Species, Site)

print(site_counts)

site_counts <- site_counts %>%
  mutate(Survival_Rate = Num_Survived / 100 * 100)  # Since 100 planted per site

print(site_counts)









#Combine ALLDATA dataframe with Site column from intake dataframes
# Keep only Prop_ID and Site from intake files
lara_sites <- LARA_Intake %>%
  select(Prop_ID, Site)

rhma_sites <- RHMA_Intake %>%
  select(Prop_ID, Site)

# Combine both into one Site lookup table
site_lookup <- bind_rows(lara_sites, rhma_sites)

# Add the Site column to ALLDATA by joining on Prop_ID
ALLDATA_with_site <- ALLDATA %>%
  left_join(site_lookup, by = "Prop_ID")

# Step 1: Clean and Prepare Data
survivors_clean2 <- ALLDATA_with_site %>%
  mutate(
    Surv_Bin = ifelse(Survivorship %in% c("A", "DY"), 1, 0),
    Treatment = case_when(
      Treatment == "A" ~ "Soil",
      Treatment == "B" ~ "Crushed glass (CG)",
      Treatment == "C" ~ "Sargassum only (SG)",
      Treatment == "D" ~ "3:1 CG to SG",
      Treatment == "E" ~ "1:3 CG to SG",
      TRUE ~ as.character(Treatment)
    ),
    Treatment = factor(Treatment, levels = c("Soil", "Crushed glass (CG)", "Sargassum only (SG)", "3:1 CG to SG", "1:3 CG to SG")),
    Time_Period = factor(Time_Period, levels = c("1", "2", "3", "4", "5", "6")),
    Species = factor(Species)
  )

# Create dataframe for LARA
LARA_data <- survivors_clean2 %>%
  filter(Species == "LARA")

# Create dataframe for RHMA
RHMA_data <- survivors_clean2 %>%
  filter(Species == "RHMA")

##Doing this to see if the survival is impacted by their collection site
# Logistic regression: Survival ~ Site
glm_site_LARA <- glm(Surv_Bin ~ Site, data = LARA_data, family = binomial)
summary(glm_site_LARA)

glm_site_RHMA <- glm(Surv_Bin ~ Site, data = RHMA_data, family = binomial)
summary(glm_site_RHMA)

library(ggplot2)
library(ggeffects)

# Plot the predicted survival probabilities with CI for LARA
lara_pred <- ggpredict(glm_site_LARA, terms = "Site")
plot(lara_pred) + 
  ggtitle("LARA: Survival Probability by Site") +
  ylab("Predicted Probability of Survival")

# Predicted survival probabilities with CI for RHMA
rhma_pred <- ggpredict(glm_site_RHMA, terms = "Site")
plot(rhma_pred) +
  ggtitle("RHMA: Survival Probability by Site") +
  ylab("Predicted Probability of Survival")

library(lme4)

#Refit Models with Interaction: Site * Treatment + (1 | Replicate)
# For LARA
glmm_LARA <- glmer(Surv_Bin ~ Site * Treatment + (1 | Replicate), 
                   data = LARA_data, family = binomial)
summary(glmm_LARA)

# For RHMA
glmm_RHMA <- glmer(Surv_Bin ~ Site * Treatment + (1 | Replicate), 
                   data = RHMA_data, family = binomial)
summary(glmm_RHMA)

library(sjPlot)

#Add Confidence Intervals to Fixed Effects (Odds Ratios)
tab_model(glmm_LARA,
          transform = "exp",  # to show odds ratios
          show.ci = TRUE,
          show.p = TRUE,
          show.se = TRUE,
          title = "LARA: Logistic Mixed Model with Site x Treatment Interaction")
###PLEASE NOTE: I love the APA-style table this created! I might use this to display my other results in supplementals.

#Visualise the things now

# Create predictions across Site and Treatment
pred_lara <- ggpredict(glmm_LARA, terms = c("Treatment", "Site"))

# Plot
ggplot(pred_lara, aes(x = x, y = predicted, color = group)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(0.3), width = 0.2) +
  labs(
    title = "Predicted Probability of Survival",
    subtitle = "LARA | Site x Treatment Interaction",
    x = "Treatment", y = "Predicted Survival Probability",
    color = "Site"
  ) +
  theme_minimal()

#The RHMA version of the glmm didn't work and this one didn't either
glmm_RHMA_simple <- glmer(Surv_Bin ~ Site + Treatment + (1 | Replicate), 
                          data = RHMA_data, family = binomial)
summary(glmm_RHMA_simple)

##NEXT STEPS
#I need to just get the mean overall height by species and any propagule differences upon intake
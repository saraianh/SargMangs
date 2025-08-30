#Title: Adding Site to Full Monitoring and Intake Dataset
#Author: Sarai Hutchinson
#Created: August 30th 2025

#This code will add the Site information from time Point 0 to all other time points based on the Prop_ID

library(dplyr)
library(tidyverse)

df <- read_csv("C:\\Users\\sarai\\OneDrive\\Email attachments\\Documents\\UVI\\Thesis.Work\\SargMangs\\Data/Hutchinson_MonitoringAndIntake.csv")


# Create a reference table of Prop_ID → Site from TP0
site_lookup <- df %>%
  filter(Time_Period == 0) %>%
  select(Prop_ID, Site)

# Join it back onto your full dataframe
df_filled_site <- df %>%
  select(-Site) %>%  # remove old Site column if you want to overwrite
  left_join(site_lookup, by = "Prop_ID") %>%
  select(1:6, Site, everything()) # Get the column order with Site placed in position 7
  
# Save
write_csv(df_filled_site, "Data/Hutchinson_MonitoringAndIntake_Sites.csv")

# Load later
ALLDATA <- read_csv("Data/Hutchinson_MonitoringAndIntake_Sites.csv")

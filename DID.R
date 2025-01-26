# Load necessary libraries
install.packages("plm")
install.packages("dplyr")
library(plm)
library(dplyr)

# Load data
data <- read.csv("DID.csv")

# Create Treated and Post variables
data$Treated <- as.numeric(data$Legalize == "1")
#first we want to assume the Post is 2018, which is when U.S. legalize sb
data$Post <- ifelse(data$Year >= 2021 & data$Year <= 2022, 1, 0)  # Only 2021 and 2022 are marked as Post = 1


library(dplyr)

# Filter data for years 2021 and 2022, excluding states that legalized in 2021 (Legalize == 1)
DID_data <- data %>%
  filter(Year %in% c(2021, 2022)) %>%  # Keep only data for 2021 and 2022
  group_by(LocationDesc) %>%
  filter(!(Year == 2021 & Legalize == 1)) %>%  # Remove states that legalized in 2021
  ungroup()  # Ungroup after filtering

# Identify states that legalized in 2021 (Legalize == 1)
states_legalized_2021 <- data %>%
  filter(Year == 2021 & Legalize == 1) %>%
  select(LocationDesc) %>%
  distinct()

# Filter the data for 2021 and 2022, remove states that legalized in 2021, and keep all variables
DID_data <- data %>%
  filter(Year %in% c(2021, 2022)) %>%  # Keep data for 2021 and 2022
  filter(!(LocationDesc %in% states_legalized_2021$LocationDesc))  # Remove states that legalized in 2021
head(DID_data)

# Clean and convert Income column
DID_data$Income <- as.numeric(gsub("[^0-9.]", "", DID_data$Income))  # Remove non-numeric characters
DID_data$Income[is.na(DID_data$Income)] <- mean(DID_data$Income, na.rm = TRUE)  # Impute missing values with mean

# Convert factor variables
factor_vars <- c("Party", "Legislative.Party")
DID_data[factor_vars] <- lapply(DID_data[factor_vars], as.factor)

library(plm)
pdata <- pdata.frame(DID_data, index = c("States", "Year"))

#(Significant)This is the DID Revenue with Legalize with all the more significant variables in the model 
did_model <-plm(Revenue ~ Legalize + Party + Firearm + Alcohol + E.Cigarettes + Black, data = DID_data, index = c("States","Year"), effect = "twoways", model = "within")
summary(did_model)


did_model <-plm(Revenue ~ Legalize + Employment + Alcohol + Party + Firearm + E.Cigarettes + Poverty.100... + White, data = pdata, index = c("States","Year"), effect = "twoways", model = "within")
summary(did_model)

#This is the DID with all the variables we have included in the model 
did_model <-plm(Income ~ Legalize + Employment + X19.25 + X26.34 + X35.54 + X55.64 + X65 + Alcohol + Male + Party + Firearm + E.Cigarettes + Poverty.100... + White + Black + Hispanic + Asian + Mix.Races, data = pdata, index = c("States","Year"), effect = "twoways", model = "within")
summary(did_model)


#Margins Assignment Below 
# Load necessary libraries
if (!require("plm")) install.packages("plm")
library(plm)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Load your dataset (replace with your file path)
file_path <- "~/Downloads/DID.csv"
library(readr)
data <- read_csv(file_path)

# Convert data to pdata.frame for panel data analysis
data_clean <- pdata.frame(data, index = c("States", "Year"))

# Define the baseline model
baseline_model <- plm(Revenue ~ Legalize + Employment + X19.25 + X26.34 + X35.54 + X55.64 + X65 + 
                      Alcohol + Male + Party + Firearm + E.Cigarettes + Poverty.100... + 
                      White + Black + Hispanic + Asian + Mix.Races,
                      data = data_clean, 
                      effect = "twoways", 
                      model = "within")

# Summary of the baseline model
baseline_summary <- summary(baseline_model)

# Extract coefficients, R-squared, and observations
coefficients <- coefficients %>%
  select(Variable, Estimate, Std..Error, t.value, Pr...t..)
observations <- nobs(baseline_model)
r_squared_within <- baseline_summary$r.squared[1]  # Within R-squared
r_squared_between <- baseline_summary$r.squared[2]

coefficients$Observations <- observations
coefficients$R_squared_within <- r_squared_within
coefficients$R_squared_between <- r_squared_between

# Create a summary table
summary_table <- coefficients %>%
  mutate(Observations = observations, R_squared = r_squared)

# Save the summary table to a CSV file
write.csv(summary_table, "baseline_summary.csv", row.names = FALSE)


# Visualization
library(ggplot2)
# Load necessary libraries
if (!require("margins")) install.packages("margins")
library(margins)
# Load necessary libraries
if (!require("margins")) install.packages("margins")
library(margins)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("plm")) install.packages("plm")
library(plm)

# Load your dataset (adjust the path as necessary)
file_path <- "~/Downloads/DID.csv"
data <- read.csv(file_path)

# Convert data to pdata.frame for panel data analysis
data_clean <- pdata.frame(data, index = c("States", "Year"))

# Define the baseline model (ensure the correct model is specified for panel data)
baseline_model <- plm(Revenue ~ Legalize + Employment + X19.25 + X26.34 + X35.54 + X55.64 + X65 + 
                        Alcohol + Male + Party + Firearm + E.Cigarettes + Poverty.100... + 
                        White + Black + Hispanic + Asian + Mix.Races,
                      data = data_clean, 
                      effect = "twoways", 
                      model = "within")

# Use the margins function to calculate the marginal effects (without 'newdata')
marginal_effects <- margins(baseline_model)

# Create a data frame of the margins results
margins_df <- summary(marginal_effects)

# Extract relevant columns and format them
margins_df <- margins_df[, c("factor", "AMean", "SE", "t", "P", "lower", "upper")]

# Rename columns for clarity
colnames(margins_df) <- c("Variable", "Margin", "Delta_std_err", "t_stat", "P_value", "Lower_CI", "Upper_CI")

# View the margins summary data
print(margins_df)

# Save the margins summary data to a CSV file
write.csv(margins_df, "margins_summary.csv", row.names = FALSE)

# Plot the results using marginsplot (for visualization)
marginsplot(marginal_effects)

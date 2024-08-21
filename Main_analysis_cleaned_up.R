library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)
library(car)
library(multcompView)
library(agricolae)
library(ggsignif)
library(report)
library(multcomp)
library(growthcurver)
library (gridExtra)



# Load the dataset
df <- read.csv("Rosalynn Tidy data.csv")

# SECTION A

# Filter the data to include only weeks 1 to 5 for all species, except Suillus lakei which includes up to week 6.
df_1_5 <- df %>%
  filter(Week %in% 1:5)

# Convert temperature and species columns to factors for proper categorical handling in plots.
df_1_5$Temperature..Celsius. <- as.factor(df_1_5$Temperature..Celsius.)
df_1_5$Species <- as.factor(df_1_5$Species)

# Summarize the data to calculate the mean and standard deviation of growth for each combination of week, temperature, and species.
# This aggregated data will be used for plotting average growth across different conditions.
summary_df <- df_1_5 %>%
  group_by(Week, Temperature..Celsius., Species) %>%
  summarise(
    mean_growth = mean(Growth, na.rm = TRUE),
    sd_growth = sd(Growth, na.rm = TRUE)
  )

# Figure 1A: Line plot showing the average growth of each species across different temperatures, faceted by temperature.
ggplot(summary_df, aes(x = Week, y = mean_growth, color = Species)) +
  geom_line(aes(group = Species), size = 1.5, alpha = 0.7) + # Add lines with increased thickness and transparency
  geom_point(size = 4, alpha = 0.7) + # Add points with increased size and transparency
  geom_errorbar(aes(ymin = mean_growth - sd_growth, ymax = mean_growth + sd_growth),
                width = 1, position = position_dodge(0.2), alpha = 0.7) + # Add error bars with offset and transparency
  scale_color_viridis_d(option = "C", begin = 0, end = 0.85) + # Use the viridis color palette for clarity
  labs(title = "Average Growth of Fungi at Different Temperatures",
       x = "Week",
       y = "Average Growth (cm^2)",
       color = "Species") +
  theme_minimal() +
  facet_wrap(~ Temperature..Celsius., scales = "fixed") # Facet by temperature with fixed y-axis scaling

# Figure 1B: Line plot showing the average growth across species, faceted by species.
ggplot(summary_df, aes(x = Week, y = mean_growth, color = Temperature..Celsius.)) +
  geom_line(aes(group = Temperature..Celsius.), size = 1.5, alpha = 0.7) + # Add lines with increased thickness and transparency
  geom_point(size = 4, alpha = 0.7) + # Add points with increased size and transparency
  geom_errorbar(aes(ymin = mean_growth - sd_growth, ymax = mean_growth + sd_growth),
                width = 0.2, position = position_dodge(0.1), alpha = 0.7) + # Add error bars with offset and transparency
  scale_color_viridis_d(option = "C", begin = 0, end = 0.85) + # Use the viridis color palette
  labs(title = "Average Growth of Fungi at Different Temperatures",
       x = "Week",
       y = "Average Growth (cm^2)",
       color = "Temperature (Celsius)") +
  theme_minimal() +
  facet_wrap(~ Species, scales = "fixed") # Facet by species with fixed y-axis scaling

# Figure 1C: Line plot showing average growth by species and temperature, with custom facet grid layout.
ggplot(summary_df, aes(x = Week, y = mean_growth, color = Species)) +
  geom_line(size = 1.5, alpha = 0.7) + # Add lines with transparency
  geom_point(size = 4, alpha = 0.7) + # Add points with transparency
  geom_errorbar(aes(ymin = mean_growth - sd_growth, ymax = mean_growth + sd_growth),
                width = 0.2, position = position_dodge(0.1), alpha = 0.7) + # Add error bars with offset and transparency
  scale_color_viridis_d(option = "inferno", begin = 0, end = 0.85) + # Use the viridis color palette
  labs(title = "Average Growth of Fungi at Different Temperatures",
       x = "Week",
       y = "Average Growth (cm^2)",
       color = "Species") +
  theme_minimal() +
  facet_grid(rows = vars(Temperature..Celsius.), cols = vars(Species), scales = "free_y") # Facet grid by temperature and species with free y-axis scaling

# Calculate the rate of growth for each week using the (y2-y1)/(x2-x1) formula for each strain and plate within species.
df_growth_rate <- df_1_5 %>%
  group_by(strain_ID, Temperature..Celsius., Plate) %>%
  arrange(strain_ID, Temperature..Celsius., Plate, Week) %>%
  mutate(
    Previous_Week = lag(Week),
    Previous_Growth = lag(Growth),
    Growth_Rate = ifelse(is.na(Previous_Week) | is.na(Previous_Growth), 0, (Growth - Previous_Growth) / (Week - Previous_Week))
  ) %>%
  ungroup()

# Summarize the growth rate data to calculate the mean and standard deviation for each combination of week, temperature, and species.
summary_df_growth_rate <- df_growth_rate %>%
  group_by(Week, Temperature..Celsius., Species) %>%
  summarise(
    mean_growth_rate = mean(Growth_Rate, na.rm = TRUE),
    sd_growth_rate = sd(Growth_Rate, na.rm = TRUE)
  )

# Figure 2A: Line plot showing the rate of growth for each species across different temperatures.
ggplot(summary_df_growth_rate, aes(x = Week, y = mean_growth_rate, color = Species)) +
  geom_line(aes(group = Species), size = 1.5, alpha = 0.7) + # Add lines with increased thickness and transparency
  geom_point(size = 4, alpha = 0.7) + # Add points with increased size and transparency
  geom_errorbar(aes(ymin = mean_growth_rate - sd_growth_rate, ymax = mean_growth_rate + sd_growth_rate),
                width = 0.2, position = position_dodge(0.1), alpha = 0.7) + # Add error bars with offset and transparency
  scale_color_viridis_d(option = "inferno", begin = 0, end = 0.85) + # Use the viridis color palette
  labs(title = "Rate of Growth of Fungi at Different Temperatures",
       x = "Week",
       y = "Rate of Growth (cm^2/week)",
       color = "Species") +
  theme_minimal() +
  facet_wrap(~ Temperature..Celsius., scales = "fixed") # Facet by temperature with fixed y-axis scaling

# Figure 2B: Line plot showing the rate of growth across species, faceted by species.
ggplot(summary_df_growth_rate, aes(x = Week, y = mean_growth_rate, color = Temperature..Celsius.)) +
  geom_line(aes(group = Temperature..Celsius.), size = 1.5, alpha = 0.7) + # Add lines with increased thickness and transparency
  geom_point(size = 4, alpha = 0.7) + # Add points with increased size and transparency
  geom_errorbar(aes(ymin = mean_growth_rate - sd_growth_rate, ymax = mean_growth_rate + sd_growth_rate),
                width = 0.2, position = position_dodge(0.1), alpha = 0.7) + # Add error bars with offset and transparency
  scale_color_viridis_d(option = "inferno", begin = 0, end = 0.85) + # Use the viridis color palette
  labs(title = "Rate of Growth of Fungi by Species",
       x = "Week",
       y = "Rate of Growth (cm^2/week)",
       color = "Temperature (Celsius)") +
  theme_minimal() +
  facet_wrap(~ Species, scales = "fixed") # Facet by species with fixed y-axis scaling

# Determine the highest growth rate for each species and temperature combination from any week.
highest_growth_rate <- summary_df_growth_rate %>%
  group_by(Temperature..Celsius., Species) %>%
  filter(mean_growth_rate == max(mean_growth_rate)) %>%
  ungroup()

# Figure 3: Line plot showing the highest growth rate across species and temperatures.
ggplot(highest_growth_rate, aes(x = Temperature..Celsius., y = mean_growth_rate, group = Species, color = Species)) +
  geom_line(size = 1.5, alpha = 0.7) + # Add lines with increased thickness and transparency
  geom_point(size = 4, alpha = 0.7) + # Add points with increased size and transparency
  geom_errorbar(aes(ymin = mean_growth_rate - sd_growth_rate, ymax = mean_growth_rate + sd_growth_rate), 
                width = 0.2, position = position_dodge(0.1), alpha = 0.7) + # Add error bars with offset and transparency
  scale_color_viridis_d(option = "viridis", begin = 0, end = 0.85) +
  labs(title = "Highest Growth Rate of Fungi by Temperature",
       x = "Temperature (Celsius)",
       y = "Highest Growth Rate",
       color = "Species") +
  theme_minimal()

#### Further Analysis for Figure 3

# Finding the raw growth data associated with the highest growth rates identified earlier.
raw_growth_data <- df_growth_rate %>%
  semi_join(highest_growth_rate, by = c("Species", "Week", "Temperature..Celsius."))

# Create the boxplot with jittered points for each species to visualize the distribution of growth rates according to strains.
a <- ggplot(raw_growth_data, aes(x = factor(Temperature..Celsius.), y = Growth_Rate, color = Species)) +
  geom_boxplot(aes(group = interaction(Species, Temperature..Celsius.)), outlier.shape = NA, alpha = 0.5, position = position_dodge(width = 0.75)) +
  geom_jitter(aes(shape = strain_ID), size = 2, alpha = 0.7, position = position_dodge(width = 0.75)) +
  scale_color_viridis_d(option = "C", begin = 0, end = 0.85) +
  scale_shape_manual(values = 1:length(unique(df_growth_rate$strain_ID))) +  # Assign different shapes for each strain
  labs(title = "Highest Growth Rate of Fungi by Temperature",
       x = "Temperature (Celsius)",
       y = "Highest Growth Rate at Any Week (cm^2/week") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 6.3)) +
  facet_wrap(~ Species, scales = "fixed") +  # Facet by species with fixed y-axis scaling
  theme(legend.position = "right")

b <- ggplot(highest_growth_rate, aes(x = Temperature..Celsius., y = mean_growth_rate, group = Species, color = Species)) +
  geom_line(size = 1.5, alpha = 0.7) + # Add lines with increased thickness and transparency
  geom_point(size = 4, alpha = 0.7) + # Add points with increased size and transparency
  geom_errorbar(aes(ymin = mean_growth_rate - sd_growth_rate, ymax = mean_growth_rate + sd_growth_rate), 
                width = 0.2, position = position_dodge(0.1), alpha = 0.7) + # Add error bars with offset and transparency
  scale_color_viridis_d(option = "C", begin = 0, end = 0.85) +
  labs(title = "Highest Growth Rate of Fungi by Temperature",
       x = "Temperature (Celsius)",
       y = "Highest Growth Rate at Any Week (cm^2/week)",
       color = "Species") +
  coord_cartesian(ylim = c(0, 6.3)) +
  facet_wrap(~ Species, scales = "fixed") +
  theme_minimal()

# Combine the boxplot and line plot side by side for comparison.
grid.arrange(a, b, ncol = 2)

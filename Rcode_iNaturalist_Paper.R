# Setup and required packages
setwd("C:/Users/15087/Desktop/iNaturalistPaperData")

library(tidyverse)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(ggpubr)
library(readxl)
library(RColorBrewer)
library(rcompanion)

# Load all necessary input files
# Phenotype data
CassPheno <- read_excel("Cassiopea_Phenotype_iNaturalist_Final.xlsx")

# iNaturalist Observation Files
CassObs <- read_csv("observations-423020.csv")
TrueJellies <- read.csv("observations-TrueJellies.csv")

# NOAA temperature logger data
all_loggers_combined <- read_csv("all_loggers_fulldata.csv")

# Files for supplemental material
jellyfish_with_loggers <- read.csv("jellyfish_with_loggers.csv")
SD_Logger <- read.csv("SD_Logger.csv") # Our deployed logger
NOAA_TB_Logger <- read.csv("NOAA_TB_Logger.csv") # Tampa logger
NOAA_KW_Logger <- read.csv("NOAA_KW_Logger.csv") # Key West logger

# Define color schemes used throughout the analysis
# Colors for different phenotypes and regions
BellColorValues <- c(brown = "#6C3B1C", blue = "#1E88E5", white = "Beige")
BellColorValuesBlack <- c(brown = "#6C3B1C", blue = "#1E88E5", white = "Black")
BellSizeColor <- c(small = "#FFE0B2", medium = "#FFB74D", large = "#F57C00")
VesicleSizeColor <- c(small = "#C8E6C9", medium = "#81C784", large = "#2E7D32")
SizeColor <- brewer.pal(n = 3, name = "YlOrBr")
VesShColor <- c(round = "#BEAED4", slender = "#7FC97F")
regioncolors <- c(
  "NorthWest" = "#D50A0A",  
  "SouthWest" = "#8BC34A",  
  "SouthEast" = "#008E97",  
  "NorthEast" = "#F4B831"   
)
# Define color schemes for MCA
MCABellColor <- c("BellColor blue" = "#1E88E5", 
                  "BellColor brown" = "#6C3B1C", 
                  "BellColor white" = "Beige")
MCASizeColor <- c("Size small" = "#FFE0B2", 
                  "Size medium" = "#FFB74D", 
                  "Size large" = "#F57C00")
MCAVesicleSizeColor <- c("VesicleSize small" = "#FFE0B2", 
                         "VesicleSize medium" = "#FFB74D", 
                         "VesicleSize large" = "#F57C00")
MCAVesicleShapeColor <- c("slender" = "#7FC97F", 
                          "round" = "#BEAED4")
MCAVesicleColorValues <- c("VesicleColor blue" = "#1E88E5","VesicleColor brown" = "#6C3B1C",
                           "VesicleColor white" = "Beige")

# Environment and region colors
EnvironmentColorsMCA <- c("sandbar" = "#4A4A4A","dock" = "#4A4A4A","wetland" = "#4A4A4A","mangrove" = "#4A4A4A",
  "estuary" = "#4A4A4A","channel" = "#4A4A4A","reef" = "#4A4A4A","seagrass bed" = "#4A4A4A","lagoon" = "#4A4A4A")

RegionColorsMCA <- c("NorthWest" = "#2E2E2E","NorthEast" = "#2E2E2E","SouthWest" = "#2E2E2E","SouthEast" = "#2E2E2E")

# Combine all color schemes
all_colors <- c(MCABellColor, 
                MCASizeColor, 
                MCAVesicleSizeColor,
                MCAVesicleShapeColor,
                MCAVesicleColorValues,
                EnvironmentColorsMCA,
                RegionColorsMCA)

# Function to categorize regions based on lat/long
categorize_region <- function(lat, lon) {
  case_when(
    lat > 26 & lon > -81 ~ "NorthEast",
    lat > 26 & lon <= -81 ~ "NorthWest",
    lat <= 26 & lon > -81 ~ "SouthEast",
    lat <= 26 & lon <= -81 ~ "SouthWest",
    TRUE ~ NA_character_
  )
}

# Initial data cleaning and preparation
# Convert dates and add year column
CassObs$Date <- as.Date(CassObs$observed_on, format = "%Y-%m-%d")
CassObs$Year <- format(CassObs$Date, format="%Y")
CassObs$days_since_reference <- as.numeric(difftime(CassObs$Date, "2003-01-01", units = "days"))
CassObs <- subset(CassObs, user_login != "myles_wagner")
CassObs <- subset(CassObs, days_since_reference > 0)

# Process True Jellies data
TrueJellies <- tidyr::separate(TrueJellies, 'observed_on',
                               into = c('year', 'month', 'day'),
                               sep= '-', remove = FALSE)
TrueJellies$Date <- as.Date(TrueJellies$observed_on, format = "%Y-%m-%d")
TrueJellies$days_since_reference <- as.numeric(difftime(TrueJellies$Date, "2003-01-01", units = "days"))
TrueJellies <- subset(TrueJellies, days_since_reference > 0 & captive_cultivated == "false")

# Create subsets for different analyses
TrueJelliesNoCass <- subset(TrueJellies, !grepl("^Cassiopea", scientific_name))
Aurelia <- subset(TrueJellies, grepl("Aurelia", scientific_name))

# Create Florida-specific datasets
CassFlNL <- subset(CassObs, latitude >= 24.035 & latitude < 30.627 & longitude > -83.997 & longitude < -79.493)
CassFl <- subset(CassObs, latitude >= 24.035 & latitude < 30.627 & longitude > -83.997 & longitude < -79.493)
Aurelia_FL <- subset(Aurelia, latitude >= 24.035 & latitude < 30.627 & longitude > -83.997 & longitude < -79.493)
AllJellyFl <- subset(TrueJellies, latitude >= 24.035 & latitude < 30.627 & longitude > -83.997 & longitude < -79.493)

env_order <- c("sandbar", "dock", "wetland", "mangrove", "estuary", "channel", "reef", "seagrass bed","lagoon") 

#figure generation

# Function to find highest latitude points by year for range expansion analysis
find_highest_points <- function(data, year_col, lat_col) {
  data %>%
    mutate(!!lat_col := as.numeric(as.character(!!sym(lat_col)))) %>%
    filter(!is.na(!!sym(lat_col))) %>%
    group_by(!!sym(year_col)) %>%
    filter(!!sym(lat_col) == max(!!sym(lat_col), na.rm = TRUE)) %>%
    slice_head(n = 1)
}

# Get highest points for each species group
highest_points <- find_highest_points(CassFl, "Year", "latitude")
highest_Aurelia <- find_highest_points(Aurelia_FL, "year", "latitude")
highest_AllJelly <- find_highest_points(AllJellyFl, "year", "latitude")

# Create subsets for 2015 onwards
highest_points2015 <- subset(highest_points, Year >= 2015)
highest_Aurelia2015 <- subset(highest_Aurelia, year >= 2015)
highest_AllJelly2015 <- subset(highest_AllJelly, year >= 2015)

# Process data for genus-specific analysis
AllJellyFl$scientific_name <- sub(" .*", "", AllJellyFl$scientific_name)
AllJellyFl <- AllJellyFl %>%
  mutate(genus_group = case_when(
    scientific_name == "Aurelia" ~ "Aurelia", 
    scientific_name == "Cassiopea" | scientific_name == "Cassiopeidae" ~ "Cassiopea",  
    TRUE ~ "Other"  
  ))

# Define colors for different genera
genus_colors <- c("Aurelia" = "#FDB58B",     
                  "Cassiopea" = "#1E88E5",   
                  "Other" = "#8CBC8C")

# Create post-2015 datasets
CassFl_2015 <- subset(CassFl, Year >= 2015)
CassFLNL_2015 <- subset(CassFlNL, Year >= 2015)
Aurelia_FL_2015 <- subset(Aurelia_FL, year >= 2015)
AllJellyFl2015 <- subset(AllJellyFl, year >= 2015)

# Create histogram plots for each group
# All Scyphozoa histogram
JellyHist2015 <- ggplot(AllJellyFl2015, aes(x = factor(year), fill = genus_group)) + 
  geom_bar(position = "stack") + 
  labs(title = "All Scyphozoa", x = element_blank(), y = element_blank()) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = genus_colors) +
  scale_y_continuous(limits = c(0, 650))

# Cassiopea histogram
CassHist2015 <- ggplot(CassFLNL_2015, aes(x = factor(Year))) + 
  geom_bar(position = "stack", fill = "#1E88E5") + 
  labs(title = "Cassiopea spp.", x = element_blank(), y = "Count of Observations") + 
  theme_classic() + 
  scale_y_continuous(limits = c(0, 650))

# Aurelia histogram
AureliaHist2015 <- ggplot(Aurelia_FL_2015, aes(x = factor(year))) + 
  geom_bar(position = "stack", fill = "#FDB58B") + 
  labs(title = "Aurelia spp.", x = element_blank(), y = element_blank()) + 
  theme_classic() + 
  scale_y_continuous(limits = c(0, 650))

# Create range expansion plots for each group
# Cassiopea range expansion
CasslineNL_Final <- ggplot(CassFLNL_2015, aes(x = Date, y = latitude)) + 
  geom_point(color = "#1E88E5", show.legend = F, size = .5) + 
  theme_classic() + 
  geom_smooth(data = CassFLNL_2015, aes(x = Date, y = latitude), method = "glm", se = TRUE, color = "red") + 
  stat_cor(label.y = 28.4, size = 5) + 
  stat_regline_equation(label.y = 28, size = 5) + 
  geom_line(data = highest_points2015, aes(group = 1), color = "blue") + 
  scale_y_continuous(limits = c(24,30)) + 
  theme(plot.margin = margin(10, 10, 10, 10), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 22)) + 
  labs(y = "Latitude")

# Aurelia range expansion
Aurelialine2015 <- ggplot(Aurelia_FL_2015, aes(x = Date, y = latitude)) + 
  geom_point(color = "#FDB58B", size = .5) + 
  theme_classic() + 
  geom_smooth(data = Aurelia_FL_2015, aes(x = Date, y = latitude), method = "glm", se = TRUE, color = "red") + 
  stat_cor(label.y = 28.4, size = 5) + 
  stat_regline_equation(label.y = 28, size = 5) + 
  theme(plot.margin = margin(10, 10, 10, 10), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 22)) + 
  scale_y_continuous(limits = c(24,30)) + 
  labs(y = element_blank()) +
  geom_line(data = highest_Aurelia2015, aes(group = 1), color = "blue") + 
  scale_y_continuous(limits = c(24,30))

# All Scyphozoa range expansion
AllJellyline2015 <- ggplot(AllJellyFl2015, aes(x = Date, y = latitude)) + 
  geom_point(show.legend = FALSE, aes(color = genus_group ), size = .5) + 
  theme_classic() + 
  geom_smooth(method = "glm", se = TRUE, color = "red") + 
  stat_cor(label.y = 28.4, size = 5) + 
  stat_regline_equation(label.y = 28, size = 5) + 
  theme(plot.margin = margin(10, 10, 10, 10), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 22),
        legend.position = "none") + 
  scale_y_continuous(limits = c(24,30)) + 
  labs(y = element_blank()) +
  geom_line(data = highest_AllJelly2015, aes(group = 1), color = "blue") + 
  scale_color_manual(values = genus_colors)

# Combine all plots into Figure 2
GenusComparison <- grid.arrange(CassHist2015, AureliaHist2015, JellyHist2015,
                                CasslineNL_Final, Aurelialine2015, AllJellyline2015, 
                                ncol = 3, heights = c(1, 2))

# Save Figure 2
#ggsave("GenusComparison.svg", GenusComparison)

# Statistical analysis of range expansion
# Linear model for Cassiopea range expansion
lm_model <- glm(latitude ~ days_since_reference, data = CassObs)
summary(lm_model)
tab_model(lm_model)


#figure 3

# Process temperature data
all_loggers_combined$date <- as.Date(all_loggers_combined$date)

# Add region classification to temperature data
all_loggers_combined <- all_loggers_combined %>%
  mutate(region = mapply(categorize_region, lat, lon))

# Calculate yearly statistics for each region
Yearly_region_data <- all_loggers_combined %>%
  mutate(Year = as.integer(format(date, "%Y"))) %>%
  filter(Year < 2024) %>%
  group_by(Year, region) %>%
  summarise(yearly_avg_temp = mean(mean_temp, na.rm = TRUE)) %>%
  ungroup()

# Create regional temperature trend plot
RegionalTemperatures <- ggplot(Yearly_region_data, aes(x = Year, y = yearly_avg_temp, color = region)) +
  geom_line(size = 1) +
  geom_smooth(method = "glm", se = TRUE, color = "black", linetype = "dashed") +
  labs(title = "Yearly Average Temperature by Region",
       x = "Year",
       y = "Average Temperature (°C)") +
  theme_classic() +
  scale_color_manual(values = regioncolors)
RegionalTemperatures
# Save temperature trends plot
#ggsave("RegionalTemperatures.svg", RegionalTemperatures)

# Calculate cold events (temp < 15°C) by region
cold_event_by_logger3 <- all_loggers_combined %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(region, year, logger_name) %>%
  filter(year < 2024) %>%
  summarize(
    cold_days = sum(mean_temp < 15, na.rm = TRUE),
    total_days = n(),
    .groups = "drop"
  ) %>%
  filter(total_days > 180) %>%  # Only include loggers with >180 days of data
  group_by(year, region) %>%
  summarize(
    mean_cold_events = mean(cold_days),
    n_loggers = n(),
    .groups = "drop"
  )

# Create cold events plot
ColdEventsControlled <- ggplot(cold_event_by_logger3, 
                               aes(x = year, y = mean_cold_events, color = region, group = region)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year", 
    y = "Mean Cold Events per Logger\n(>180 days coverage)",
    title = "Cold Events by Region",
    color = "Region"
  ) +
  theme_classic() + 
  scale_color_manual(values = regioncolors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ColdEventsControlled
# Save cold events plot
#ggsave("ColdEventsControlled.svg", ColdEventsControlled)

# Create logger location map
# Aggregate data by logger location
aggregated_Loggerdata <- all_loggers_combined %>%
  group_by(lat, lon, logger_name) %>%
  summarise(count = n())

# Load map data
library(maps)
wrld_simpl <- map_data("world")

# Create logger location map
Loggermap <- ggplot() +
  geom_polygon(data = wrld_simpl, aes(x = long, y = lat, group = group), 
               colour = "black", fill = "grey95") +
  geom_point(data = aggregated_Loggerdata, 
             aes(x = lon, y = lat, size = 10),
             alpha = 0.7, show.legend = FALSE) +
  geom_text(data = aggregated_Loggerdata,
            aes(x = lon, y = lat, 
                label = paste(logger_name, sep = ": #")), 
            color = "black", size = 3, 
            nudge_x = 0.05, nudge_y = 0.05,
            hjust = 0) +
  geom_vline(xintercept = -81, linetype = "dashed", color = "grey30") +
  geom_hline(yintercept = 26, linetype = "dashed", color = "grey30") +
  coord_fixed(xlim = c(-83, -79.5), ylim = c(24, 30)) +
  theme_classic() +
  theme(legend.position = c(0.5, 0.75),
        axis.text.y = element_text(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  xlab("Longitude") + ylab("Latitude") +
  guides(size = guide_legend(order = 1))
Loggermap
# Save logger map
#ggsave("Loggermap.svg", Loggermap)

# Validate logger data accuracy
# Compare our deployed logger with NOAA logger
merged_data2 <- merge(SD_Logger, NOAA_TB_Logger, 
                      by = "DateTime", suffixes = c("_SD", "_NOAA"))
merged_data2 <- na.omit(merged_data2)

# Calculate correlation
correlation_coefficient <- cor(merged_data2$Temp_SD, merged_data2$Temp_NOAA)

# Create validation plot
logger_validation <- ggplot(merged_data2, aes(x = Temp_SD, y = Temp_NOAA)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Temperature (SD)", 
       y = "Temperature (NOAA)", 
       title = "Correlation between Shallow Dock and NOAA Logger") + 
  stat_cor(label.y = 22.4) + 
  stat_regline_equation(label.y = 23) +
  theme_classic()

# Save validation plot
#ggsave("logger_validation.svg", logger_validation)

# Statistical analysis of temperature differences
# ANOVA and Tukey's test for regional differences
temp_region_aov <- aov(mean_temp ~ region, data = all_loggers_combined)
temp_region_tukey <- TukeyHSD(temp_region_aov)
print(temp_region_tukey)

# Calculate specific temperature differences between regions
temp_diffs <- all_loggers_combined %>%
  group_by(region) %>%
  summarise(
    mean_temp = mean(mean_temp, na.rm = TRUE),
    sd_temp = sd(mean_temp, na.rm = TRUE)
  )

#phenotypes and figure 4

# Load and prepare phenotype data
CassPheno <- read_excel("Cassiopea_Phenotype_iNaturalist_Final.xlsx")
CassObs <- read_csv("observations-423020.csv")
CassObs$Date <- as.Date(CassObs$observed_on, format = "%Y-%m-%d")
CassObs$Year <- format(CassObs$Date, format="%Y")
CassObs$days_since_reference <- as.numeric(difftime(CassObs$Date, "2003-01-01", units = "days"))
CassObs <- subset(CassObs, days_since_reference > 0)

# Merge datasets and clean
MergedCass <- merge(CassPheno, CassObs, by = "id") %>%
  mutate(latitude = latitude.y,
         longitude = longitude.y) %>%
  filter(!is.na(latitude) & !is.na(longitude),
         latitude >= 24,
         longitude <= -79.5)

# Process dates and set factor levels
MergedCass <- MergedCass %>%
  separate(observed_on, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         Date = as.Date(observed_on, format = "%Y-%m-%d"))

# Set factor levels for phenotypic traits
MergedCass <- MergedCass %>%
  mutate(Size = factor(Size, levels = c("small", "medium", "large", "n/a")),
         VesicleSize = factor(VesicleSize, levels = c("small", "medium", "large", "n/a")),
         BellColor = factor(BellColor, levels = c("blue", "brown", "white")),
         VesicleShape = factor(VesicleShape, levels = c("round", "slender")),
         VesicleColor = factor(VesicleColor, levels = c("blue", "brown", "white")))

# Fix one observation's region classification
MergedCass <- MergedCass %>%
  mutate(longitude = ifelse(id == 156358703, -80.99999, longitude))

# Create filtered datasets for each phenotype
CassSize <- MergedCass %>%
  filter(Size != "n/a") %>%
  mutate(Size = factor(Size, levels = c("small", "medium", "large")))

CassBC <- MergedCass %>%
  filter(BellColor != "n/a")

CassVS <- MergedCass %>%
  filter(VesicleSize != "n/a") %>%
  mutate(VesicleSize = factor(VesicleSize, levels = c("small", "medium", "large")))

CassVsh <- MergedCass %>%
  filter(VesicleShape != "n/a")

CassVC <- MergedCass %>%
  filter(VesicleColor != "n/a")

# Function to create stacked bar plot for phenotypes
create_stacked_bar_plot <- function(data, category_col, category_name, color_palette) {
  binned_data <- data %>%
    group_by(region, !!sym(category_col)) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(region) %>%
    mutate(proportion = count / sum(count)) %>%
    ungroup()
  
  ggplot(binned_data, 
         aes(x = factor(region, levels = c("NorthWest", "NorthEast", "SouthWest", "SouthEast")), 
             y = proportion, fill = !!sym(category_col))) +
    geom_bar(position = "stack", stat = "identity") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_manual(values = color_palette, guide = guide_legend(title = NULL)) +
    scale_x_discrete(labels = c("NW", "NE", "SE", "SW")) +
    theme_classic() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = element_blank(), y = "Proportion", title = category_name)
}

# Add region classification to all datasets
CassSize$region <- categorize_region(CassSize$latitude, CassSize$longitude)
CassBC$region <- categorize_region(CassBC$latitude, CassBC$longitude)
CassVS$region <- categorize_region(CassVS$latitude, CassVS$longitude)
CassVsh$region <- categorize_region(CassVsh$latitude, CassVsh$longitude)
CassVC$region <- categorize_region(CassVC$latitude, CassVC$longitude)

# Create phenotype distribution plots
SizeProp <- create_stacked_bar_plot(CassSize, "Size", "Bell Size", BellSizeColor)
BCProp <- create_stacked_bar_plot(CassBC, "BellColor", "Bell Color", BellColorValues)
VSProp <- create_stacked_bar_plot(CassVS, "VesicleSize", "Vesicle Size", BellSizeColor)
VShProp <- create_stacked_bar_plot(CassVsh, "VesicleShape", "Vesicle Shape", VesShColor)
VCProp <- create_stacked_bar_plot(CassVC, "VesicleColor", "Vesicle Color", BellColorValues)

# Create observation map for phenotype distribution
aggregated_data <- MergedCass %>%
  group_by(longitude = round(longitude, 1), latitude = round(latitude, 1)) %>%
  summarise(count = n(), .groups = 'drop')

Aggregatemap2 <- ggplot() +
  geom_polygon(data = wrld_simpl, aes(x = long, y = lat, group = group), 
               colour = "black", fill = "grey95") +
  geom_point(data = aggregated_data, 
             aes(x = longitude, y = latitude, size = count, color = count),
             alpha = 0.6) +
  scale_size_continuous(range = c(1, 10), name = "Number of\nObservations") +
  scale_color_viridis_c(name = "Number of\nObservations") +
  coord_fixed(xlim = c(-83, -80), ylim = c(24, 30)) +
  theme_classic() +
  geom_vline(xintercept = -81, linetype = "dashed", color = "grey30") +
  geom_hline(yintercept = 26, linetype = "dashed", color = "grey30") +
  theme(legend.position = c(0.5, 0.75),
        axis.text.y = element_text(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color = guide_legend(),
         size = guide_legend())

# Combine plots into Figure 4
PhenotypeRegions <- grid.arrange(Aggregatemap2, SizeProp, BCProp, 
                                 VShProp, VSProp, VCProp, 
                                 ncol = 3,
                                 top = "Distribution of Phenotypes Across Regions")

# Save Figure 4
#ggsave("PhenotypeRegions.svg", PhenotypeRegions)

# Function to perform Fisher's test for habitat associations
perform_habitat_fisher_test <- function(data, category_col) {
  categories <- unique(data[[category_col]])
  
  # Create contingency table with Environment
  contingency_table <- table(data$Environment, data[[category_col]])
  fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)
  cramers_v <- cramerV(contingency_table)
  
  return(list(
    phenotype = category_col,
    p_value = fisher_test$p.value,
    cramers_v = cramers_v
  ))
}

# Run tests for each phenotype against habitat
habitat_tests <- list(
  Size = perform_habitat_fisher_test(CassSize, "Size"),
  BellColor = perform_habitat_fisher_test(CassBC, "BellColor"),
  VesicleSize = perform_habitat_fisher_test(CassVS, "VesicleSize"),
  VesicleShape = perform_habitat_fisher_test(CassVsh, "VesicleShape"),
  VesicleColor = perform_habitat_fisher_test(CassVC, "VesicleColor")
)

# Create summary table
habitat_results <- data.frame(
  Phenotype = names(habitat_tests),
  P_value = sapply(habitat_tests, function(x) x$p_value),
  Cramers_V = sapply(habitat_tests, function(x) x$cramers_v)
)

print(habitat_results)
# Function to analyze phenotype distribution across habitats
analyze_habitat_phenotype <- function(data, phenotype_col) {
  # Calculate proportions for each phenotype in each habitat
  habitat_props <- data %>%
    group_by(Environment) %>%
    count(!!sym(phenotype_col)) %>%
    group_by(Environment) %>%
    mutate(proportion = n/sum(n)) %>%
    ungroup()
  
  # Fisher's test for each habitat
  habitats <- unique(data$Environment)
  results <- list()
  
  for(hab in habitats) {
    subset_data <- data %>% 
      mutate(in_habitat = Environment == hab)
    
    cont_table <- table(subset_data[[phenotype_col]], subset_data$in_habitat)
    fisher_test <- fisher.test(cont_table)
    
    results[[hab]] <- list(
      p_value = fisher_test$p.value,
      table = cont_table
    )
  }
  
  return(list(proportions = habitat_props, tests = results))
}

# Run analysis for each phenotype
shape_analysis <- analyze_habitat_phenotype(CassVsh, "VesicleShape")
color_analysis <- analyze_habitat_phenotype(CassBC, "BellColor")

# View results
print(shape_analysis$proportions %>%
        pivot_wider(names_from = Environment, 
                    values_from = proportion, 
                    id_cols = VesicleShape))

# Statistical analysis of regional phenotype distributions
library(rcompanion)

# Function to perform Fisher's test for each phenotype
perform_fisher_test <- function(data, category_col) {
  categories <- unique(data[[category_col]])
  contingency_table <- table(data$region, data[[category_col]])
  fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)
  cramers_v <- cramerV(contingency_table)
  
  return(list(
    p_value = fisher_test$p.value,
    cramers_v = cramers_v
  ))
}

# Run tests for each phenotype
phenotype_tests <- list(
  Size = perform_fisher_test(CassSize, "Size"),
  BellColor = perform_fisher_test(CassBC, "BellColor"),
  VesicleSize = perform_fisher_test(CassVS, "VesicleSize"),
  VesicleShape = perform_fisher_test(CassVsh, "VesicleShape"),
  VesicleColor = perform_fisher_test(CassVC, "VesicleColor")
)

# Load required packages for MCA
library(FactoMineR)
library(factoextra)

# Prepare data for MCA
df<-read_excel("Cassiopea_Phenotype_iNaturalist_Final.xlsx")
# Step 3: Convert Environment to a factor with the specified levels
nrow(df)
df$Environment <- factor(df$Environment, levels = env_order)
df$VesicleShape <- factor(df$VesicleShape, levels = c("slender", "round"))
df <- subset(df, !is.na(Environment))
df$region <- categorize_region(df$latitude, df$longitude)
nrow(df)
dfstart <- df  # df is your phenotype dataset
dfstart <- subset(dfstart, BellColor != "n/a") %>%
  subset(Size != "n/a") %>%
  subset(VesicleSize != "n/a") %>%
  subset(VesicleShape != "n/a") %>%
  subset(VesicleColor != "n/a") %>%
  subset(Environment != "n/a")

# Remove behavior column and filter for Florida
dfstart2 <- subset(dfstart, select=-c(Behavior))
dfstart3 <- subset(dfstart2, latitude >= 24) %>%
  subset(longitude <= -79.5)

# Prepare MCA data
mca <- dfstart3[1:442, c(4:9, 13)]
mca <- as.data.frame(mca)

# Perform MCA
res.mca <- MCA(mca, quali.sup = 6:7, ncp = 5, graph = TRUE)

# Create MCA visualization
MCA_Final <- fviz_mca_var(res.mca,
                          col.var = "black",
                          repel = TRUE,
                          ggtheme = theme_minimal()) +
  geom_point(aes(color = rownames(res.mca$var$coord)), 
             size = 3) +
  scale_color_manual(values = all_colors) +
  theme_classic() +
  theme(legend.position = "none", 
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        panel.grid = element_line(color = "gray90"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank())
MCA_Final
# Save MCA plot
#ggsave("MCA_Final.svg", MCA_Final)

# Create supplemental figures
# Supplemental Figure 1: Pre-2015 range expansion

JellyHist <- ggplot(AllJellyFl, aes(x = factor(year), fill = genus_group)) + 
  geom_bar(position = "stack") + 
  labs(title = "All Scyphozoa", x = element_blank(), y = element_blank()) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = genus_colors) +
  scale_y_continuous(limits = c(0, 650))

# Cassiopea histogram
CassHist <- ggplot(CassFlNL, aes(x = factor(Year))) + 
  geom_bar(position = "stack", fill = "#1E88E5") + 
  labs(title = "Cassiopea spp.", x = element_blank(), y = "Count of Observations") + 
  theme_classic() + 
  scale_y_continuous(limits = c(0, 650))

# Aurelia histogram
AureliaHist <- ggplot(Aurelia_FL, aes(x = factor(year))) + 
  geom_bar(position = "stack", fill = "#FDB58B") + 
  labs(title = "Aurelia spp.", x = element_blank(), y = element_blank()) + 
  theme_classic() + 
  scale_y_continuous(limits = c(0, 650))

# Create range expansion plots for each group
# Cassiopea range expansion
CasslineNL <- ggplot(CassFlNL, aes(x = Date, y = latitude)) + 
  geom_point(color = "#1E88E5", show.legend = F, size = .5) + 
  theme_classic() + 
  geom_smooth(data = CassFlNL, aes(x = Date, y = latitude), method = "glm", se = TRUE, color = "red") + 
  stat_cor(label.y = 28.4, size = 5) + 
  stat_regline_equation(label.y = 28, size = 5) + 
  geom_line(data = highest_points, aes(group = 1), color = "blue") + 
  scale_y_continuous(limits = c(24,30)) + 
  theme(plot.margin = margin(10, 10, 10, 10), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 22)) + 
  labs(y = "Latitude")

# Aurelia range expansion# CassFlNLAurelia range expansion
Aurelialine <- ggplot(Aurelia_FL, aes(x = Date, y = latitude)) + 
  geom_point(color = "#FDB58B", size = .5) + 
  theme_classic() + 
  geom_smooth(data = Aurelia_FL, aes(x = Date, y = latitude), method = "glm", se = TRUE, color = "red") + 
  stat_cor(label.y = 28.4, size = 5) + 
  stat_regline_equation(label.y = 28, size = 5) + 
  theme(plot.margin = margin(10, 10, 10, 10), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 22)) + 
  scale_y_continuous(limits = c(24,30)) + 
  labs(y = element_blank()) +
  geom_line(data = highest_Aurelia, aes(group = 1), color = "blue") + 
  scale_y_continuous(limits = c(24,30))

# All Scyphozoa range expansion
AllJellyline <- ggplot(AllJellyFl, aes(x = Date, y = latitude)) + 
  geom_point(show.legend = FALSE, aes(color = genus_group ), size = .5) + 
  theme_classic() + 
  geom_smooth(method = "glm", se = TRUE, color = "red") + 
  stat_cor(label.y = 28.4, size = 5) + 
  stat_regline_equation(label.y = 28, size = 5) + 
  theme(plot.margin = margin(10, 10, 10, 10), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 22),
        legend.position = "none") + 
  scale_y_continuous(limits = c(24,30)) + 
  labs(y = element_blank()) +
  geom_line(data = highest_AllJelly, aes(group = 1), color = "blue") + 
  scale_color_manual(values = genus_colors)

SupFig1 <- grid.arrange(CassHist, AureliaHist, JellyHist,
                        CasslineNL, Aurelialine, AllJellyline, 
                        ncol = 3)
#ggsave("SupFig1.svg", SupFig1)

# Supplemental Figure 2: Logger validation
# Compare our logger with NOAA logger
logger_comparison <- ggplot() +
  geom_point(data = SD_Logger, aes(x = DateTime, y = Temp), color = "blue") +
  geom_point(data = NOAA_TB_Logger, aes(x = DateTime, y = Temp), color = "red") +
  theme_classic() +
  labs(x = "Date", y = "Temperature (°C)") +
  scale_y_continuous(limits = c(8, 32))
logger_comparison
#ggsave("SupFig2.svg", logger_comparison)

# Create three separate plots comparing our logger to NOAA data
SDpointClean <- ggplot(SD_Logger, aes(x = DateTime, y = Temp)) + 
  geom_point() + 
  theme_classic() + 
  scale_y_continuous(limits = c(8, 32)) + 
  ggtitle("Shallow Boat Dock (Outliers Removed)") + 
  # Add the 15°C reference line in red
  geom_hline(yintercept = 15, color = "red")

NOAATBpoint <- ggplot(NOAA_TB_Logger, aes(x = DateTime, y = Temp)) + 
  geom_point() + 
  theme_classic() + 
  scale_y_continuous(limits = c(8, 32)) + 
  ggtitle("TB NOAA logger (SAPF1)") + 
  # Add the 15°C reference line in red
  geom_hline(yintercept = 15, color = "red")

# Combine plots into a single figure with panels
grid.arrange(SDpointClean, NOAATBpoint, nrow = 2)


# First convert DateTime to proper format
SD_Logger$DateTime <- as.POSIXct(SD_Logger$DateTime, format="%Y-%m-%d %H:%M:%S")
NOAA_TB_Logger$DateTime <- as.POSIXct(NOAA_TB_Logger$DateTime, format="%Y-%m-%d %H:%M:%S")

# Create plots 
SDpointClean <- ggplot(SD_Logger, aes(x = DateTime, y = Temp)) + 
  geom_point() + 
  theme_classic() + 
  scale_y_continuous(limits = c(8, 32)) + 
  # Format x-axis to show dates every 15 days
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Angle the dates for better readability
  ggtitle("Shallow Boat Dock (Outliers Removed)") + 
  geom_hline(yintercept = 15, color = "red")

NOAATBpoint <- ggplot(NOAA_TB_Logger, aes(x = DateTime, y = Temp)) + 
  geom_point() + 
  theme_classic() + 
  scale_y_continuous(limits = c(8, 32)) + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("TB NOAA logger (SAPF1)") + 
  geom_hline(yintercept = 15, color = "red")

# Calculate correlation 
merged_data2 <- merge(SD_Logger, NOAA_TB_Logger, by = "DateTime", suffixes = c("_SD", "_NOAA"))
merged_data2 <- na.omit(merged_data2)
correlation <- cor(merged_data2$Temp_SD, merged_data2$Temp_NOAA)

# Add correlation to first plot
SDpointClean <- SDpointClean +
  annotate("text", x = min(SD_Logger$DateTime), y = 30, 
           label = paste("r =", round(correlation, 3)),
           hjust = 0)

# Combine plots
grid.arrange(SDpointClean, NOAATBpoint, nrow = 2)




# Supplemental Figure 3: Temperature ranges
# Create merged temperature dataframe
jellyfish_with_loggers$date <- as.Date(jellyfish_with_loggers$date)
merged_temp <- jellyfish_with_loggers %>%
  left_join(all_loggers_combined, by = c("date", "nearest_logger_id" = "logger_id"))

# Add region classification
merged_temp <- merged_temp %>%
  mutate(region = mapply(categorize_region, lat.x, lon.x)) %>%
  mutate(region = factor(region, 
                         levels = c("NorthWest", "NorthEast", "SouthWest", "SouthEast")))
obs_counts <- merged_temp %>%
  group_by(region) %>%
  summarize(n = n())

Yearly_logger_data <- all_loggers_combined %>%
  mutate(Year = as.integer(format(date, "%Y")),
         region = mapply(categorize_region, lat, lon)) %>%
  filter(Year < 2024) %>%  
  group_by(Year, region) %>%
  summarise(yearly_avg_temp = mean(mean_temp, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(region = factor(region, 
                         levels = c("NorthEast", "NorthWest", "SouthEast", "SouthWest")))

TemperatureBox_Jellyfish <- ggplot(data = merged_temp, aes(x = region, y = mean_temp, fill = region)) +
  geom_boxplot() +
  geom_text(data = obs_counts, aes(x = region, y = 38, label = paste0("n = ", n)), inherit.aes = FALSE) +
  geom_signif(
    comparisons = list(c("NorthWest", "SouthWest"), c("NorthEast", "NorthWest"), c("SouthEast", "NorthWest")), # Modify pairs as needed
    map_signif_level = TRUE,
    y_position = c(34, 33, 35)) +
  scale_y_continuous(
    name = "Temperature (°C)", 
    breaks = seq(8, ceiling(max(all_loggers_combined$mean_temp)), by = 4)  ) +
  labs(
    title = "Temperature Ranges Experienced by Jellyfish Across Regions",
    x = "Region",
    y = "Temperature (°C)"  ) +
  theme_classic() + scale_fill_manual(values = regioncolors)
TemperatureBox_Jellyfish

all_loggers_combined <- all_loggers_combined %>% #add region
  mutate(region = mapply(categorize_region, lat, lon))

obs_counts_new <- all_loggers_combined %>%
  group_by(region) %>%
  summarise(n = n())

anova_result2 <- aov(mean_temp ~ region, data = all_loggers_combined)
tukey_result2 <- TukeyHSD(anova_result2)

all_loggers_combined$region <- factor(all_loggers_combined$region, levels = c("NorthWest", "NorthEast", "SouthWest", "SouthEast"))
# Generate the plot using all_loggers_combined
TemperatureBox_Loggers <- ggplot(data = all_loggers_combined, aes(x = region, y = mean_temp, fill = region)) +
  geom_boxplot() +
  geom_text(data = obs_counts_new, aes(x = region, y = 40, label = paste0("n = ", n)), inherit.aes = FALSE) +
  geom_signif(
    comparisons = list(c("SouthEast", "SouthWest")), # Modify pairs as needed
    map_signif_level = TRUE,
    y_position = c(37)) +
  scale_y_continuous(
    name = "Temperature (°C)", 
    breaks = seq(8, ceiling(max(all_loggers_combined$mean_temp)), by = 4)
  ) +
  labs(
    title = "Temperature Range Across Regions (Full Temp Data)",
    x = "Region",
    y = "Temperature (°C)"
  ) +
  theme_classic() + scale_fill_manual(values = regioncolors)
TemperatureBox_Loggers

# Supplemental Figure 6: Temperature by observation
TemplineNL_Region <- ggplot(merged_temp, aes(x = date, y = mean_temp)) + 
  geom_point() + 
  theme_classic() + 
  geom_smooth(aes(x = date, y = mean_temp), method = "glm", se = TRUE, color = "red") + 
  stat_cor(label.y = 17) + 
  stat_regline_equation(label.y = 13) + 
  facet_wrap(~ region) +
  geom_line(data = Yearly_logger_data, 
            aes(x = as.Date(paste0(Year, "-01-01")), 
                y = yearly_avg_temp), 
            color = "blue", 
            linetype = "dashed", 
            size = 0.8) +
  labs(title = "Daily Temperature of Observations by Region",
       y = "Mean Daily Temperature (°C)")
TemplineNL_Region
#ggsave("SupFig5.svg", TemplineNL_Region)

#suplementary figure 8
mca_colors <- c(
  # Bell Colors
  "BellColor_blue" = "#1E88E5",
  "BellColor_brown" = "#6C3B1C", 
  "BellColor_white" = "Grey",
  
  # Size Colors
  "Size_small" = "#FFE0B2", 
  "Size_medium" = "#FFB74D", 
  "Size_large" = "#F57C00",
  
  # Region Colors
  "NorthWest" = "#D50A0A",
  "SouthWest" = "#8BC34A",
  "SouthEast" = "#008E97",
  "NorthEast" = "#F4B831",
  
  # Vesicle Shape Colors
  "slender" = "#7FC97F",
  "round" = "#BEAED4"
)
# Create the plot with custom colors
fviz_ellipses(res.mca, 
              c("BellColor", "Size", "region", "VesicleShape"), 
              geom = "point",
              palette = mca_colors,) +
  theme_classic() + theme(legend.position = "none")
fviz_ellipses(res.mca, "Environment", geom = "point",) + theme_classic()

# Create summary statistics for phenotypes by region
summary_stats <- df %>%
  group_by(region) %>%
  summarise(
    total_observations = n(),
    blue_bell_prop = mean(BellColor == "blue", na.rm = TRUE),
    brown_bell_prop = mean(BellColor == "brown", na.rm = TRUE),
    slender_vesicle_prop = mean(VesicleShape == "slender", na.rm = TRUE)
  )

# Create summary statistics for temperature by region
temp_summary <- all_loggers_combined %>%
  group_by(region) %>%
  summarise(
    mean_temp = mean(mean_temp, na.rm = TRUE),
    sd_temp = sd(mean_temp, na.rm = TRUE),
    n_cold_days = sum(mean_temp < 15, na.rm = TRUE)
  )

# Export summary stYear# Export summary statistics
write.csv(summary_stats, "phenotype_summary_stats.csv")
write.csv(temp_summary, "temperature_summary_stats.csv")

# Perform tests for each phenotype
perform_fisher_test <- function(data, category_col) {
  categories <- unique(data[[category_col]])
  results <- data.frame(
    Phenotype = character(),
    Category = character(),
    P_value = numeric(),
    NE_Prop = numeric(),
    NW_Prop = numeric(),
    SE_Prop = numeric(),
    SW_Prop = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (cat in categories) {
    contingency_table <- table(data$region, data[[category_col]] == cat)
    fisher_test <- fisher.test(contingency_table)
    
    ne_prop <- sum(data$region == "NorthEast" & data[[category_col]] == cat) / sum(data$region == "NorthEast")
    nw_prop <- sum(data$region == "NorthWest" & data[[category_col]] == cat) / sum(data$region == "NorthWest")
    se_prop <- sum(data$region == "SouthEast" & data[[category_col]] == cat) / sum(data$region == "SouthEast")
    sw_prop <- sum(data$region == "SouthWest" & data[[category_col]] == cat) / sum(data$region == "SouthWest")
    
    results <- rbind(results, data.frame(
      Phenotype = category_col,
      Category = cat,
      P_value = fisher_test$p.value,
      NE_Prop = ne_prop,
      NW_Prop = nw_prop,
      SE_Prop = se_prop,
      SW_Prop = sw_prop))
  }
  
  # Add Cramer's V calculation
  contingency_table <- table(data$region, data[[category_col]])
  cramers_v <- cramerV(contingency_table)
  results$cramers_v <- cramers_v
  return(results)
}

# Then run the tests
size_results <- perform_fisher_test(CassSize, "Size")
bc_results <- perform_fisher_test(CassBC, "BellColor")
vs_results <- perform_fisher_test(CassVS, "VesicleSize")
vsh_results <- perform_fisher_test(CassVsh, "VesicleShape")
vc_results <- perform_fisher_test(CassVC, "VesicleColor")

# Combine all results
all_results <- rbind(size_results, bc_results, vs_results, vsh_results, vc_results)

# Format the table for display
formatted_table <- all_results %>%
  mutate(P_value = format.pval(P_value, digits = 3),
         NE_Prop = sprintf("%.2f", NE_Prop),
         NW_Prop = sprintf("%.2f", NW_Prop),
         SE_Prop = sprintf("%.2f", SE_Prop),
         SW_Prop = sprintf("%.2f", SW_Prop))

# Display the formatted table
kable(formatted_table, 
      col.names = c("Phenotype", "Category", "P-value", "NE Proportion", 
                    "NW Proportion", "SE Proportion", "SW Proportion", "Crammer's V"), 
      align = c('l', 'l', 'r', 'r', 'r', 'r', 'r'))


##Suplimentary figure 5 

SizeDen <- ggplot(CassSize, aes(y = latitude, fill = Size))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = BellSizeColor)+ #Add Colors
  xlab("Bell Size")+ #labels 
  facet_wrap(CassSize$Size, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = "none", axis.title.y = element_blank()) + scale_y_continuous(limits = c(24,30))
SizeDen

BCDen <- ggplot(CassBC, aes(y = latitude, fill = BellColor))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = BellColorValues)+ #Add Colors
  xlab("Bell Color")+ #labels 
  facet_wrap(CassBC$BellColor, strip.position = "top") + theme_classic() + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = "none", axis.title.y = element_blank()) + scale_y_continuous(limits = c(24,30))
BCDen

VSDen <- ggplot(CassVS, aes(y = latitude, fill = VesicleSize))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = VesicleSizeColor)+ #Add Colors
  xlab("Vesicle Size")+ #labels 
  facet_wrap(CassVS$VesicleSize, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = "none", axis.title.y = element_blank()) + scale_y_continuous(limits = c(24,30))
VSDen

VShDen <- ggplot(CassVsh, aes(y = latitude, fill = VesicleShape))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = VesShColor)+ #Add Colors
  xlab("Vesicle Shape")+ #labels 
  facet_wrap(CassVsh$VesicleShape, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = "none", axis.title.y = element_blank()) + scale_y_continuous(limits = c(24,30))
VShDen 

VCDen <- ggplot(CassVC, aes(y = latitude, fill = VesicleColor))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = BellColorValues)+ #Add Colors
  xlab("Vesicle Color")+ #labels 
  facet_wrap(CassVC$VesicleColor, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = "none", axis.title.y = element_blank()) + scale_y_continuous(limits = c(24,30))
VCDen

grid.arrange( SizeDen, BCDen, VSDen, VShDen, VCDen, ncol = 5)


jellyfish_with_loggers$date <- as.Date(jellyfish_with_loggers$date)

merged_data <- jellyfish_with_loggers %>%
  left_join(all_loggers_combined, by = c("date", "nearest_logger_id" = "logger_id"))

merged_data$Size <- factor(merged_data$Size, levels = c("small", "medium", "large", "NA"))
merged_data$VesicleSize <- factor(merged_data$VesicleSize, levels = c("small", "medium", "large", "n/a"))
merged_data$BellColor <- factor(merged_data$BellColor, levels = c("blue", "brown", "white"))
merged_data$VesicleShape <- factor(merged_data$VesicleShape, levels = c("round", "slender"))
merged_data$VesicleColor <- factor(merged_data$VesicleColor, levels = c("blue", "brown", "white"))

merged_data <- merged_data %>%
  group_by(nearest_logger_id, year = format(date, "%Y")) %>%
  mutate(mean_yearly_temp = mean(mean_temp, na.rm = TRUE)) %>%
  ungroup()
merged_data <- merged_data %>%
  group_by(nearest_logger_id, month = format(date, "%Y,%m")) %>%
  mutate(mean_monthly_temp = mean(mean_temp, na.rm = TRUE)) %>%
  ungroup()

##
BellColorValues <- c(brown = "#6C3B1C",blue = "#1E88E5",white = "Beige")
BellColorValuesBlack <- c(brown = "#6C3B1C",blue = "#1E88E5",white = "Black")
SizeColor<- brewer.pal(n=3, name = "YlOrBr")
VesShColor <- brewer.pal(n=3, name = "Accent")

CassSize <- subset(merged_data, Size != ('n/a'))
CassSize$Size <- factor(CassSize$Size, levels =  c("small", "medium", "large"))
CassBC <- subset(merged_data, BellColor != ('n/a'))
CassVS<- subset(merged_data, VesicleSize != ('n/a'))
CassVS$VesicleSize <- factor(CassVS$VesicleSize, levels =  c("small", "medium", "large"))
CassVsh<- subset(merged_data, VesicleShape != ('n/a'))
CassVC<- subset(merged_data, VesicleColor != ('n/a'))

#mean_temp
SizeDen <- ggplot(CassSize, aes(y = mean_temp, fill = Size))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = SizeColor)+ #Add Colors
  xlab("Bell Size")+ #labels 
  facet_wrap(CassSize$Size, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none")
SizeDen

BCDen <- ggplot(CassBC, aes(y = mean_temp, fill = BellColor))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = BellColorValues)+ #Add Colors
  xlab("Bell Color")+ #labels 
  facet_wrap(CassBC$BellColor, strip.position = "top") + theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none", axis.title.y = element_blank())
BCDen

VSDen <- ggplot(CassVS, aes(y = mean_temp, fill = VesicleSize))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = SizeColor)+ #Add Colors
  xlab("Vesicle Size")+ #labels 
  facet_wrap(CassVS$VesicleSize, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none", axis.title.y = element_blank())
VSDen

VShDen <- ggplot(CassVsh, aes(y = mean_temp, fill = VesicleShape))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = VesShColor)+ #Add Colors
  xlab("Vesicle Shape")+ #labels 
  facet_wrap(CassVsh$VesicleShape, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none", axis.title.y = element_blank())
VShDen 

VCDen <- ggplot(CassVC, aes(y = mean_temp, fill = VesicleColor))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = BellColorValues)+ #Add Colors
  xlab("Vesicle Color")+ #labels 
  facet_wrap(CassVC$VesicleColor, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none", axis.title.y = element_blank())
VCDen

TempDensity <- grid.arrange(SizeDen, BCDen, VSDen, VShDen, VCDen, ncol = 5)

#mean__yearly_temp
SizeDen <- ggplot(CassSize, aes(y = mean_yearly_temp, fill = Size))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = SizeColor)+ #Add Colors
  xlab("Bell Size")+ #labels 
  facet_wrap(CassSize$Size, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none")
SizeDen

BCDen <- ggplot(CassBC, aes(y = mean_yearly_temp, fill = BellColor))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = BellColorValues)+ #Add Colors
  xlab("Bell Color")+ #labels 
  facet_wrap(CassBC$BellColor, strip.position = "top") + theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none", axis.title.y = element_blank())
BCDen

VSDen <- ggplot(CassVS, aes(y = mean_yearly_temp, fill = VesicleSize))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = SizeColor)+ #Add Colors
  xlab("Vesicle Size")+ #labels 
  facet_wrap(CassVS$VesicleSize, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none", axis.title.y = element_blank())
VSDen

VShDen <- ggplot(CassVsh, aes(y = mean_yearly_temp, fill = VesicleShape))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = VesShColor)+ #Add Colors
  xlab("Vesicle Shape")+ #labels 
  facet_wrap(CassVsh$VesicleShape, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none", axis.title.y = element_blank())
VShDen 

VCDen <- ggplot(CassVC, aes(y = mean_yearly_temp, fill = VesicleColor))+
  geom_density(alpha = 1, size = .5, )+ #create Boxplot
  scale_fill_manual(values = BellColorValues)+ #Add Colors
  xlab("Vesicle Color")+ #labels 
  facet_wrap(CassVC$VesicleColor, strip.position = "top") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90),legend.position = "none", axis.title.y = element_blank())
VCDen

grid.arrange(SizeDen, BCDen, VSDen, VShDen, VCDen, ncol = 5)

# Supplemental Figure 8: Regional phenotype distributions
df<-read_excel("Cassiopea_Phenotype_iNaturalist_Final.xlsx")

nrow(df)
df$Environment <- factor(df$Environment, levels = env_order)
df$VesicleShape <- factor(df$VesicleShape, levels = c("slender", "round"))
df <- subset(df, !is.na(Environment))
df$region <- categorize_region(df$latitude, df$longitude)
env_counts <- df %>%
  group_by(Environment) %>%
  summarise(count = n())

RegionxEnvironment <- ggplot(df, aes(Environment, fill = region)) +
  geom_bar(position = "fill") +
  labs(fill = "Region", y = "Proportion", x = "Environment") +
  scale_fill_manual(values = regioncolors) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
RegionxEnvironment
#ggsave("SupFig3.svg", RegionxEnvironment)

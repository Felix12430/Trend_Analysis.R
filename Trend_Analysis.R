# Drought Trend Analysis in R
# Time series analysis of composite drought indices
# Mann-Kendall test and Sen's slope estimator for trend detection
# Author: [FELIX WAIGIRI KIRUKI]
# STEP 0: Install & Load Libraries
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(Kendall)) install.packages("Kendall")
if(!require(zyp)) install.packages("zyp")
if(!require(ggplot2)) install.packages("ggplot2")
library(readr)     # For reading CSV files
library(dplyr)     # For data manipulation
library(lubridate) # For handling dates
library(Kendall)   # For Mann-Kendall test
library(zyp)       # For Sen's slope estimator
library(ggplot2)   # For visualization
# STEP 1: Load the Data
# Load SPI data (adjust file path and column names as needed)
spi_data <- read_csv("E:\\PROJECT\\final_spi.csv")
# Load VCI and TCI data (assuming similar structure)
vci_data <- read_csv("E:\\PROJECT\\kiruki project\\final_vci.csv")
tci_data <- read_csv("E:\\PROJECT\\kiruki project\\final_tci.csv")
# Inspect the first few rows
head(spi_data)
head(vci_data)
head(tci_data)
# Convert the Date column to Date class for each dataset.
# (For spi_data, we rename the date column if needed and adjust the format)
spi_data <- spi_data %>%
  rename(Date = `system:time_start`) %>%  # Change if needed
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Year = year(Date),
         Month = month(Date, label = TRUE))
vci_data <- vci_data %>%
  rename(Date = `system:time_start`) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))  %>%
  mutate(Year = year(Date),
         Month = month(Date, label = TRUE))
tci_data <- tci_data %>%
  rename(Date = `system:time_start`) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y")) %>%
  mutate(Year = year(Date),
         Month = month(Date, label = TRUE))
# STEP 2: Merge the Data
# Merge the datasets by Date (inner join to keep common dates)
drought_data <- spi_data %>%
  inner_join(vci_data, by = "Date", suffix = c("_spi", "_vci")) %>%
  inner_join(tci_data, by = "Date")  # TCI column remains as is
# Ensure the merged data has the necessary columns:
# We'll assume the following columns exist: Date, Year, Month, SPI-3 (from spi_data),
# VCI (from vci_data) and TCI (from tci_data).
# (Adjust column names below if different. In this example, I assume spi_data contains a column named "SPI-3".)
head(drought_data)
# STEP 3: Standardize Each Indicator and Create a Composite Index
# Standardize each index using scale() to compute z-scores.
drought_data <- drought_data %>%
  mutate(SPI_std = as.numeric(scale(`SPI-3`)),
         VCI_std = as.numeric(scale(VCI)),
         TCI_std = as.numeric(scale(TCI)))
# Create a composite drought index as the average of the three standardized indicators.
# (If one of the indices needs to be reversed so that higher values mean wetter conditions, do that here.)
drought_data <- drought_data %>%
  mutate(Composite = rowMeans(select(., SPI_std, VCI_std, TCI_std), na.rm = TRUE))
# STEP 4: Define Drought Events and Compute Annual Metrics
# Define a drought event: Here we use SPI <= -0.1 for demonstration.
# You can similarly define drought events for the composite index if desired.
drought_data <- drought_data %>%
  mutate(Drought_SPI = ifelse(`SPI-3` <= -0.1, 1, 0),
         Deficit_SPI = ifelse(`SPI-3` <= -0.1, abs(`SPI-3` + 1), 0),
         Drought_Composite = ifelse(Composite <= -0.1, 1, 0),
         Deficit_Composite = ifelse(Composite <= -0.1, abs(Composite + 1), 0))
# Aggregate annual metrics: frequency (count of events) and severity (cumulative deficit).
annual_drought <- drought_data %>%
  group_by(Year) %>%
  summarise(Frequency_SPI = sum(Drought_SPI, na.rm = TRUE),
            Severity_SPI = sum(Deficit_SPI, na.rm = TRUE),
            Frequency_Composite = sum(Drought_Composite, na.rm = TRUE),
            Severity_Composite = sum(Deficit_Composite, na.rm = TRUE))
print(annual_drought)
# STEP 5: Trend Analysis (Mann-Kendall & Sen's Slope)
# Mann-Kendall Test for Annual Metrics
# SPI-based metrics
mk_freq_spi <- MannKendall(annual_drought$Frequency_SPI)
mk_severity_spi <- MannKendall(annual_drought$Severity_SPI)
# Composite-based metrics
mk_freq_comp <- MannKendall(annual_drought$Frequency_Composite)
mk_severity_comp <- MannKendall(annual_drought$Severity_Composite)
cat("Mann-Kendall Test for Annual SPI-based Frequency:\n")
print(mk_freq_spi)
cat("Mann-Kendall Test for Annual SPI-based Severity:\n")
print(mk_severity_spi)
cat("Mann-Kendall Test for Annual Composite-based Frequency:\n")
print(mk_freq_comp)
cat("Mann-Kendall Test for Annual Composite-based Severity:\n")
print(mk_severity_comp)
# Sen's Slope Estimator
sen_freq_spi <- zyp.sen(Frequency_SPI ~ Year, data = annual_drought)
sen_severity_spi <- zyp.sen(Severity_SPI ~ Year, data = annual_drought)
sen_freq_comp <- zyp.sen(Frequency_Composite ~ Year, data = annual_drought)
sen_severity_comp <- zyp.sen(Severity_Composite ~ Year, data = annual_drought)
cat("Sen's Slope for Annual SPI-based Frequency:\n")
print(sen_freq_spi)
cat("Sen's Slope for Annual SPI-based Severity:\n")
print(sen_severity_spi)
cat("Sen's Slope for Annual Composite-based Frequency:\n")
print(sen_freq_comp)
cat("Sen's Slope for Annual Composite-based Severity:\n")
print(sen_severity_comp)
# STEP 6: Visualizations
# 6a. Line Chart for SPI, VCI, TCI, and Composite Index Over Time
p_line <- ggplot(drought_data, aes(x = Date)) +
  geom_line(aes(y = `SPI-3`, color = "SPI-3"), size = 0.7) +
  geom_line(aes(y = VCI, color = "VCI"), size = 0.7) +
  geom_line(aes(y = TCI, color = "TCI"), size = 0.7) +
  geom_line(aes(y = Composite, color = "Composite"), size = 1.2, linetype = "dashed") +
  labs(title = "Time Series of Drought Indicators and Composite Index",
       x = "Date", y = "Index Value",
       color = "Indicator") +
  theme_minimal()
print(p_line)
# 6b. Bar Chart for Annual Drought Frequency (Composite-based)
p_bar <- ggplot(annual_drought, aes(x = as.factor(Year), y = Frequency_Composite)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Annual Drought Frequency (Composite Index)",
       x = "Year", y = "Number of Drought Events") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_bar)
# 6c. Boxplot for Seasonal Variability of SPI
p_box <- ggplot(drought_data, aes(x = Month, y = `SPI-3`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Seasonal Distribution of SPI",
       x = "Month", y = "SPI-3") +
  theme_minimal()
print(p_box)
# 6d. Scatterplot with Regression Line for Annual Composite Frequency
p_scatter_freq <- ggplot(annual_drought, aes(x = Year, y = Frequency_Composite)) +
  geom_point(color = "purple", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Trend in Annual Drought Frequency (Composite)",
       x = "Year", y = "Frequency") +
  theme_minimal()
print(p_scatter_freq)
# 6e. Scatterplot with Regression Line for Annual Composite Severity
p_scatter_sev <- ggplot(annual_drought, aes(x = Year, y = Severity_Composite)) +
  geom_point(color = "darkorange", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Trend in Annual Drought Severity (Composite)",
       x = "Year", y = "Cumulative Drought Deficit") +
  theme_minimal()
print(p_scatter_sev)
# STEP 7: Save a Report Summary
report_text <- capture.output({
  cat("Drought Trend Analysis Report\n")
  cat("-------------------------------\n\n")
  cat("Mann-Kendall Test for Annual SPI-based Frequency:\n")
  print(mk_freq_spi)
  cat("\nSen's Slope for SPI-based Frequency:\n")
  print(sen_freq_spi)
  cat("\n\nMann-Kendall Test for Annual SPI-based Severity:\n")
  print(mk_severity_spi)
  cat("\nSen's Slope for SPI-based Severity:\n")
  print(sen_severity_spi) 
  cat("\n\nMann-Kendall Test for Annual Composite-based Frequency:\n")
  print(mk_freq_comp)
  cat("\nSen's Slope for Composite-based Frequency:\n")
  print(sen_freq_comp)
  cat("\n\nMann-Kendall Test for Annual Composite-based Severity:\n")
  print(mk_severity_comp)
  cat("\nSen's Slope for Composite-based Severity:\n")
  print(sen_severity_comp)
})
writeLines(report_text, "Drought_Trend_Analysis_Report.txt")
cat("Report saved as 'Drought_Trend_Analysis_Report.txt'\n")

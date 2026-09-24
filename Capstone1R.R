## DS 201 capstone 1 - Getting into real estate
install.packages("tidyverse")
install.packages("knitr")
install.packages("kableExtra")
library(tidyverse)
library(knitr)
library(kableExtra)

# load data directly from FHFA for reproducibility
hpi_master <- read.csv("https://www.fhfa.gov/hpi/download/monthly/hpi_master.csv")

# structure
str(hpi_master)

# unique categories for the key categorical fields
sapply(hpi_master[c("hpi_type","hpi_flavor","frequency","level")], unique)

# year range and number of unique places
range(hpi_master$yr)
n_distinct(hpi_master$place_id)

# type/flavor/geography breakdowns
table(hpi_master$hpi_type)
table(hpi_master$hpi_flavor)
table(hpi_master$hpi_type, hpi_master$hpi_flavor)

hpi_master %>%
  distinct(level, place_name, place_id) %>%
  count(level)



## ---- Part 2. Data summarizing & Initial Insights ----

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## ---- Summary statistics for numeric variables ----
numeric_summary <- hpi_master %>%
  summarise(across(
    c(yr, period, index_nsa, index_sa, rstderr),
    list(
      Mean   = ~mean(.x, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      SD     = ~sd(.x, na.rm = TRUE),
      Min    = ~min(.x, na.rm = TRUE),
      Max    = ~max(.x, na.rm = TRUE),
      NAs    = ~sum(is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = c("Variable", "Stat"), names_sep = "__") %>%
  pivot_wider(names_from = Stat, values_from = value)

kable(numeric_summary, digits = 2, caption = "Summary Statistics for Numeric Variables")

## ---- Missing value counts, all columns ----
missing_summary <- hpi_master %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count") %>%
  mutate(Missing_Pct = round(100 * Missing_Count / nrow(hpi_master), 1)) %>%
  arrange(desc(Missing_Count))

kable(missing_summary, caption = "Missing Values by Column")

## ---- Frequency tables for categorical variables ----
kable(hpi_master %>% count(hpi_type, sort = TRUE), caption = "Observations by hpi_type")
kable(hpi_master %>% count(hpi_flavor, sort = TRUE), caption = "Observations by hpi_flavor")
kable(hpi_master %>% count(level, sort = TRUE), caption = "Observations by Geography Level")
kable(hpi_master %>% count(frequency, sort = TRUE), caption = "Observations by Frequency")

## ---- Visualization: distribution of index_nsa ----
ggplot(hpi_master, aes(x = index_nsa)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Non-Seasonally-Adjusted HPI",
       x = "Index (NSA)", y = "Count") +
  theme_minimal()

## ---- Visualization: index_nsa by hpi_type ----
ggplot(hpi_master, aes(x = hpi_type, y = index_nsa, fill = hpi_type)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "HPI (NSA) by Index Type", x = NULL, y = "Index (NSA)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## ---- Visualization: national HPI trend over time ----
national_trend <- hpi_master %>%
  filter(place_id == "USA", hpi_type == "traditional", frequency == "monthly") %>%
  mutate(date = as.Date(paste(yr, period, "01", sep = "-")))

ggplot(national_trend, aes(x = date, y = index_nsa, color = hpi_flavor)) +
  geom_line(linewidth = 0.8) +
  labs(title = "U.S. National HPI Over Time (Traditional, Monthly)",
       x = NULL, y = "Index (NSA)", color = "Flavor") +
  theme_minimal()
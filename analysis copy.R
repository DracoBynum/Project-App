#install.packages("wbstats")
library("wbstats")
#install.packages("ggplot2")
library("ggplot2")
library("dplyr")
options(scipen = 999)

## 2.2 some of the summary analysis ##

# Overall summary analysis

# avg total crimes per year from 1975 to present

library("dplyr")

library("tidyr")

library("wbstats")

crime_df <- read.csv("Crime_Data.csv", stringsAsFactors = FALSE)

sample_df2 <- crime_df %>% 
  filter(Precinct != "UNKNOWN") %>% 
  select(Reported.Date, Crime.Subcategory, Precinct, Neighborhood) %>%
  head(10)

avg_total_crimes <- crime_df %>%
  mutate(Year = substr(Reported.Date, 7, 10)) %>%
  group_by(Year) %>%
  arrange(Year) %>% 
  summarize(num_crime = length(Crime.Subcategory)) %>% 
  summarize(avg_per_year = mean(num_crime)) %>% 
  select(avg_per_year) %>% 
  pull(1) %>% 
  round(digits = 2)

# outlier data for years
low_crime <- crime_df %>%
  mutate(Year = substr(Reported.Date, 7, 10)) %>%
  group_by(Year) %>%
  arrange(Year) %>% 
  summarize(num_crime = length(Crime.Subcategory)) %>% 
  filter(num_crime == min(num_crime)) 
  

high_crime <- crime_df %>% 
  mutate(Year = substr(Reported.Date, 7, 10)) %>%
  group_by(Year) %>%
  arrange(Year) %>% 
  summarize(num_crime = length(Crime.Subcategory)) %>% 
  filter(num_crime == max(num_crime)) 

# plot for year data 
year_data <- crime_df %>%
  mutate(Year = substr(Reported.Date, 7, 10)) %>%
  group_by(Year) %>%
  arrange(Year) %>% 
  summarize(num_crime = length(Crime.Subcategory))
#View(year_data)

# Plot also shows some outliers can just point them out
year_data_plot <- ggplot(data = year_data) +
  geom_point(aes(x = Year, y = num_crime), size = 7, color = "#ff9966") + 
  geom_text(aes(x = Year, y = num_crime, label = year_data$num_crime), color = "black", size = 2) +
  labs(title = "Amount of Crimes Per Year", x = "Year", y = "Amount of Crimes") + 
  theme(axis.text.x = element_text(size = 6, angle = 45))

# List for summary analysis
summary_analysis_2.2 <- list(avg_total_crimes, low_crime, high_crime)


#Section 2. Data Description
# Global homocide rate data set: 
# 1. Rmd file 
# 2. Rmd file 
# 3. 
wb_homocide <- wb(country = "countries_only", indicator = c("VC.IHR.PSRC.P5"), mrv = 26) %>% 
  mutate(date = as.numeric(date)) %>% 
  arrange(value)

  

sample_data <- sample_n(wb_homocide, 10)

# 4. Rmd file
# Subsection 2.2 Summary Analysis

# 1.
range_global_homocide <- max(wb_homocide$date) - min(wb_homocide$date)
mean_murder_rate <- round(mean(wb_homocide$value), 2)
max_murder_rate <- round(max(wb_homocide$value), 2)
# El Salvador, 1995

country_murder_rate <- wb_homocide %>% 
  arrange(value) %>% 
  top_n(5, value) %>% 
  nrow()
# El Salvador

min_murder_rate <- wb_homocide[91, "value"]
#Cyrpus, 2002
  


# 2.
plot_homicide <- ggplot(data = sample_data) +
  geom_col(mapping = aes(x = country, y = value, color = country)) +
  coord_flip() +
  labs(title = "Sample plot of Homicide Rate (per 100,000 people) in different countries", x = "Country", y = "Homiicide Rate (per 100,000 people)") +
  theme(legend.position = "none") 
# 3. Rmd File


#Subsection 2.1 

#Subsection 2.2 Data Set Summary Analysis

# Seattle 2018 Data Set 

seattle_summary <- read.csv('Seattle_2018.csv', stringsAsFactors=FALSE) %>%
  select(Crime.Subcategory) 
  
seattle_summary <- group_by(seattle_summary, Crime.Subcategory) %>%
  summarize(crime_amount = n()) %>%
  arrange(crime_amount)



seattle_2018_summary_plot <- ggplot(data = seattle_summary) +
  geom_col(mapping = aes(x = reorder(Crime.Subcategory, crime_amount), y = crime_amount)) +
  labs(title = "Seattle 2018 Distribution Summary") +
  labs(x = "Crime Category") +
  labs(y = "Crimes Reported") +
  theme(axis.text.x = element_text(size = 6, angle = 90))

print(seattle_2018_summary_plot)


# Denver 2018 Data Set 

# Denver 2018 Summary Analysis
denver_2018 <- read.csv("Denver_2018.csv", stringsAsFactors = FALSE) %>%
  select(c(FIRST_OCCURRENCE_DATE, OFFENSE_CATEGORY_ID))
denver_sample <- sample_n(denver_2018, 10)



denver_summary <- read.csv('Denver_2018.csv', stringsAsFactors=FALSE) %>%
  select(OFFENSE_CATEGORY_ID) 

denver_summary <- group_by(denver_summary, OFFENSE_CATEGORY_ID) %>%
  summarize(crime_amount = n())



denver_2018_summary_plot <- ggplot(data = denver_summary) +
  geom_col(mapping = aes(x = reorder(OFFENSE_CATEGORY_ID, crime_amount), y = crime_amount)) +
  labs(title = "Denver 2018 Distribution Summary") +
  labs(x = "Crime Category") +
  labs(y = "Crimes Reported") +
  theme(axis.text.x = element_text(size = 10, angle = 90)) 


print(denver_2018_summary_plot)



# D.C. 2018 Data Set

# Washington DC 2018 Summary Analysis
dc_2018 <- read.csv("DC_2018.csv", stringsAsFactors = FALSE) %>%
  select(c(START_DATE, offense.text))
dc_sample <- sample_n(dc_2018, 10)




dc_summary <- read.csv('DC_2018.csv', stringsAsFactors=FALSE) %>%
  select(offense.text) 

dc_summary <- group_by(dc_summary, offense.text) %>%
  summarize(crime_amount = n())




dc_2018_summary_plot <- ggplot(data = dc_summary) +
  geom_col(mapping = aes(x = reorder(offense.text, crime_amount), y = crime_amount)) +
  labs(title = "Washington D.C. 2018 Distribution Summary") +
  labs(x = "Crime Category") +
  labs(y = "Crimes Reported") +
  theme(axis.text.x = element_text(size = 11, angle = 90))


print(dc_2018_summary_plot)




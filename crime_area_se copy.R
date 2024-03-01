library("dplyr")
library("tidyr")
library("ggplot2")
options(scipen = 999)

# Data Wrangling for data science question: From the past 10 years, what is the most dangerous area (cardinal direction),
# of seattle based on the crime rate per year? 
########################################################################

# data frame of seattle crime
crime_df <- read.csv("Crime_Data.csv", stringsAsFactors = FALSE)

#View(crime_df)

#10 years of crime data in seattle from 2009
updated_df <- crime_df %>% 
  mutate(Year = substr(Reported.Date, 7, 10)) %>% 
  filter(Year >= 2009) %>% 
  filter(Year <= 2019) %>% 
  select(Year, Crime.Subcategory, Precinct, Neighborhood)

#View(updated_df)

by_area <- updated_df %>% 
  group_by(Precinct) %>% 
  arrange(Precinct)

#View(by_area)

# filtering for all directions except the blank one with extranious infromation
total_crime_area <- by_area %>% 
  summarize(Total = length(Precinct)) %>% 
  filter(Precinct != "")

#View(total_crime_area)

# plot of the crime area df 
crime_area_plot <- ggplot(data = total_crime_area) +
  geom_col(mapping = aes(x = reorder(Precinct, Total), y = Total, fill = reorder(Precinct, Total))) +
  scale_fill_brewer(palette =  "Pastel2") + 
  labs(title = "Most Dangerous Area in Seattle by Crime Rate", x = "Cardinal Direction", y = "Amount of Crimes", fill = "Cardinal Direction") +
  theme_classic() +
  theme(legend.position = c(.20, .70), legend.background = element_rect(fill = "grey", size = 0.5, linetype = "solid", colour = "black"), axis.text.x = element_text(size = 10)) 
  
# sample_df for the whole project
sample_df <- crime_df %>% 
  filter(Precinct != "UNKNOWN") %>% 
  select(Reported.Date, Crime.Subcategory, Precinct, Neighborhood) %>%
  head(20)

#View(sample_df)  

########################################################################

#Summary Analysis for my graph

# Total Crimes in last 10 years
max_crimes <- updated_df %>%
  select(Crime.Subcategory) %>% 
  summarise(number = length(Crime.Subcategory)) %>% 
  pull(1)

#print(max_crimes)

# Most common type of crime in the last 10 years
common_crime <- updated_df %>% 
  group_by(Crime.Subcategory) %>% 
  arrange(Crime.Subcategory) %>% 
  summarize(amount = length(Crime.Subcategory)) %>% 
  filter(amount == max(amount)) %>% 
  select(Crime.Subcategory) %>% 
  pull(1)
  
#print(common_crime)

# Avg amount of crimes reported per year
average_crimes <- updated_df %>%
  group_by(Year) %>%
  arrange(Year) %>% 
  summarize(num_crime = length(Crime.Subcategory)) %>% 
  summarize(avg_per_year = mean(num_crime)) %>% 
  select(avg_per_year) %>% 
  pull(1) %>% 
  round(digits = 2)
  
#print(average_crimes)

# Percent of crimes in the North of the total amount in the past 10 years
north_amount <- total_crime_area %>%
  filter(Total == max(Total)) %>% 
  select(Total) %>% 
  pull(1)
 
#print(north_amount)

percent_north <- round((north_amount / max_crimes) * 100, digits = 1)

percentage_north <- paste0(percent_north, "%")

#print(percentage_north) 
  

# List of the data for the summary analysis
summary_analysis_1 <- list(max_crimes, common_crime, average_crimes, percentage_north, north_amount)
  
#print(summary_analysis_list)

########################################################################






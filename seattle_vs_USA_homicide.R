library("httr")
library("dplyr")
library("jsonlite")
library("wbstats")
library("ggplot2")
library("tidyr")


crime_rate_df = read.csv(file = "Crime_Data copy.csv")

#View(crime_rate_df)


homicide_df <- crime_rate_df %>% 
  mutate(year = as.character(Occurred.Date)) %>% 
  mutate(year = substr(year, 7, 10)) %>% 
  filter(Crime.Subcategory == "HOMICIDE") %>% 
  filter(year <= 2017) %>% 
  filter(year >= 1990) %>% 
  #Had to add a number for each row in order to sum all homicides over a particular year
  mutate(for_sum = 1) %>% 
  group_by(year) %>% 
  summarise(homicide_rate_seattle = sum(for_sum) / 10)
  
 
#View(homicide_df)



homicides_usa <- wb(country = "USA", indicator = c("VC.IHR.PSRC.P5"), mrv = 30) %>% 
  mutate(year = date) %>% 
  mutate(homicide_rate_USA = value) %>% 
  select(year, homicide_rate_USA)

#View(homicides_usa)

final_df <- left_join(homicide_df, homicides_usa, by = "year") %>% 
  gather(area, rate, homicide_rate_seattle:homicide_rate_USA)

columns_s <- left_join(homicide_df, homicides_usa, by = "year") %>% 
  select(-year)

choices_df <-  left_join(homicide_df, homicides_usa, by = "year")

#View(columns_s)


sample_df <- final_df %>% 
  top_n(10, year)


#View(choices_df)


#Plot

comp_hom_rate <- ggplot(final_df)+
  
  geom_bar(mapping = aes(
    
    x = year,
    y = rate,
    color = area
    
  ),
  
  stat = "identity",
  
  position = "dodge"
  
  )
  
?theme



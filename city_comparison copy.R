#install.packages("dplyr")
library("dplyr")
#install.packages("ggplot2")
library("ggplot2")
library("tidyr")


Seattle_population <- 744955

seattle_crimes <- read.csv('Seattle_2018 copy.csv', stringsAsFactors=FALSE) %>%
  filter(startsWith(Crime.Subcategory, "AGGRAVATED ASSAULT") | startsWith(Crime.Subcategory, "ROBBERY") |
           startsWith(Crime.Subcategory, "BURGLARY") | Crime.Subcategory == "ARSON" | Crime.Subcategory == "HOMICIDE" |
           Crime.Subcategory == "MOTOR VEHICLE THEFT" | Crime.Subcategory == "RAPE") %>%
  mutate(crime_type = substr(Crime.Subcategory, 1, 4), city = "Seattle")  %>%
  select(c(crime_type, city))

  seattle_crimes$crime_type[seattle_crimes$crime_type == "AGGR"] <- "Aggravated_Assault"
  seattle_crimes$crime_type[seattle_crimes$crime_type == "ROBB"] <- "Robbery"
  seattle_crimes$crime_type[seattle_crimes$crime_type == "BURG"] <- "Burglary"
  seattle_crimes$crime_type[seattle_crimes$crime_type == "ARSO"] <- "Arson"
  seattle_crimes$crime_type[seattle_crimes$crime_type == "HOMI"] <- "Homicide"
  seattle_crimes$crime_type[seattle_crimes$crime_type == "MOTO"] <- "Motor_Vehicle_Theft"
  seattle_crimes$crime_type[seattle_crimes$crime_type == "RAPE"] <- "Sexual_Assault"
  
  
  
seattle_crimes <- group_by(seattle_crimes, crime_type) %>%
  summarize(crime_amount = n()) %>%
  mutate(city = "Seattle")



Denver_population <- 716492

denver_crimes <- read.csv('Denver_2018 copy.csv', stringsAsFactors=FALSE) %>%
  filter(OFFENSE_CATEGORY_ID == "aggravated-assault" | OFFENSE_CATEGORY_ID == "robbery" |
           OFFENSE_CATEGORY_ID == "burglary" | OFFENSE_CATEGORY_ID == "arson" | OFFENSE_CATEGORY_ID == "murder" |
           OFFENSE_CATEGORY_ID == "auto-theft" | OFFENSE_CATEGORY_ID == "sexual-assault") %>%
  mutate(city = "Denver") %>%
  select(c(OFFENSE_CATEGORY_ID, city)) %>%
  rename(crime_type = OFFENSE_CATEGORY_ID)

  denver_crimes$crime_type[denver_crimes$crime_type == "aggravated-assault"] <- "Aggravated_Assault"
  denver_crimes$crime_type[denver_crimes$crime_type == "burglary"] <- "Burglary"
  denver_crimes$crime_type[denver_crimes$crime_type == "robbery"] <- "Robbery"
  denver_crimes$crime_type[denver_crimes$crime_type == "arson"] <- "Arson"
  denver_crimes$crime_type[denver_crimes$crime_type == "murder"] <- "Homicide"
  denver_crimes$crime_type[denver_crimes$crime_type == "auto-theft"] <- "Motor_Vehicle_Theft"
  denver_crimes$crime_type[denver_crimes$crime_type == "sexual-assault"] <- "Sexual_Assault"

  denver_crimes <- group_by(denver_crimes, crime_type) %>%
    summarize(crime_amount = (n() * (Seattle_population / Denver_population))) %>%
    mutate(city = "Denver")
  
  
  
DC_population <- 702455

DC_crimes <- read.csv('DC_2018 copy.csv', stringsAsFactors = FALSE)  %>%
  filter(offense.text == "robbery" | offense.text == "burglary" | offense.text == "assault w/dangerous weapon" |
           offense.text == "arson" | offense.text == "homicide" | offense.text == "motor vehicle theft" | 
           offense.text == "sex abuse") %>%
  mutate(city = "D.C") %>%
  select(c(offense.text, city)) %>%
  rename(crime_type = offense.text)

  DC_crimes$crime_type[DC_crimes$crime_type == "assault w/dangerous weapon"] <- "Aggravated_Assault"
  DC_crimes$crime_type[DC_crimes$crime_type == "burglary"] <- "Burglary"
  DC_crimes$crime_type[DC_crimes$crime_type == "robbery"] <- "Robbery"
  DC_crimes$crime_type[DC_crimes$crime_type == "arson"] <- "Arson"
  DC_crimes$crime_type[DC_crimes$crime_type == "homicide"] <- "Homicide"
  DC_crimes$crime_type[DC_crimes$crime_type == "motor vehicle theft"] <- "Motor_Vehicle_Theft"
  DC_crimes$crime_type[DC_crimes$crime_type == "sex abuse"] <- "Sexual_Assault"

  
  DC_crimes <- group_by(DC_crimes, crime_type) %>%
    summarize(crime_amount = (n() * (Seattle_population / DC_population))) %>%
    mutate(city = "D.C.")
  

Seattle_and_Denver_crimes <- full_join(seattle_crimes, denver_crimes, by = c("crime_type", "crime_amount", "city")) 
city_comparison_df <- full_join(Seattle_and_Denver_crimes, DC_crimes, by = c("crime_type", "crime_amount", "city")) %>% 
  spread(key = crime_type, value = crime_amount)

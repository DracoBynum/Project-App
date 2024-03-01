# test

library("ggplot2") 

library("dplyr")

library("tidyr")

library("wbstats")



wb_murder <- wb(country = "countries_only", indicator = c("VC.IHR.PSRC.P5"), mrv = 26) %>% 
  mutate(date = as.numeric(date)) %>% 
  arrange(date) %>% 
  group_by(date) %>% 
  summarise(avg = mean(value)) %>% 
  rename(Year = date, Average_homicide_rate = avg) 


mean_rate <-round(mean(wb_murder$Average_homicide_rate), 2)

max_rate <- round(max(wb_murder$Average_homicide_rate), 2)

min_rate <- round(min(wb_murder$Average_homicide_rate), 2)

change_since_max <- round(max_rate - 	7.386598, 2)
  
percent_change <- round((change_since_max / max_rate) * 100, 2)
  
  
murder_data_sample <- wb_murder %>% 
  head(6)


#View(murder_data_sample)





murder_plot <- ggplot(data = wb_murder) +
  geom_line(mapping = aes(x = Year, y = Average_homicide_rate, color = Year)) +
  labs(title = "Average annual global intentional homicide rate", 
       x = "Year",
       y = "Average homocide rate (per 100,000 people)") + 
       theme(legend.position = "none") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 19)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  
  
 




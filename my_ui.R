library("shiny")
source("city_comparison copy.R")
library("ggplot2")
source("seattle_vs_USA_homicide.R")
source("wb_murder copy.R")


my_ui <- navbarPage(
  "Crime Statistics: From Seattle and Beyond",
  introduction,
  global_homicide_rates,
  dangerous_areas,
  seattle_vs_US,
  city_comparison
)


introduction <- tabPanel(
  title = "Introduction",
  titlePanel("Data Introduction"),
 
   
mainPanel(h4("By: Draco Bynum, Daniel Penkov, Ryan Crowley, and Tim Hahn"),
            
p("“Criminal behavior” has been, and always will be, an issue that affects the whole world. Our team was interested in comparing data pertaining to different criminal behavior in Seattle, the United States as a whole, as well as the rest of the world. For criminal data throughout the world, we looked at intentional homicide (murder) statistics from the World Bank API. Intentional homocides are described as “unlawful homicides purposely inflicted as a result of domestic disputes, interpersonal violence, violent conflicts over land resources, intergang violence over turf or control, and predatory violence and killing by armed groups.” When you look at the world around us today, it is easy to be bleak or depressed. However, our team feels that through our analysis, we can hopefully show that the world is becoming a safer place to live."),

p("On a more local scope, we looked at the City of Seattle open data portal, which provided us with some insight into the various crime rates in the Seattle area. The portal is far wider in scope than just crime, including information about property rights, education, socioeconomics and information about City of Seattle employees. This is a powerful information source, especially to us as residents of this city. Using this information we can help others to have a deeper understanding of the crime statistics of their university’s home."),

p("In addition to Seattle crime data, our analysis has also led us to Denver Open Source Crime Data and Washington D.C Open Source Crime Data in order to compare some of our city’s crime rates to cities with similar populations in the United States in order to get an additional meaningful perspective of our city’s public safety."),

p("- Link to World Bank Data website:", a("World Bank API", href = "https://data.worldbank.org/indicator/VC.IHR.PSRC.P5?end=2017&start=1990&view=chart")),

p("- Link to City of Seattle Data website:", a("City of Seattle", href = "https://data.seattle.gov/")),

p("- Link to Denver Crime Data website:", a("Denver Open Source Crime Data", href = "https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-crime")),

p("- Link to Washington D.C Crime Data website:", a("Washington D.C Open Source Crime Data", href = "https://dcatlas.dcgis.dc.gov/crimecards/")),

p("- Link to our data report for A7 github page:", a("Our Github Data Report", href = "https://info201a-wi20.github.io/project-report-Daniel-Penkov/"))
          
    )

) 



  
  year_range_r <- range(wb_murder$Year)
  
  year_slider_r <- sliderInput(
    inputId = "murder_slider",
    label = "Slide to see a range of the Years of Intrest",
    min = 1992,
    max = 2017,
    value = year_range_r
  )
  
  output_murder_plot <- plotOutput(outputId = "plot_murder")
  
  global_homicide_rates <- tabPanel(
    title = "Global Homicide Rates Over Time",
    titlePanel("How has the average homicide rate (per 100,000 people) changed globally in the last 26 years?"),
    
    sidebarLayout(
      sidebarPanel(
        year_slider_r, p(em("Note: The slider is most effective if the range has 15 years or more of data"))
      ),
      
      mainPanel(
        output_murder_plot
      )
      
    ),
    
    h3("Answer"),


  p("Watching the news or going on social media may give somebody the impression that the world is getting worse everyday. However, when it comes to murder, the global rate seems to have been in decline since the mid 1990’s. In fact, since 1994,
  the rate has decreased by over 26.47%. Some other interesting peices of data I gathered from my analysis were that over the last 26 years, the average global homicide rate (per 100,000 people) has been 8.21. Additionaly, the year 1994 had the 
  highest murder rate, 10.05, while 2013 had the lowest rate, 7.19."),  

  
  
  
  )

############# Work for dangerous areas in seattle ####################

crime_df <- read.csv("Crime_Data copy.csv")

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

crime_area_plot <- ggplot(data = total_crime_area) +
  geom_col(mapping = aes(x = reorder(Precinct, Total), y = Total, fill = reorder(Precinct, Total))) +
  scale_fill_brewer(palette =  "Pastel2") + 
  labs(title = "Most Dangerous Areas in Seattle by Crime Rate", x = "Cardinal Direction", y = "Amount of Crimes", fill = "Cardinal Direction") +
  theme_classic() +
  theme(legend.position = c(.20, .70), legend.background = element_rect(fill = "grey", size = 0.5, linetype = "solid", colour = "black"), axis.text.x = element_text(size = 10))

select_area <- as.character(total_crime_area$Precinct)

average <- total_crime_area %>% 
  summarise(avg = mean(Total)) %>% 
  select(avg) %>% 
  pull(1) %>% 
  round(digits = 2)

greater_than_average <- total_crime_area %>% 
  filter(Total > average)

less_than_average <- total_crime_area %>% 
  filter(Total < average)

filter_most_safe <- total_crime_area %>% 
  filter(Precinct == "SOUTHWEST")

filter_least_safe <- total_crime_area %>% 
  filter(Precinct == "NORTH")

area_select <- selectInput(
  inputId = "area_choice",
  label = paste("Select Most or Least Safe to view the areas in the table below the plot for their respective options (All shows the data frame that holds the information from the 'Original' plot)."),
  choices = list("All", "Most Safe", "Least Safe"),
  selected = NA,
  multiple = FALSE
)



average_buttons <- radioButtons(
  inputId = "average_choice",
  label = "Select Above or Below Average to see the areas in Seattle that are either Above or Below the average amount of crimes committed per area in the plot (Original shows the plot with all of the areas).",
  list("Original", "Above Average" , "Below Average"),
  selected = "Original"
)

average_text <- textOutput(outputId = "avg_area")

plot_output <- plotOutput(outputId = "plot_area")

table_output <- tableOutput(outputId = "df_danger")

########################  End of work for dangerous areas in seattle #############################


dangerous_areas <- tabPanel(
  title = "Amount of Crime per Area of Seattle",
  titlePanel("Over the past ten years, what have been the most dangerous and safe areas (cardinal directions) in Seattle based on the crime rate?"),   
  
  sidebarLayout(
    
    sidebarPanel(h4("Plot Interaction"), average_buttons, em(average_text)),
    
    mainPanel(h3("Interactive Plot of the Crimes Committed in Relation to the Area of Seattle Based on the Average"),
      plot_output),
      
  ),
  
  sidebarLayout(
    
    sidebarPanel(h4("Table Interaction"), area_select, p(em("-'Precinct' means Area or Cardinal Direction")), p(em("-'Total' means total amount of crimes committed")),
                                                          p(em("-'Most Safe' excludes 'Unknown' to make the information more useful."))),
    
    mainPanel(h3("Interactive Table of the Most and Least Dangerous Areas of Seattle"), table_output,
              
              h3("Answer and Conclusion"),
              
              p("After interacting with the plot and table regarding the amount of crimes per area in Seattle, 
                a lot of information is shown.  First off, the average amount of crimes committed in each area 
                is about 80105.  When clicking the ‘Above Average’ button, the plot shows that the West and North 
                Areas of Seattle are the most dangerous due to crime rate, with the North and West having 155845 and 137532
                crimes committed in the last 10 years respectively.  As for the ‘Below Average’ plot it shows that the least
                dangerous areas are ‘Unknown’ and Southwest with 2996 and 45597 crimes committed respectively.
                As for the South and the East we can see that they are below the average, however they are fairly close to it.
                Overall it can be concluded that the most dangerous area in Seattle based on crime rate for the past 10 years is the North.
                As far as the safest I would say that the safest area is Southwest excluding the ‘Unknown’ Area
                due to having the least amount of crimes committed in an unknown area."),
    
              p(em("Link to the specific Seattle Crime Data from the City of Seattle Data website:"), a("Seattle Crime Data", href = "https://data.seattle.gov/Public-Safety/Crime-Data/4fs7-3vj5"))
              
        )  
    )


)


year_range_s <- as.numeric(range(final_df$year))
select_values_s <- colnames(columns_s)


year_input_sea_vs_USA <- sliderInput(
  inputId = "seattle_vs_US_slider",
  label = "Year",
  min = 2000,
  max = 2018,
  value = year_range_s
)

data_input_sea_vs_USA <- selectInput(
  inputId = "data_input_sea_vs_USA",
  label = "Area Of Interest",
  choices = select_values_s,
  selected = "homicide_rate_seattle"
)

seattle_vs_US <- tabPanel(
  title = "Seattle vs U.S. Homicide",
  titlePanel("Does Seattle have an increased homicide rate compared to the national average?"),

  year_input_sea_vs_USA,
  
  data_input_sea_vs_USA,
  
  plotOutput("plot_s"),
  
  p("My question explores the difference in homicide rate between the Seattle Area and the United 
States as a whole. Homicide rate refers to the number of intentional killings over a year, per 100,
000 people.  This is a statistic is indicative of the effectiveness of policing, local legislation,
and the overall safety of an area."),  
    
p("For my purposes, I used homicide rate data from the",
a("World Bank API", href = "https://data.worldbank.org/indicator/VC.IHR.PSRC.P5"), "and raw Crime 
Data from the", a("Seattle City Portal", href = "https://data.seattle.gov/Public-Safety/Crime-Data/4fs7-3vj5"), 
"and filtered it down to only homicide.  Typical homicide data reports have the crimes listed as a 
rate per 100,000 people, so to match this convention I divided the yearly totals by 100,000.  
The Data was then organized into the following bar chart to display the yearly rates side by side."),  



p("My data clearly shows that Seattle has a far lower homicide rate than the United States as a whole,
despite having an increased overall rate over the past 20 years.  In fact, Seattle has a rate that 
is roughly half the national average, and sometimes even less!  What is interesting to consider as 
well, is that while the national rate has declined by about 50% over the past ~30 years, the Seattle
rate has climbed greatly over the same time. It's hard to say what caused this great increase,
but experts writing for", a("Seattle Business Magazine", href = "https://www.seattlebusinessmag.com/tourismhospitality/downtown-seattles-rising-crime-rate-stings-citys-hospitality-sector"), 
"think that rising homelessness and street violence could be possible causes.")

)


############################## Work for City Comparison ##############################


#spread_city_df <- city_comparison_df %>% spread(key = crime_type, value = crime_amount)
#crime_choices <- colnames(spread_city_df)
#print(crime_choices)
crime_choices <- tail(colnames(city_comparison_df), 7)

crime_input <- selectInput(
  inputId = "crime_choice",
  label = "Crime of Interest:",
  choices = crime_choices,
  selected = "Aggravated_Assault"
)


city_selection <- checkboxGroupInput(
  inputId = "display_city", 
  label = "Cities to Display:", 
  choices = city_comparison_df$city, 
  selected = city_comparison_df$city,
  )


city_comparison_plot_output <- plotOutput(outputId = "city_compare_plot")


city_comparison <- tabPanel(
  title = "2018 City Comparison",
  titlePanel("How does Seattle compare proportionally to the two American cities with the closest population sizes in 2018 crime rates?"),
  crime_input,
  city_selection,
  city_comparison_plot_output,
  p("This analysis question focused on the comparison of various crime rates from the cities of Seattle, Washington D.C., 
    and Denver. 7 individual crime rates were compared across all three cities in the analysis conducted in answering this question, and the resulting comparisons 
    can be visualized by interacting with the plot above. In order to have the ability to compare the crime rates of the 3 cities proportionally, I multiplied each crime rate for each city
    by the population ratio between that city and Seattle."),
  
  p("In order to compare crime rates of the three cities, I used 3 raw crime data sets from 2018. These data sets came from ", a("City of Seattle", href = "https://data.seattle.gov/Public-Safety/Crime-Data/4fs7-3vj5"),", ",
  a("Denver Open Source Crime Data", href = "https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-crime"), " and ", a("Washington D.C Open Source Crime Data", href = "https://dcatlas.dcgis.dc.gov/crimecards/"), "."),
  
  p("Through my data analysis, it cannot be clearly concluded whether the city of Seattle had a holistic lower or higher rate of crime in 2018 compared to Washington D.C. and Denver. However it 
    is clear that Seattle had a higher rate of some destructive crimes, while having less rampant crime in other crime categories. For instance, for some destructive crimes such as Aggrevated Assault, 
    Arson, and Burglary, Seattle had the highest crime rate among the 3 cities, with our Aggrevated Assault and Burglary rates significantly higher than D.C. and Denver. On the other end of the spectrum,
    Seattle had the lowest amount of Homicide and Sexual Assault cases recorded among the 3 cities for the same year. To even things out, Seattle came 2nd in Motor Vehicle Theft and Robbery cases for 2018.
    This data distribution for Seattle crime compared to Washington D.C. and Denver shows that with a general holistic approach to crime comparison, data analysis can help conclude
    that there are crime areas where Seattle is worse off than would be expected, but there are also places where Seattle very strongly promotes safety and social order in the law enforcement
    sector. To conclude the findings generated from this analysis, in 2018, Seattle had a significantly high amount of Aggrevated Assault, Arson, and Burglary cases in proportion to the comparative cities,
    but also had a low amount of Homicide and Sexual Assault Cases.")
  
)

############################## End of Work for City Comparison ##############################


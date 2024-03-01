my_server <- function(input, output) {
  
  output$city_compare_plot <- renderPlot({
    View(city_comparison_df)
    city_comparison_df <- filter(city_comparison_df, city %in% input$display_city)
    city_comparison_plot <- ggplot(data = city_comparison_df) +
      geom_col(mapping = aes_string(x = "city", y = input$crime_choice, fill = "city"), position = "dodge") +
      labs(title = "2018 Crime Amount Comparision") +
      labs(fill = "City") +
      scale_fill_brewer(type = "qal", palette = "Set2") 
    
    return(city_comparison_plot)
  })
  


  output$plot_area <- renderPlot({
    
  
  if (input$average_choice == "Original"){
    area_plot <- ggplot(data = total_crime_area) +
      geom_col(mapping = aes(x = reorder(Precinct, Total), y = Total, fill = reorder(Precinct, Total))) +
      scale_fill_brewer(palette =  "Pastel2") + 
      labs(title = "Most Dangerous Areas in Seattle by Crime Rate", x = "Cardinal Direction", y = "Amount of Crimes", fill = "Cardinal Direction") +
      theme_classic() +
      theme(legend.position = c(.20, .70), legend.background = element_rect(fill = "grey", size = 0.5, linetype = "solid", colour = "black"), axis.text.x = element_text(size = 10))
  
  }
  
  if (input$average_choice == "Above Average") {
    area_plot <- ggplot(data = greater_than_average) +
      geom_col(mapping = aes(x = reorder(Precinct, Total), y = Total, fill = reorder(Precinct, Total))) +
      scale_fill_brewer(palette = "Reds") +
      labs(title = "Areas Above the Average Crime Rate", x = "Cardinal Direction", y = "Amount of Crimes", fill = "Cardinal Direction") +
      theme_classic() +
      theme(legend.position = c(.20, .70), legend.background = element_rect(fill = "grey", size = 0.5, linetype = "solid", colour = "black"), axis.text.x = element_text(size = 10))
  
  }

  if (input$average_choice == "Below Average") {
    area_plot <- ggplot(data = less_than_average) + 
      geom_col(mapping = aes(x = reorder(Precinct, Total), y = Total, fill = reorder(Precinct, Total))) +
      scale_fill_brewer(palette =  "Greens", direction = -1) + 
      labs(title = "Areas Below the Average Crime Rate", x = "Cardinal Direction", y = "Amount of Crimes", fill = "Cardinal Direction") +
      theme_classic() +
      theme(legend.position = c(.15, .70), legend.background = element_rect(fill = "grey", size = 0.5, linetype = "solid", colour = "black"), axis.text.x = element_text(size = 10))

  }
    
    return(area_plot)
    
  })
  
  
  
  output$avg_area <- renderText({
    if (input$average_choice == "Below Average") {
      the_average <- paste("Average Crimes per Area:", average)
    }
    
    if (input$average_choice == "Above Average") {
      the_average <- paste("Average Crimes per Area:", average)
    }
    
    if (input$average_choice == "Original") {
      the_average <- ""
    }
    
    return(the_average)
  })
     
  
  output$df_danger <- renderTable({
    
    if (input$area_choice == "Most Safe"){
      saftey_table <- filter_most_safe
    }
    
    if (input$area_choice == "Least Safe"){
      saftey_table <- filter_least_safe
    }
    
    if (input$area_choice == "All"){
      saftey_table <- total_crime_area
    }
    
    return(saftey_table)
    
  })
  
  
   
  
  output$plot_s <- renderPlot({
    
    plot_data_s <- choices_df %>% 
      filter(year > input$seattle_vs_US_slider[1], year < input$seattle_vs_US_slider[2])
    
    p <- ggplot(
      
      data = plot_data_s,
      mapping = aes_string(x = "year", y = input$data_input_sea_vs_USA, fill = "year"),
 
      
      )+
        
        geom_bar(
          
          stat = "identity"
          
      )+
        
        scale_color_brewer(
          
          palette = "Dark2"
          
     )+
      
          labs(
        
        title = "Crime rates in Seattle vs the United States",
        x = "Year"
        
      )+
      
          theme(
            
            legend.position = "none"
            
          )
      
    

    p
  })
  
  output$plot_murder <- renderPlot({

    range_year <- input$murder_slider  
    
    filter_murder <- wb_murder %>% 
      filter(Year > range_year[1], Year < range_year[2])

    
    murder_plot_r <- ggplot(data = filter_murder) +
      geom_line(mapping = aes_string(x = "Year", y = "Average_homicide_rate", color = "Year")) +
      labs(title = "Average annual global intentional homicide rate", 
           x = "Year",
           y = "Average homocide rate (per 100,000 people)") + 
      theme(legend.position = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 19)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 12))
    
    return(murder_plot_r)
  })
  

}






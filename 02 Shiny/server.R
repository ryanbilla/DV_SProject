# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {
  
  KPI_Low_Max_value <- reactive({input$KPI1/1000})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.50 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  
  df1 <- eventReactive(input$clicks1, {KPI_df<- death_df %>% group_by(AGE_GROUP, SEX) %>% summarize(sum_death = sum(NUMBER_OF_DEATHS)/1000000, sum_100 = sum(DEATH_RATE_PER_100_000)/1000000) %>% mutate(ratio = sum_death / sum_100) %>% mutate(kpi = ifelse(ratio <= KPI_Low_Max_value, '03 Low', ifelse(ratio <= KPI_Medium_Max_value, '02 Medium', '01 High'))) %>% rename(age_group=AGE_GROUP, sex=SEX, SUM_DEATH=sum_death, SUM_100=sum_100, RATIO=ratio, KPI=kpi)%>%tbl_df
  })
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Mortality of the World Population\n Number of Deaths(millions), Sum of Death Rate per 100,000 (millions),\n and Ratio Between the Number of Deaths and Death Rate') +
      labs(x=paste("Age Group"), y=paste("Sex")) +
      layer(data=KPI_df, 
            mapping=aes(x=as.character(age_group), y=sex, label=round(SUM_DEATH,2)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=KPI_df, 
            mapping=aes(x=as.character(age_group), y=sex, label=round(SUM_100,2)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", vjust=2), 
            position=position_identity()
      ) +
      layer(data=KPI_df, 
            mapping=aes(x=as.character(age_group), y=sex, label=round(RATIO, 2)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", vjust=4), 
            position=position_identity()
      ) +
      layer(data=KPI_df, 
            mapping=aes(x=as.character(age_group), y=sex, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
            
      )
    plot
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for Second Tab:
  
  df2 <- eventReactive(input$clicks2, {df})
  
  output$distPlot2 <- renderPlot(height=1000, width=2000, {
    plot1 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~SEX, ncol=1) +
      labs(title='Country vs Death Rate per 100000 ') +
      labs(x=paste(""), y=paste("Avg. Death Rate per 100000")) +
      layer(data=df, 
            mapping=aes(x=COUNTRY_NAME, y=AVG_DR), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="sky blue", fill="dark green"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df, 
            mapping=aes(x=COUNTRY_NAME, y=AVG_DR, label=round(AVG_DR)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      ) +
      layer(data=df, 
            mapping=aes(x=COUNTRY_NAME, y=AVG_DR, label=round(WINDOW_AVG_DR)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-2), 
            position=position_identity()
      ) +
      layer(data=df, 
            mapping=aes(x=COUNTRY_NAME, y=AVG_DR, label=round(AVG_DR - WINDOW_AVG_DR)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-5), 
            position=position_identity()
      ) +
      layer(data=df, 
            mapping=aes(yintercept = WINDOW_AVG_DR), 
            geom="hline",
            geom_params=list(colour="red")
      ) 
    
    plot1
  })
  
  # Begin code for Third Tab:
  
df3 <- eventReactive(input$clicks3, {death_df  })
  
  output$distPlot3 <- renderPlot(height=1000, width=2000, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='How Disease Affects Number of Deaths Worldwide \n When Compared to Death Per 100,000 Statistics from 1970 to 2010') +
      labs(x="Number of Deaths", y=paste("Death Rate Per 100,000")) +
      layer(data=death_df, 
            mapping=aes(x=as.numeric(as.character(NUMBER_OF_DEATHS)), y=as.numeric(as.character(DEATH_RATE_PER_100_000)), color=YEAR), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            position=position_jitter(width=0.3, height=0)
      )
    
    plot3
  })
})

# server.R
require(jsonlite)
require(RCurl)
require(ggplot2)
require(dplyr)
require(shiny)

shinyServer(function(input, output) {
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.5 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  
  df1 <- eventReactive(input$clicks, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                               "select AGE_GROUP, SEX, sum_death, sum_100, kpi as ratio, 
                                                                               case
                                                                               when kpi < "p1" then \\\'03 Low\\\'
                                                                               when kpi < "p2" then \\\'02 Medium\\\'
                                                                               else \\\'01 High\\\'
                                                                               end kpi
                                                                               from (select AGE_GROUP, SEX, 
                                                                               sum(NUMBER_OF_DEATHS)/1000000 as sum_death, sum(DEATH_RATE_PER_100_000)/1000000 as sum_100, 
                                                                               (sum(NUMBER_OF_DEATHS) / sum(DEATH_RATE_PER_100_000))*10 as kpi
                                                                               from DISEASE 
                                                                               group by SEX, AGE_GROUP)
                                                                               order by AGE_GROUP;"
                                                                               ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_nar784', PASS='orcl_nar784', 
                                                                                                 MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value(), p2=KPI_Medium_Max_value()), verbose = TRUE)))
  })
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Mortality of the World Population\n Number of Deaths(millions), Sum of Death Rate per 100,000 (millions),\n and Ratio Between the Number of Deaths and Death Rate') +
      labs(x=paste("Age Group"), y=paste("Sex")) +
      layer(data=df1(), 
            mapping=aes(x=as.character(AGE_GROUP), y=SEX, label=round(SUM_DEATH,2)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=as.character(AGE_GROUP), y=SEX, label=round(SUM_100,2)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", vjust=2), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=as.character(AGE_GROUP), y=SEX, label=round(RATIO, 2)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", vjust=4), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=as.character(AGE_GROUP), y=SEX, fill=KPI), 
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
  
  # Begin code for Second Tab (Bar Chart):
  
  df2 <- eventReactive(input$clicks2, {bar_df})
  
  output$distPlot2 <- renderPlot(height=1000, width=2000, {
    plot1 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~SEX, ncol=1) +
      labs(title='Country vs Death Rate per 100000 ') +
      labs(x=paste(""), y=paste("Avg. Death Rate per 100000")) +
      layer(data=df2(), 
            mapping=aes(x=COUNTRY_NAME, y=AVG_DR), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="sky blue", fill="dark green"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df2(), 
            mapping=aes(x=COUNTRY_NAME, y=AVG_DR, label=round(AVG_DR)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      ) +
      layer(data=df2(), 
            mapping=aes(x=COUNTRY_NAME, y=AVG_DR, label=round(WINDOW_AVG_DR)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-2), 
            position=position_identity()
      ) +
      layer(data=df2(), 
            mapping=aes(x=COUNTRY_NAME, y=AVG_DR, label=round(AVG_DR - WINDOW_AVG_DR)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-5), 
            position=position_identity()
      ) +
      layer(data=df2(), 
            mapping=aes(yintercept = WINDOW_AVG_DR), 
            geom="hline",
            geom_params=list(colour="red")
      ) 
    
    plot1
  })
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for Third Tab (Scatter Plot):
  
df3 <- eventReactive(input$clicks3, {death_df  })
  
  output$distPlot3 <- renderPlot(height=1000, width=2000, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='How Disease Affects Number of Deaths Worldwide \n When Compared to Death Per 100,000 Statistics from 1970 to 2010') +
      labs(x="Number of Deaths", y=paste("Death Rate Per 100,000")) +
      layer(data=df3(), 
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

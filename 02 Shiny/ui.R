#ui.R 
# Here we call the visualizations from server.R (distPlot 1,2,3) and allow the user to interact with the plots with clicks (1,2,3)
# To show that we a low, medium, and high KPI in our crosstab, we set the low max fairly high as well as the medium max.
# The link to our web app is https://nathanriojas.shinyapps.io/WorldDiseasePopulationData


library(shiny)

navbarPage(
  title = "Elements of Visualization",
  tabPanel(title = "Crosstab",
           sidebarPanel(
             actionButton(inputId = "light", label = "Light"),
             actionButton(inputId = "dark", label = "Dark"),
             sliderInput("KPI1", "KPI_Low_Max_value:", 
                         min = 1, max = 15,  value = 9),
             sliderInput("KPI2", "KPI_Medium_Max_value:", 
                         min = 15, max = 50,  value = 35),
             textInput(inputId = "title", 
                       label = "Crosstab Title",
                       value = "Disease and World Population Cross Tab: Sum_Death,Sum_100, Sum_Death/Sum_100"),
             actionButton(inputId = "clicks1",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot1")
           )
  ),
  tabPanel(title = "Barchart",
           sidebarPanel(
             actionButton(inputId = "clicks2",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot2")
           )
  ),
  tabPanel(title = "Scatter Plot",
           sidebarPanel(
             actionButton(inputId = "clicks3",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot3")
           )        
  )
)

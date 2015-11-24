#ui.R 
# Here we call the visualizations from server.R (distPlot 1,2,3) and allow the user to interact with the plots with clicks (1,2,3)
# To show that we a low, medium, and high KPI in our crosstab, we set the low max fairly high as well as the medium max.
# The link to our web app is https://nathanriojas.shinyapps.io/WorldDiseasePopulationData


library(shiny)
shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),

  
  
  
navbarPage(
  
  title = "Elements of Visualization",
  tabPanel(title = "Data Overview",
           headerPanel("The Effect of Diseases on World Population"),
           
           mainPanel("Our data looks at the way disease affects the population of different countries and the world overall.The two main variables we review are the number of deaths due to disease per country and the death rate per 100,000. The latter is a standard way of normalizing the number of deaths to accurately compare and scale the effect disease has on each country despite size differences between country population. This data is an accumulation of data from 1975-2010, so the numbers are not for a single year."
           ),
           mainPanel("____________________________________________________________"
                     
           ),
           mainPanel("Here is an explanation of the three visualizations in the next three tabs"
           
  ),
  mainPanel("____________________________________________________________"
            
  ),
  mainPanel("1. Crosstab: An interactive visualization that represents how different age groups are killed by diseases each year. We further break this down by sex. The user can adjust the KPI values. "
  )
,
mainPanel("2. Barchart: A very large barchart (sorry for the size) that compares the death rate per 100,000 for different countries. Also shows window averages."
)
,
mainPanel("3. Scatter Plot: This compares the number of deaths and death rate per 100,000 that the world experiences for different sexes. This shows how normalized values show that the actual number of deaths can hide the true nature of how disease affects the population.")
),
  tabPanel(title = "Crosstab",
           
           sidebarPanel(
             actionButton(inputId = "light", label = "Light"),
             actionButton(inputId = "dark", label = "Dark"),
             sliderInput("KPI1", "KPI_Low_Max_value:", 
                         min = 1, max = 15,  value = 9),
             sliderInput("KPI2", "KPI_Medium_Max_value:", 
                         min = 15, max = 80,  value = 35),
             textInput(inputId = "title", 
                       label = "Crosstab Title",
                       value = "Disease and World Population Cross Tab (Millions): Sum_Death,Sum_100, Sum_Death/Sum_100"),
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
))

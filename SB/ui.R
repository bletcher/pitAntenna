library(shiny)
library(dplyr)
library(plotly)

startTime <- Sys.time()

load(file='/home/ben/ShinyApps/pitAntenna/SB/counts.RData') # counts
print(c("after load counts", Sys.time() - startTime))

uniqueTags <- counts$tag

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Stanley Brook"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel( width = 2,
        selectInput('tag', 'Fish ID:', uniqueTags, selectize=FALSE, selected = '1c2d6c19ad', width = '80%'),
        hr(),

        checkboxInput(inputId = "tide",
                      label = strong("Show tide height"),
                      value = FALSE),
        checkboxInput(inputId = "temperature",
                      label = strong("Show daily mean water temp"),
                      value = FALSE),
        checkboxInput(inputId = "flow",
                      label = strong("Show daily mean stream flow"),
                      value = FALSE),
        hr(),
        checkboxInput(inputId = "sampDate",
                      label = strong("Show e-fishing sample dates"),
                      value = TRUE),
        checkboxInput(inputId = "portableSampleDays",
                      label = strong("Show portable ant sample dates"),
                      value = FALSE),
        hr(),
        helpText("Select an antenna to show # of hits/individiual/day, range = (log()*20)"),
        selectInput('ant', 'Antenna (river m):', c("none",-20, 5, 10, 20, 60, 220, 340, 420,430, 1260), 
                    selectize=FALSE, multiple = TRUE, selected = 'none', width = '50%'),
        hr(),
        helpText(p("Data from Stanley Brook, ME. 
                 Tags are ordered from most observations to fewest. 
                 Fish with single observations are left out."),
                 p("Orange horizontal lines are acoustic tag readers. Blue horizontal lines are PIT tag readers."),
                 p("Electrofishing samples (2/year) start dates are vertical green lines and end dates are vertical red lines.")),
        hr(),
        
        helpText("To zoom in, click and drag a rectangle. 
                 Double click in the rectangle to zoom. 
                 Double click on the zoomed graph to restore original ranges.")

      ),
      
      # Create a spot for the barplot
      mainPanel(

         # plotOutput("IDPlot", height = 500,
         #              dblclick = "IDPlot_dblclick",
         #              brush = brushOpts(
         #                id = "IDPlot_brush",
         #                resetOnNew = TRUE
         #              )
         #           )
         
         # code from https://gitlab.com/snippets/16220
         div(
           style = "position:relative",
           plotOutput("IDPlot", height = 500,
                      dblclick = "IDPlot_dblclick",
                      brush = brushOpts(
                        id = "IDPlot_brush",
                        resetOnNew = TRUE
                      ),
                      hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")
           ),
           uiOutput("hover_info")
         )
      )
      
    )
  )
)
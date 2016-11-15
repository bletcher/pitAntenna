library(shiny)
library(dplyr)
library(plotly)

startTime <- Sys.time()

load(file='/home/ben/ShinyApps/pitAntenna/WB/counts.RData') # counts
print(c("after load counts", Sys.time() - startTime))

uniqueTags <- counts$tag

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("West Brook"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel( width = 2,
        selectInput('tag', 'Fish ID:', uniqueTags, selectize=TRUE, selected = '00088cc473', width = '80%'),
        hr(),
        
        checkboxInput(inputId = "facet",
                      label = strong("Facet rivers for graph?"),
                      value = FALSE),
        hr(),

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
        selectInput('ant', 'Antenna (river m):', c("none", 4218.25, 4221.25, 4259.55, 4263.95, 4755.25, 4791.55, 4794.55, 4797.25, 4808.3, 
                                                          4830.25, 5005.25, 5020, 5024.1, 5049.3, 5081.3, 5094.25, 5096.3, 5116.8,5524.25), 
                    selectize=FALSE, multiple = TRUE, selected = 'none', width = '50%'),
        hr(),
        helpText(p("Data from West Brook, MA. 
                 Tags are ordered from most observations to fewest. 
                 Fish with single observations are left out."),
                 p("Electrofishing samples (4/year) start dates are vertical light grey lines and end dates are vertical dark grey lines.")),
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
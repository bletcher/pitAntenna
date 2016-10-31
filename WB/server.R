library(shiny)
library(ggplot2)
library(plotly)
#tmp3

startTime <- Sys.time()

load(file='/home/ben/ShinyApps/pitAntenna/WB/cdForShiny.RData') # cdWB
print(c("after load cdForShiny", Sys.time() - startTime))

load(file='/home/ben/ShinyApps/pitAntenna/WB/envDataWBForMM.RData') #envDataWB
print(c("after load env",Sys.time() - startTime)) 

load(file='/home/ben/ShinyApps/pitAntenna/WB/sampDateRange.RData') #envDataWB
print(c("after load sampDateRange",Sys.time() - startTime)) 

load(file='/home/ben/ShinyApps/pitAntenna/WB/countsByDay.RData') #countsByDay, portableSampleDays
print(c("after load countsByDay",Sys.time() - startTime))

palette1 <- (c("#EA602E", "#4DAF4A", "#377EB8","#E41A1C",  "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

stationaryRM <- cdWB %>% filter(survey == "stationaryAntenna") %>% distinct(riverMeter,river) %>% filter(!is.na(riverMeter) & riverMeter > 2000)

shinyServer(function(input, output, session) {
  
  # filter for the selected ID
  selectedData <- reactive({
    cdWB %>% filter(tag == input$tag)
  })
  
  observeEvent(input$tag, {
    ranges$x <- NULL
    ranges$y <- NULL
  })
  
   xMin <- reactive({
     min(selectedData()$detectionDate)
   })
   xMax <- reactive({
     max(selectedData()$detectionDate)
   })

   selectedDataEnv <- reactive({
     envDataWB %>% filter( date >= xMin() & date <= xMax() )
   }) 
   
   selectedDataAnt <- reactive({
     countsByDay %>% filter( date >= xMin() & date <= xMax() & riverMeter %in% input$ant )
   })
   
  ranges <- reactiveValues( x = NULL, y = NULL )

  output$IDPlot <- renderPlot({
    
print(c("in RenderPlot",xMin(),xMax()))
    
    p <- selectedData() %>% arrange(detectionDate) %>%
      ggplot(aes(detectionDate,riverMeter)) +
      geom_point(aes(color=river, size=sizeForGraph)) +
      scale_colour_manual( values = c("acoustic" = palette1[1],"portableAntenna" = palette1[4],"shock" = palette1[2],"stationaryAntenna" = palette1[3],
                                      "west brook" = palette1[1],"wb obear" = palette1[2],"wb mitchell" = palette1[3],"wb jimmy" = palette1[4])) +
      scale_size( "Body size (mm)", guide = guide_legend(override.aes = list(colour = palette1[2]))) + #range = c(1, 8),
      geom_line(color="darkgrey") +
      
      geom_hline(aes(yintercept = riverMeter, color=river),  size=0.5, data=stationaryRM) +
      
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) + #xlim(ranges$x) +
      scale_x_datetime("Detection date",date_labels = "%b %d %Y") +
      scale_y_continuous("River meter from bottom of study area") +
      
      theme_bw() +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

    #p <- p+ geom_point(data=countsByDay, aes(date,riverMeter,color = log(n)), size=1) 
    
    if(input$sampDate) {
      print(c("in sampDate",xMin(),xMax(),ranges$x,ranges$y))
      p <- p + 
        geom_vline(data= sampDateRange,aes(xintercept = as.numeric(minDate)), color="lightgrey", size=0.5) +
        geom_vline(data= sampDateRange,aes(xintercept = as.numeric(maxDate)), color="darkgrey", size=0.5)
    }  
     
    if(input$portableSampleDays) {
      print(c("in portableSampleDays",xMin(),xMax(),ranges$x,ranges$y))
      p <- p + 
        geom_vline(data = portableSampleDays, aes(xintercept = as.numeric(as.POSIXct(date, origin = "1970-01-01"))), color="lightgrey", size=0.5) 
      }  

    if(input$temperature) {
      print(c("in temp",xMin(),xMax(),ranges$x,ranges$y))
      p <- p + geom_line( aes(date, daily_mean_temp * 10 - 220), color="grey", data=selectedDataEnv() ) 
    }
    
    if(input$flow) {
      print(c("in flow",xMin(),xMax(),ranges$x,ranges$y))
      p <- p + geom_line( aes(date,qPredicted * 3 - 260), color="lightblue", data=selectedDataEnv() ) 
    }
    
    if(input$ant != "none") {
      print(c("in ant",xMin(),xMax(),ranges$x,ranges$y,input$ant))
      p <- p + geom_ribbon( aes(date,y=riverMeter, ymin=riverMeter - log(n)*10, ymax=riverMeter + log(n)*10, 
                                fill=factor(riverMeter)
                                ), 
                               # fill = "grey40", 
                                alpha = 0.25,
                                data=selectedDataAnt() ) 
    }
    
    p 
    
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$IDPlot_dblclick, {
    brush <- input$IDPlot_brush
    print(c("in brush0",ranges$x,brush))
    
    if (!is.null(brush)) {
      ranges$x <- c(as.POSIXct(brush$xmin, origin = "1970-01-01"), as.POSIXct(brush$xmax, origin = "1970-01-01"))
      ranges$y <- c(brush$ymin, brush$ymax)

      print(c("in brush1",ranges$x,ranges$y))
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
      print(c("in brush2",ranges$x))
    }
    print(c("in brush3",ranges$x))
  })
  
  # tooltip
  # code from https://gitlab.com/snippets/16220
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(selectedData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      if(point$survey != "shock"){
        p(HTML(paste0(
          "<b> Date: </b>", point$detectionDate, "<br/>",
          "<b> River meter: </b>", point$riverMeter, "<br/>",
          "<b> Survey: </b>", point$survey, "<br/>"
          )))
      }
      else{
        p(HTML(paste0(
          "<b> Date: </b>", point$detectionDate, "<br/>",
          "<b> River meter: </b>", point$riverMeter, "<br/>",
          "<b> Survey: </b>", point$survey, "<br/>",
          "<b> Length: </b>", point$observedLength, " mm<br/>"
        )))  
      }
    )
  })
  
  
})
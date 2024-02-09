#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Old Faithful Geyser Data, with dynamic UI"),
  
  #First Div Section
  div(
    #Create drop down with two choices with Slider or Exact numeric input
    selectInput(
      "mainChoice",
      label = "Select Input type",
      multiple = FALSE,
      choices = c("Slider", "Exact")
    )
  ),
  
  #For debugging purpposes, show immutable vars
  div(uiOutput(("show_inputs"))),
  #For debugging purposes, show mutable vars
  div(uiOutput(("show_mem"))),
  
  div(sidebarPanel(uiOutput("sliderInput1"))),
  
  #Show a plot of the data
  uiOutput("plot1")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Define an object able to store data when app is running.
  working_mem = reactiveValues()
  working_mem$bins = 30
  working_mem$exactNum = 0
  
  #Create debug table for reactive object
  AllMem <- reactive({
    x <- reactiveValuesToList(working_mem)
    data.frame(names = names(x),
               values = unlist(x, use.names = FALSE))
  })
  
  output$show_mem <- renderTable({
    AllMem()
  })
  
  #Create debug table for input object
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
    data.frame(names = names(x),
               values = unlist(x, use.names = FALSE))
  })
  
  output$show_inputs <- renderTable({
    AllInputs()
  })
  
  #When any of the input objects store values copy them to reactive object
  #This allows for the slider options to be retained even when switching types.
  observeEvent(input$mainChoice, {
    working_mem$mainChoice = input$mainChoice
  })
  
  
  observeEvent(input$bins, {
    tryCatch({
      working_mem$bins = input$bins
    }, error = function(e) {
      NULL
    })
  })
  
  observeEvent(input$exactNum, {
    tryCatch({
      working_mem$exactNum = input$exactNum
    }, error = function(e) {
      NULL
    })
  })
  
  
  
  #Depending on choice of drop down, either generate a Slider input widget
  # or numeric input.
  output$sliderInput1 <- renderUI({
    if (input$mainChoice == "Slider") {
      sliderInput(
        "bins",
        "Number of bins:",
        min = 0,
        max = 50,
        value = working_mem$bins
      )
    } else if (input$mainChoice == "Exact") {
      numericInput("exactNum",
                   label = "Number of bins:",
                   value = working_mem$exactNum)
    }
  })
  
  
  #Create the visualization dynamically using the choice of bin input
  #Further plot should not be generated when 0 bins are selected.
  output$plot1 <- renderUI({
    print (timestamp())
    
    #check to see what variables exist
    
    x <- reactiveValuesToList (input)
    
    #If the variables for exact have not been created yet, use plot with
    #only the options for slider
    if (input$mainChoice == "Slider" & ("bins" %in% names(x))) {
      if (input$mainChoice == "Slider" & input$bins > 0) {
        print ("Slider2")
        plotOutput("distPlot")
      }
    } else if (input$mainChoice == "Exact" &
               ("exactNum" %in% names(x))) {
      print ("Exact")
      if (input$mainChoice == "Exact" & input$exactNum > 0) {
        plotOutput("distPlot")
      }
    }
  })
  
  
  
  output$distPlot <- renderPlot({
    
    
    mem <- names(reactiveValuesToList(input))
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    
    
    if (input$mainChoice == "Slider") {
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
    }
    
    
    if (input$mainChoice == "Exact" & ("exactNum" %in% mem)) {
      bins <- seq(min(x), max(x), length.out = input$exactNum + 1)
    } else if (input$mainChoice ==
               "Exact" &
               !("exactNum" %in% mem)) {
      bins <- seq(min(x), max(x), length.out = 10)
    }
    
    #bins «- seq(min(x), max(x), length.out = 30)
    
    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      col = 'darkgray',
      border = 'white',
      xlab = 'Waiting time to next eruption (in mins)',
      main = 'Histogram of waiting times'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

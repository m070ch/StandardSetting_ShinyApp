# This is a basic Shiny app that you can use to help facilitate a bookmark
# standard setting.
#
# In this example, the first tab, 'Booklet', allows you to mark down the
# judge's bookmark placement for each booklet, and provides a visual aid 
# which displays the item difficulties and judge's bookmarks along a line.

# load in required libraries
library(shiny) # library which handles the shiny app syntax
library(rhandsontable) # library which handles the R implementation of 
                       # handsontable
library(ggplot2) # library which handles the ggplot visualization syntax

# source files with helper functions
source("vis.R") # file with functions making visualizations
source("func.R") # file with functions doing data operations

# load in data
difficulties <- read.csv("difficulties.csv") # csv with all booklet's item
                                             # difficulties
scores <- read.csv("scores.csv") # csv with impact data- test-taker performance

# set number of judges
judges <- 6

#####User Interface#####

ui <- fluidPage( # Define the user interface, formatted as 'fluid' page
                 # (reactive to display size)
  tabsetPanel( # Set ui to have multiple tabs
    # Booklet tab
    tabPanel("Booklet",
      sidebarLayout( # set layout for booklet tab (sidebar/main panel)
        sidebarPanel( # create the sidebar panel
          # add a text box named 'bookletNumber', with the label 'Booklet:',
          # and an initial value of 1
          textInput(inputId = "bookletNumber", label = "Booklet:", value = 1),
          # add a save button to save results
          actionButton(inputId = "save", label = "Save"),
          #create a div element, so we can add css properties
          div(
            rHandsontableOutput("judgements"), # editable/reactive table of
                                               # judge's bookmarks
            style = "font-size:150%") # set css property- fontsize 150% of norm
          ), # end sidebar
        mainPanel(plotOutput("booklet")) # main panel and visual aid for
                                         # booklet
      ) # end sidebar layout for booklet tab
    ), # end tab 'Booklet'
    tabPanel("Impact", 
      sidebarLayout( # set layout for impant tab (sidebar/main panel)
        sidebarPanel( # create the sidebar panel
          #create a div element, so we can add css properties
          div(
            # add table of the standard setting judgement results
            tableOutput("results"),
          style = "font-size:150%") # set css property- fontsize 150% of norm
        ), 
        # create main panel with impact plot
        mainPanel(plotOutput("impact"))
      ) # end sidebar layout for impact tab
    ) # end tab 'Impact'
    
  ) # end tabset
) # end ui

####Server Logic/Reactivity#####

# Define server logic
server <- function(input, output) {
  
  # define an object of reactive values
  values <- reactiveValues()
  # define the object 'judgements', an element of the reactive values object
  # the value of 'judgements' is set as the dataframe returned by the helper 
  # function 'placeBookmark'
  values[["judgements"]] <- placeBookmark(bookletNum = 1, 
                                          difficulties = difficulties,
                                          judgements = data.frame(
                                            Judge = c(1:judges),Bookmark = 0))
  # define the object 'finalJudgements'
  values[["finalJudgements"]] <- data.frame()
  
  # set the shiny component 'judgements' to render an rHandsontable
  # which will be filled with the 'judgements' reactive value object
  # row names are hidden from display
  output$judgements <- renderRHandsontable(
    rhandsontable(values[["judgements"]], rowHeaders = NULL)
  )

  # instruct shiny to observe the shiny component 'judgements'
  # will not run until it is filled with a table to prevent error
  # when the component is interacted with (i.e. editing a cell's value)
  # the table is passed back into the 'placeBookmark' helper function
  observe(
    if(!is.null(input$judgements)){
      values[["judgements"]] <- placeBookmark(isolate(input$bookletNumber), 
                                              difficulties, 
                                              hot_to_r(input$judgements))
    }
  )

  # instruct shiny to observe the shiny component 'bookletNumber'
  # when the component is interacted with, re-runs the helper function 
  # 'placeBookmark', re-defines the 'judgements' object to clear out 
  # work from last booklet
  observeEvent(input$bookletNumber, {
    values[["judgements"]] <- placeBookmark(input$bookletNumber, difficulties,
                                            data.frame(Judge = c(1:judges), 
                                                       Bookmark = 0))
  })

  # set the shiny component 'booklet' to render the plot returned from the 
  # helper function 'bookletPlot'- creates booklet line plot
  output$booklet <- renderPlot(
      bookletPlot(input$bookletNumber, difficulties, values[["judgements"]])
  )
  
  # set the save button to save the contents of the 'judgements' table to the
  # 'finalJudgements' reactive value
  observeEvent(input$save, {
    values[["finalJudgements"]] <- rbind(values[["finalJudgements"]], 
                                         hot_to_r(input$judgements))
  })
  
  # Impact Tab
  
  # set the shiny component 'results' to render an rHandsontable
  # which will be filled with the 'finalJudgements' reactive value object
  # table is the judgements decision/impact table
  output$results <- renderTable(
    analyzeResults(values[['finalJudgements']], scores)
  )
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server)


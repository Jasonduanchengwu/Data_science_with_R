library(shiny)

# # Define UI for dataset viewer app ----
# ui <- fluidPage(
#   # App title ----
#   titlePanel("Title"),
#   
#   # Sidebar layout with input and output definitions ----
#   fluidRow(
#     # column(4,
#     #        # Input: Text for providing a caption ----
#     #        # Note: Changes made to the caption in the textInput control
#     #        # are updated in the output area immediately as you type
#     #        # textInput(inputId = "caption",
#     #        #           label = "Caption:",
#     #        #           value = "Data Summary"),
#     #        
#     #        # Input: Selector for choosing dataset ----
#     #        selectInput(inputId = "dataset",
#     #                    label = "Variable of interest:",
#     #                    choices = c("rock", "pressure", "cars")),
#     #        
#     #        # Input: Numeric entry for number of obs to view ----
#     #        numericInput(inputId = "obs",
#     #                     label = "Number of observations to view:",
#     #                     value = 10)
#     # ),
#     
#     column(8,
#            tabsetPanel(
#              tabPanel("Patient characteristics",
#                       h5('subtitle1'),
#                       verbatimTextOutput("summary"),
#                       
#              selectInput(inputId = "dataset",
#                          label = "Variable of interest:",
#                          choices = c("rock", "pressure", "cars"))
#              ),
#              
#              tabPanel("Patient's ADT and ICU stay information",
#                       h5('subtitle2'),
#                       tableOutput("view"),
#                       
#              numericInput(inputId = "obs",
#                           label = "Number of observations to view:",
#                           value = 10)
#              )
#            ),
#            
#            mainPanel(
#              
#            )
#     )
#   )
# )

# Define UI
ui <- fluidPage(
  titlePanel("Shiny App with Custom Sidebars per Tab"),
  

  
  # Tabs
  tabsetPanel(
    # First tab with its own sidebar-like layout
    tabPanel("Patient characteristics",
             fluidRow(
               column(3, # Sidebar-like column for inputs
                      wellPanel( # Using wellPanel for sidebar appearance
                        selectInput("var", "Variable of interest:", choices = c("One", "Two", "Three")),
                        # Copy the line below to make a checkbox
                        checkboxInput("checkbox", label = "Remove outliers in IQR method for measurements?", value = FALSE)
                      )),
               
               # column(3, verbatimTextOutput("value")),
               
               column(9, # Main content column
                      h2("Content of Tab 1"),
                      textOutput("text1"))
             )),
    # Second tab with its own sidebar-like layout
    tabPanel("Patient's ADT and ICU stay information",
             fluidRow(
               column(3, # Sidebar-like column for inputs
                      wellPanel( # Using wellPanel for sidebar appearance
                        textInput("pid", label = h3("Patient ID"), value = "")
                      )),
               column(9, # Main content column
                      textOutput("text2"))
             ))
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$var,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
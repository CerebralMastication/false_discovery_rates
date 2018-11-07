library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Shiny Example"),
    
    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "alpha",
                         label = "Alpha:",
                         value = .05), 
            numericInput(inputId = "power",
                         label = "Power:",
                         value = .95), 
            numericInput(inputId = "sample_size",
                         label = "Sample Size:",
                         value = 2000)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: HTML table with calcs ----
            tableOutput("view")
            
        )
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    # Return the requested dataset ----
    datasetInput <- reactive({
        
        out_frame <- data.frame(initial_guess = c(0,.01,.05,.1,.25,.5,.75,.9,.95,.99,1))
        
        out_frame %>% 
            mutate( percent_report_sig = ((1-initial_guess) * input$alpha) + (initial_guess * input$power), 
                    pos_predict_rate =   (initial_guess * input$power) / percent_report_sig,
                    significant_per_run = percent_report_sig * input$sample_size, 
                    true_positive = pos_predict_rate * significant_per_run, 
                    num_wrong = (1-pos_predict_rate) * significant_per_run, 
                    percent_report_not_sig =  1-percent_report_sig, 
                    neg_predict_rate = 1-((1-input$power) * initial_guess) / percent_report_not_sig, 
                    total_accuracy = pos_predict_rate * percent_report_sig + neg_predict_rate * percent_report_not_sig
            ) 
    })
    
    # Generate a summary of the dataset ----
    output$view <- renderTable({
        datasetInput()
    })
    
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


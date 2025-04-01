library(shiny)

ui <- fluidPage(
  titlePanel("CSV Column Selector"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("column_selector")
    ),
    
    mainPanel(
      tableOutput("preview")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$column_selector <- renderUI({
    req(data())
    selectInput("column", "Select a Column", choices = names(data()))
  })
  
  output$preview <- renderTable({
    req(input$column)
    head(data()[, input$column, drop = FALSE])
  })
}

shinyApp(ui, server)

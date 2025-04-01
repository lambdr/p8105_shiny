# Load packages/scripts ----
library(shiny)
library(bslib)
library(maps)
library(mapproj)
source("helpers.R")

# Data etc
counties <- readRDS("data/counties.rds")
percent_map(counties$white, "darkgreen", "% White")

# Define UI ----
ui <- page_sidebar(
  title = "censusVis",
  sidebar = sidebar(
    helpText("Create demographic maps with information from the 2010 US Census."),
    selectInput(
      "select",
      "Choose a variable to display",
      choices = c("Percent White", "Percent Black", 
                     "Percent Hispanic", "Percent Asian"),
      selected = 1
      ),
    sliderInput(
      "range",
      "Range of interest:",
      min = 0,
      max = 100,
      value = c(0,100)
    )

  ),
  textOutput("selected_var"),
  textOutput("range")
)

# Define server logic ----
server <- function(input, output) {
  output$selected_var <- renderText({
    paste0("You have selected this: ", input$select)
  })
  
  output$range <- renderText({
    paste0("Your range goes from ", input$range[1], " to ",
           input$range[2])
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
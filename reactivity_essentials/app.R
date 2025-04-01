
# Load packages
library(shiny)
library(bslib)
library(tidyverse)

# Load data
load("movies.RData")

# UI
ui <- page_sidebar(
  sidebar = sidebar(
    selectInput("y", "Y-axis:", choices = c("IMDB Rating" = "imdb_rating", "Votes on IMDB" = "imdb_num_votes", "Critics score" = "critics_score", "Audience score" = "audience_score", "Movie runtime" = "runtime"), 
                selected = "critics_score"),
    selectInput("x", "X-axis:", choices = c("IMDB Rating" = "imdb_rating", "Votes on IMDB" = "imdb_num_votes", "Critics score" = "critics_score", "Audience score" = "audience_score", "Movie runtime" = "runtime"), 
                selected = "audience_score"),
    selectInput("selected_type", "Select movie type:", choices = levels(movies$title_type),
                selected = "Feature Film"),
    downloadButton("download_data", "Download data"),
    radioButtons(
        inputId = "filetype",
        label = "Select filetype:",
        choices = c("csv", "tsv"),
        selected = "csv"
    ),

    checkboxGroupInput(
        inputId = "selected_var",
        label = "Select variables:",
        choices = names(movies),
        selected = c("title", "title_type", "genre", "runtime", "mpaa_rating", "studio", "thtr_rel_year")
    )
  ),
  card(
    plotOutput("scatterplot"),
    uiOutput("n")
  ),
  card(DT::DTOutput("filtered_table"))
)

# Server
server <- function(input, output){
  
  movies_subset <- reactive({
    req(input$selected_type)
    movies |> 
      filter(title_type %in% input$selected_type) 
  })
  
  output$scatterplot <- renderPlot({
    movies_subset() |> 
      ggplot(aes(x = !!sym(input$x), y = !!sym(input$y))) + 
        geom_point() +
        theme_bw()
  })
  
  output$n <- renderUI({
    HTML(paste0(
      "<p>The plot displays the relationship between the audience and critics' scores of <br/>",
      nrow(movies_subset()), " <b>", input$selected_type, "</b> movies.</p>"
    ))
  })
  
  movies_filtered <- reactive({
    req(input$selected_var)
    movies |>
      select(input$selected_var)
  })
  
  output$filtered_table <- DT::renderDT({
    req(input$selected_var)
    movies_filtered()
    
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("movies.", input$filetype)
    }, 
    content = function(file) {
      if (input$filetype == "csv") {write_csv(movies |> select(input$selected_var), file)}
      if (input$filetype == "tsv") {write_tsv(movies |> select(input$selected_var), file)}
    }
  )
  
}

# App
shinyApp(ui = ui, server = server)
# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(tools)

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
  sidebar = sidebar(
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = c(
        "IMDB rating" = "imdb_rating",
        "IMDB number of votes" = "imdb_num_votes",
        "Critics Score" = "critics_score",
        "Audience Score" = "audience_score",
        "Runtime" = "runtime"
      ),
      selected = "audience_score"
    ),
    
    selectInput(
      inputId = "x",
      label = "X-axis:",
      choices = c(
        "IMDB rating" = "imdb_rating",
        "IMDB number of votes" = "imdb_num_votes",
        "Critics Score" = "critics_score",
        "Audience Score" = "audience_score",
        "Runtime" = "runtime"
      ),
      selected = "critics_score"
    ),
    
    selectInput(
      inputId = "z",
      label = "Color by:",
      choices = c(
        "Title Type" = "title_type",
        "Genre" = "genre",
        "MPAA Rating" = "mpaa_rating",
        "Critics Rating" = "critics_rating",
        "Audience Rating" = "audience_rating"
      ),
      selected = "mpaa_rating"
    ),
    
    sliderInput(
      inputId = "alpha",
      label = "Alpha:",
      min = 0, max = 1,
      value = 0.5
    ),
    
    sliderInput(
      inputId = "size",
      label = "Size:",
      min = 0, max = 5,
      value = 2
    ),
    numericInput("n_rows", "How many rows?", value = 6),
    actionButton("button", "Show")
    # actionButton("update_title", "Update Plot Title")
  ),
  
  card(plotOutput(outputId = "scatterplot")),
  card(
    DT::DTOutput("data_table")
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
 # plot_title <- eventReactive(input$update_title, {toTitleCase(input$plot_title)})
  
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      theme_bw() #+ labs(title = plot_title())
  })
  
  observeEvent(input$button, {cat("Showing", input$n_rows, "rows\n")})
  
  df <- eventReactive(input$button, {
    head(movies, input$n_rows)
  })
  
  output$data_table <- DT::renderDT({
    df()
    })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
# Load packages

library(shiny)
library(bslib)
library(tidyverse)

# Get the data

file <- "https://github.com/rstudio-education/shiny-course/raw/main/movies.RData"
destfile <- "movies.RData"

download.file(file, destfile)

# Load data

load("movies.RData")
movies <- movies |> 
  mutate(score_ratio = audience_score/critics_score)

# Define UI

ui <- page_sidebar(
  sidebar = sidebar(
    # Select variable for y-axis
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = c("IMDB Rating" = "imdb_rating", "Votes on IMDB" = "imdb_num_votes", "Critics score" = "critics_score", "Audience score" = "audience_score", "Movie runtime" = "runtime"),
      selected = "imdb_rating"
    ),
    # Select variable for x-axis
    selectInput(
      inputId = "x",
      label = "X-axis:",
      choices = c("IMDB Rating" = "imdb_rating", "Votes on IMDB" = "imdb_num_votes", "Critics score" = "critics_score", "Audience score" = "audience_score", "Movie runtime" = "runtime"),
      selected = "critics_score"
    ),
    selectInput(
      inputId = "z",
      label = "Colour by",
      choices = c("Title type" = "title_type", "Genre" = "genre", "MPAA rating" = "mpaa_rating", "Critics rating" = "critics_rating", "Audience rating" = "audience_rating"),
      selected = "mpaa_rating"
    ),
    sliderInput("alpha", "Transparency", min = 0, max = 1, value = 1),
    checkboxInput("show_datatable", "Show data table", value = TRUE),
    selectInput(
      "studio",
      "Select studio",
      choices = unique(movies$studio),
      selected = "Warner Bros.",
      selectize = TRUE,
      multiple = TRUE
    ),
    dateRangeInput(
      "date_range",
      "Movie Date Range",
      start = min(movies$thtr_rel_date), end = max(movies$thtr_rel_date),
      min = min(movies$thtr_rel_date), max = max(movies$thtr_rel_date),
      startview = "year"
    ),
    checkboxGroupInput(
      "selected_title_type",
      "Select title type:",
      choices = levels(movies$title_type),
      selected = levels(movies$title_type)
    ),
    radioButtons(inputId = "filetype",
                 label = "Select filetype:",
                 choices = c("csv", "tsv"),
                 selected = "csv"),
    
    checkboxGroupInput(inputId = "selected_var",
                       label = "Select variables:",
                       choices = names(movies),
                       selected = c("title")),
    
    HTML("Select filetype and variables, then hit 'Download data'."),
    downloadButton("download_data", "Download data")
  ),
  # Output: Show scatterplot
  card(plotOutput(outputId = "scatterplot", hover = "plot_brush")),
  card(
    verbatimTextOutput("regression"),
    htmlOutput("means")
  ),
  card(tableOutput("summary_table"))
)


# Define server

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes(x = !!sym(input$x), y = !!sym(input$y), color = !!sym(input$z))) +
      geom_point(alpha = input$alpha) +
      theme_bw()
  })
  
  output$densityplot <- renderPlot({
    ggplot(data = movies, aes(x = !!sym(input$x))) +
      geom_density(alpha = input$alpha) +
      theme_bw()
  })
  
  output$moviestable <- DT::renderDT({
    req(input$plot_brush)
    nearPoints(movies, input$plot_brush) |>
      select(title, audience_score, critics_score)
  })
  
  output$summary_table <- renderTable({
    movies |> 
      filter(title_type %in% input$selected_title_type) |> 
      group_by(mpaa_rating) |> 
      summarize(mean_score_ratio = mean(score_ratio), SD = sd(score_ratio), N = n())
    },
    striped = TRUE,
    spacing = "l",
    align = "lccr",
    digits = 4,
    width = "90%",
    caption = "Score ratio (audience / critics' scores) summary by MPAA rating"
    )
  
  output$regression <- renderPrint({
    x <- pull(movies, input$x)
    y <- pull(movies, input$y)
    summ <- lm(y ~ x, data = movies) |> 
      summary(digits = 3, signif.stars = FALSE)
    
    print(summ)
  })
  
  output$means <- renderUI({
    mean_x <- movies |> 
      pull(input$x) |> 
      mean() |> 
      round(2)
    
    mean_y <- movies |> 
      pull(input$y) |> 
      mean() |> 
      round(2)
    
    str_x <- paste0("Average ", input$x, ": ", mean_x)
    str_y <- paste0("Average ", input$y, ": ", mean_y)
    
    HTML(paste(str_x, str_y, sep="<br/>"))
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

# Create a Shiny app object

shinyApp(ui = ui, server = server)
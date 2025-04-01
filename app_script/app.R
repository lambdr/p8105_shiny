# Load packages and source scripts
library(shiny)
library(bslib)
library(tidyverse)
source("script_functions.R")

# Define UI ----
ui <- page_sidebar(
  # Javascript code to handle text input
  tags$script(HTML("
    $(document).on('keydown', function(e) {
      if (e.key === 'Enter' && e.target.id === 'text_input') {
        e.preventDefault();  // Prevent new line in textarea
        var textValue = $('#text_input').val().trim();
        if (textValue !== '') {
          Shiny.setInputValue('text_submit', textValue, {priority: 'event'});
          $('#text_input').val('');  // Clear input after sending
        }
      }
    });
  ")),
  titlePanel("Script Squared"),
  sidebar = sidebar(
    "Setup",
    fileInput("script_file", label = "Upload script", accept = ".txt"),
    uiOutput("act_selector"),
    uiOutput("scene_selector"),
    uiOutput("character_selector")
  ),
  card(
    "Line Practice",
    textOutput("starting_scene"),
    #actionButton("scene_start", "Start scene"),
    htmlOutput("cue_line"),
    textAreaInput("text_input", "Your line")
  ),
  card(
    "Line Notes",
    verbatimTextOutput("placeholder")
  )
)


# Define server logic ----
server <- function(input, output) {
    # Upload and process script
    ## Upload script
    script <- reactive({
      req(input$script_file)
      process_script(scan(input$script_file$datapath, what="", sep="\n"))
    })
    
    ## Select Act
    clean_act <- reactive({
      req(input$selected_act)
      script()[[input$selected_act]]
    })
    
    ## Select Scene
    scene <- reactive({
      req(input$selected_scene)
      clean_act()[[input$selected_scene]]
    })
    
    
    # Update UI 
    ## Select Act
    output$act_selector <- renderUI({
      req(script())
      selectInput("selected_act", "Act", choices = names(script()))
    })
    
    ## Select Scene
    output$scene_selector <- renderUI({
      req(clean_act())
      selectInput("selected_scene", "Scene", choices = names(clean_act()))
    })
    
    ## Select Character
    output$character_selector <- renderUI({
      req(scene())
      selectInput("selected_character", "Character", choices = scene()[["Characters"]])
    })
    
    
    # Line reading
    output$starting_scene <- renderText({
      req(input$selected_character)
      paste0("You have selected to read ", input$selected_character, " in ",
             input$selected_act, " ", input$selected_scene, ".")
    })
    
    # Give cue lines
    line <- reactiveVal(1)
    attempted_lines <- reactiveVal(character())
    
    # Clicking Next Line Button
    observeEvent(input$text_submit, {
      req(scene())
      if (line() >= nrow(lines_with_cues())) {
      } else {
        attempted_lines(c(attempted_lines(), input$text_submit))
        line(line() + 1)
      }
  })
    
    # Get working lines
    lines_with_cues <- reactive({
      req(scene())
      req(input$selected_character)
      
      call_lines_and_cues(scene()$Script, input$selected_character)
    })
    
   # Output cue line
   output$cue_line <- renderText({
      req(scene())
     
      cue_char <- lines_with_cues()[["cue_character"]][line()]
      cue_line <- lines_with_cues()[["cue_line"]][line()]
      
      HTML(paste0(cue_char, "<br/>", cue_line))
      })
   
   # Show attempted lines
   output$placeholder <- renderText({
     paste(attempted_lines(), collapse = "\n")
   })

}

# Run the app ----
shinyApp(ui = ui, server = server)
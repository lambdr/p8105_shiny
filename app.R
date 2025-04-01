library(shiny)

ui <- fluidPage(
  tags$script(HTML("
    $(document).on('keydown', function(e) {
      if (e.key === 'Enter') {
        var textValue = $('#text_input').val();
        if (textValue.trim() !== '') {
          Shiny.setInputValue('text_submit', textValue, {priority: 'event'});
          $('#text_input').val('');  // Clear input after sending
        }
      }
    });
  ")),
  textInput("text_input", "Enter text (press Enter to save):", ""),
  verbatimTextOutput("saved_text")
)

server <- function(input, output, session) {
  # Create a reactive value to store a vector of text inputs
  saved_text <- reactiveVal(character())
  
  # Observe the Enter key event and store input
  observeEvent(input$text_submit, {
    saved_text(c(saved_text(), input$text_submit))  # Append new input
  })
  
  # Display the stored text inputs
  output$saved_text <- renderText({
    paste(saved_text(), collapse = "\n")
  })
}

shinyApp(ui, server)

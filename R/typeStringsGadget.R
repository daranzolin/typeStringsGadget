typeStringsGadget <- function(...) {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("typeStringsGadget"),
    miniUI::miniContentPanel(
      shiny::textInput("sep", "Separator:", ", "),
      shiny::radioButtons("radio1", label = "Class:", choices = list("Character" = "char",
                                                                    "Numeric" = "num")),
      shiny::radioButtons("radio2", label = "Convert strings to title?", choices = list("Yes", "No")),
      shiny::p("Type here:"),
      shiny::tags$textarea(id = "strings", rows = 3, cols = 75, "Not, free, but, merely, licensed"),
      shiny::br(),
      shiny::p("Preview:"),
      shiny::verbatimTextOutput("preview")
    )
  )

  server <- function(input, output, session) {

    output$preview <- shiny::renderPrint({
      if (input$radio1 == "char") {
        s <- unlist(strsplit(as.character(input$strings), split = input$sep))
      } else {
        s <- as.numeric(unlist(strsplit(input$strings, split = input$sep)))
      }
      if (input$radio2 == "Yes") {
        s <- stringr::str_to_title(s)
      }
      s
    })

    shiny::observeEvent(input$done, {
      if (input$radio1 == "char") {
        rv <- unlist(strsplit(as.character(input$strings), split = input$sep))
      } else {
        rv <- as.numeric(unlist(strsplit(input$strings, split = input$sep)))
      }
      if (input$radio2 == "Yes") {
        rv <- stringr::str_to_title(rv)
      }
      stopApp(rv)
    })
  }
  viewer <- shiny::dialogViewer(dialogName = 'typeStringsGadget', width = 900, height = 900)
  shiny::runGadget(ui, server, stopOnCancel = TRUE, viewer = viewer)
}
#' Type strings hastily
#'
#' Call this function to open a gadget with a text box
#'
#' @export
typeStringsGadget <- function(...) {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("typeStringsGadget"),
    miniUI::miniContentPanel(
      shiny::fillRow(
        flex = c(1, 1, 3),
        shiny::fillCol(
          shiny::textInput("sep", "Separator:", ","),
          shiny::radioButtons("radio1", label = "Class:", choices = list("Character" = "char", "Numeric" = "num")),
          shiny::radioButtons("radio2", label = "Convert strings to title?", choices = list("Yes", "No")),
          shiny::radioButtons("radio3", label = "Trim white space?", choices = list("Yes", "No")),
          height = "75%"
          ),
        shiny::fillCol(
          shiny::br()
        ),
        shiny::fillCol(
          shiny::strong("Type here:"),
          shiny::tags$textarea(id = "strings", rows = 3, cols = 75, "Not, free, but, merely, licensed"),
          shiny::br(),
          shiny::br(),
          shiny::strong("Preview:"),
          shiny::verbatimTextOutput("preview"),
          height = "30%"
        )
      )
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
      if (input$radio3 == "Yes") {
        s <- stringr::str_trim(s)
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
      if (input$radio3 == "Yes") {
        rv <- stringr::str_trim(rv)
      }
      stopApp(rv)
    })
  }
  viewer <- shiny::dialogViewer(dialogName = 'typeStringsGadget', width = 900, height = 900)
  shiny::runGadget(ui, server, stopOnCancel = TRUE, viewer = viewer)
}

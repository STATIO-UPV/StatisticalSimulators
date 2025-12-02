# app/main.R

box::use(
  shiny,
  graphics[hist],
  shiny.i18n[Translator, usei18n, update_lang],
)

i18n <- Translator$new(translation_json_path = "app/translations/translations.json")
i18n$set_translation_language("en")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$fluidPage(
    usei18n(i18n),
    shiny$div(
      style = "float: right;",
      shiny$selectInput(
        ns("selected_language"),
        i18n$translate("Change language"),
        choices = i18n$get_languages(),
        selected = i18n$get_key_translation()
      )
    ),
    shiny$titlePanel(i18n$translate("Hello Shiny!"), windowTitle = NULL),
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        shiny$sliderInput(
          ns("bins"),
          i18n$translate("Number of bins:"),
          min = 1,
          max = 50,
          value = 30
        )
      ),
      shiny$mainPanel(
        shiny$plotOutput(ns("distPlot")),
        shiny$p(i18n$translate("This is description of the plot."))
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$observeEvent(input$selected_language, {
      update_lang(input$selected_language)
    })
    
    output$distPlot <- shiny$renderPlot({
      x <- datasets::faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(
        x,
        breaks = bins,
        col = "darkgray",
        border = "white",
        main = i18n$translate("Histogram of x"),
        ylab = i18n$translate("Frequency")
      )
    })
  })
}
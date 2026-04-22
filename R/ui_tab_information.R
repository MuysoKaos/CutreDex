tab_information <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 4,
      shiny::h4(shiny::textOutput(ns("japanese_name"))),
      shiny::h4(shiny::textOutput(ns("classification"))),
      DT::dataTableOutput(ns("typetable")),
      shiny::hr(),
      shiny::selectInput(ns("pokemon_type"), "Select Shape:", choices = "common", selected = "common"),
      shiny::imageOutput(ns("image_information"))
    ),
    shiny::column(
      width = 4,
      shiny::h4("Stats"),
      shiny::plotOutput(ns("pokemon_radarPlot")),
      shiny::h4("Male/Female"),
      plotly::plotlyOutput(ns("gender_piePlot"))
    ),
    shiny::column(
      width = 4,
      shiny::h4("Height"),
      shiny::numericInput(ns("human_height"), "Human Heigth (m):", value = 1.5, min = 0, max = 2),
      plotly::plotlyOutput(ns("barPlot_height")),
      shiny::br(),
      shiny::h4("Weight"),
      shiny::numericInput(ns("human_weight"), "Human Weigth (Kg):", value = 50, min = 20, max = 200),
      plotly::plotlyOutput(ns("barPlot_weight"))
    )
  )
}

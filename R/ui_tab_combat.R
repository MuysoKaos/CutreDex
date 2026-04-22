tab_combat <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 4,
      shiny::h4(shiny::textOutput(ns("japanese_name_combat"))),
      shiny::h4(shiny::textOutput(ns("classification_combat"))),
      DT::dataTableOutput(ns("typetable_combat")),
      shiny::hr(),
      shiny::selectInput(ns("pokemon_type_combat"), "Select Shape:", choices = "common", selected = "common"),
      shiny::imageOutput(ns("image_information_combat"))
    ),
    shiny::column(
      width = 2,
      shiny::h4("Stats"),
      shiny::selectInput(ns("generation_rival"), "Generation Rival:", choices = unique(dataset$generation)),
      shiny::selectInput(ns("pokemon_init_rival"), "Pokemon Rival:", choices = "Bulbasaur", selected = "Bulbasaur")
    ),
    shiny::column(
      width = 4,
      shiny::h4(shiny::textOutput(ns("japanese_name_rival"))),
      shiny::h4(shiny::textOutput(ns("classification_rival"))),
      DT::dataTableOutput(ns("typetable_rival")),
      shiny::hr(),
      shiny::selectInput(ns("pokemon_type_rival"), "Select Shape:", choices = "common", selected = "common"),
      shiny::imageOutput(ns("image_information_rival"))
    )
  )
}

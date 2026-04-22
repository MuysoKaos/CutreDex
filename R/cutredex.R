### main ui
### solo quiero poner el esqueleto inicial del codigo

main_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_navbar(
    theme = bslib::bs_theme(bootswatch = "minty"), ### More Themes bootswatch.com
    title = "Cutre-dex",
    sidebar = bslib::sidebar(
      open = TRUE,
      shiny::selectInput(ns("generation"), "Generation:", choices = unique(dataset$generation)),
      shiny::selectInput(ns("pokemon_init"), "Pokemon:", choices = "Bulbasaur", selected = "Bulbasaur")
    ),
    # ponerle iconos a los paneles: fontawesome.com/icons
    bslib::nav_panel("Information", icon = shiny::icon("fas fa-info"), tab_information("cutredex")),
    bslib::nav_panel("Combat", icon = shiny::icon("fas fa-hand-fist"), tab_combat("cutredex"))
  )
}


## main server
# voy a poner los diferentes servers de cada nav_panel

main_server <- function() {
  server_observer("cutredex")
  data_information <- server_information("cutredex")
  server_combat("cutredex", reactive_information = data_information)
}

##### Shiny app

cutredex <- function() {
  shiny::shinyApp(
    ui = shiny::shinyUI(main_ui("cutredex")),
    server = function(input, output, session) {
      main_server()
    }
  )
}

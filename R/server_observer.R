server_observer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$generation, {
      options_pokemon_dataset <- dataset |>
        dplyr::filter(generation == input$generation) |>
        dplyr::select(name) |>
        dplyr::pull()

      shiny::updateSelectizeInput(session,
        inputId = "pokemon_init",
        choices = options_pokemon_dataset,
        selected = options_pokemon_dataset[1]
      )
    })

    shiny::observeEvent(input$generation_rival, {
      options_pokemon_dataset <- dataset |>
        dplyr::filter(generation == input$generation_rival) |>
        dplyr::select(name) |>
        dplyr::pull()

      shiny::updateSelectizeInput(session,
        inputId = "pokemon_init_rival",
        choices = options_pokemon_dataset,
        selected = options_pokemon_dataset[1]
      )
    })
  })
}

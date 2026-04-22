server_combat <- function(id, reactive_information) {
  shiny::moduleServer(id, function(input, output, session) {
    list_objects_init_ <- reactive_information$list_objects_init_

    ########################################################
    ########### OBSERVE

    shiny::observeEvent(input$pokemon_init_rival,
      {
        pokedex_number <- pokedex_number_rival_()

        list_shape <- data_image |>
          dplyr::filter(name == pokedex_number) |>
          dplyr::arrange(complement) |>
          dplyr::select(complement) |>
          dplyr::pull()

        shiny::updateSelectInput(session,
          inputId = "pokemon_type_rival",
          choices = list_shape,
          selected = "common"
        )
      },
      ignoreInit = TRUE
    )

    ########################################################
    ########## REACTIVE

    pokedex_number_rival_ <- shiny::reactive({
      dataset |>
        dplyr::filter(name == input$pokemon_init_rival) |>
        dplyr::select(pokedex_number) |>
        dplyr::pull()
    })

    ########################################################
    ########## OUTPUT

    output$image_information_rival <- shiny::renderImage(
      {
        pokedex_number <- pokedex_number_rival_()

        file_image <- data_image |>
          dplyr::filter(name == pokedex_number, complement == input$pokemon_type_rival) |>
          dplyr::select(image_files) |>
          dplyr::pull()

        ruta_imagen <- file.path("img", file_image)

        list(
          src = ruta_imagen,
          contentType = "image/png",
          alt = "This is a Pokemon",
          width = 256,
          height = 256
        )
      },
      deleteFile = FALSE
    )

    output$japanese_name_rival <- shiny::renderText({
      number <- pokedex_number_rival_()

      japanese_name <- dataset |>
        dplyr::filter(pokedex_number == number) |>
        dplyr::select(japanese_name) |>
        dplyr::pull()

      paste0("Japanese Name: ", japanese_name)
    })

    output$classification_rival <- shiny::renderText({
      number <- pokedex_number_rival_()

      classification <- dataset |>
        dplyr::filter(pokedex_number == number) |>
        dplyr::select(classfication) |>
        dplyr::pull()

      paste0("Classification: ", classification)
    })

    output$typetable_rival <- DT::renderDataTable(
      dataset |>
        dplyr::filter(name == input$pokemon_init_rival) |>
        dplyr::select(type1, type2) |>
        dplyr::mutate(
          type1 = toupper(type1),
          type2 = toupper(type2)
        ) |>
        DT::datatable(
          escape = FALSE, rownames = FALSE, filter = "none",
          options = list(
            dom = "tp", searching = FALSE, paging = FALSE,
            initComplete = DT::JS(
              "function(settings, json) {",
              "$('table.dataTable').css({'font-size': '13px'});",
              "}"
            )
          ),
          colnames = c("Principal Type", "Secondary Type")
        ) |>
        DT::formatStyle(
          "type1",
          target = "cell",
          backgroundColor = DT::styleEqual(
            type, coltype
          ),
          color = "white",
          fontWeight = "bold",
          textAlign = "center"
        ) |>
        DT::formatStyle(
          "type2",
          target = "cell",
          backgroundColor = DT::styleEqual(
            type, coltype
          ),
          color = "white",
          fontWeight = "bold",
          textAlign = "center"
        )
    )

    output$image_information_combat <- shiny::renderImage(
      {
        list_objects_init <- list_objects_init_()

        ruta_imagen <- list_objects_init$ruta_imagen

        list(
          src = ruta_imagen,
          contentType = "image/png",
          alt = "This is a Pokemon",
          width = 256,
          height = 256
        )
      },
      deleteFile = FALSE
    )

    output$japanese_name_combat <- shiny::renderText({
      list_objects_init <- list_objects_init_()

      japanese_name <- list_objects_init$japanese_name

      paste0("Japanese Name: ", japanese_name)
    })

    output$classification_combat <- shiny::renderText({
      list_objects_init <- list_objects_init_()

      classification <- list_objects_init$classfication

      paste0("Classification: ", classification)
    })

    output$typetable_combat <- DT::renderDataTable({
      list_objects_init <- list_objects_init_()

      list_objects_init$type_init |>
        DT::datatable(
          escape = FALSE, rownames = FALSE, filter = "none",
          options = list(
            dom = "tp", searching = FALSE, paging = FALSE,
            initComplete = DT::JS(
              "function(settings, json) {",
              "$('table.dataTable').css({'font-size': '13px'});",
              "}"
            )
          ),
          colnames = c("Principal Type", "Secondary Type")
        ) |>
        DT::formatStyle(
          "type1",
          target = "cell",
          backgroundColor = DT::styleEqual(
            type, coltype
          ),
          color = "white",
          fontWeight = "bold",
          textAlign = "center"
        ) |>
        DT::formatStyle(
          "type2",
          target = "cell",
          backgroundColor = DT::styleEqual(
            type, coltype
          ),
          color = "white",
          fontWeight = "bold",
          textAlign = "center"
        )
    })
  })
}

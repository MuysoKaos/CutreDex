server_information <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ########################################################
    ########### OBSERVE

    shiny::observeEvent(input$pokemon_init,
      {
        pokedex_number <- pokedex_number_()

        list_shape <- data_image |>
          dplyr::filter(name == pokedex_number) |>
          dplyr::arrange(complement) |>
          dplyr::select(complement) |>
          dplyr::pull()

        shiny::updateSelectInput(session,
          inputId = "pokemon_type",
          choices = list_shape,
          selected = "common"
        )
      },
      ignoreInit = TRUE
    )

    ########################################################
    ########## REACTIVE

    pokedex_number_ <- shiny::reactive({
      dataset |>
        dplyr::filter(name == input$pokemon_init) |>
        dplyr::select(pokedex_number) |>
        dplyr::pull()
    })

    list_objects_init_ <- shiny::reactive({
      number <- pokedex_number_()

      file_image <- data_image |>
        dplyr::filter(name == number, complement == input$pokemon_type) |>
        dplyr::select(image_files) |>
        dplyr::pull()

      ruta_imagen <- file.path("img", file_image)

      info_init <- dataset |>
        dplyr::filter(pokedex_number == number) |>
        dplyr::select(japanese_name, classfication)

      type_init <- dataset |>
        dplyr::filter(name == input$pokemon_init) |>
        dplyr::select(type1, type2) |>
        dplyr::mutate(
          type1 = toupper(type1),
          type2 = toupper(type2)
        )

      list(
        ruta_imagen = ruta_imagen,
        japanese_name = info_init$japanese_name,
        classfication = info_init$classfication,
        type_init = type_init
      )
    })

    ########################################################
    ########## OUTPUT

    output$image_information <- shiny::renderImage(
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

    output$japanese_name <- shiny::renderText({
      list_objects_init <- list_objects_init_()

      japanese_name <- list_objects_init$japanese_name

      paste0("Japanese Name: ", japanese_name)
    })

    output$classification <- shiny::renderText({
      list_objects_init <- list_objects_init_()

      classification <- list_objects_init$classfication

      paste0("Classification: ", classification)
    })

    output$typetable <- DT::renderDataTable({
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

    output$pokemon_radarPlot <- shiny::renderPlot({
      data <- dataset |>
        dplyr::filter(name == input$pokemon_init) |>
        dplyr::select(name, attack, defense, hp, sp_attack, sp_defense, speed) |>
        dplyr::bind_rows(means)

      data_norm <- rbind(max_vals, min_vals, data[, -1])
      rownames(data_norm) <- c("Max", "Min", input$pokemon_init, "Average")

      colores <- c("#2980ef", "#e62829")

      fmsb::radarchart(data_norm,
        axistype = 2,
        pcol = colores,
        pfcol = adjustcolor(colores, alpha.f = 0.3),
        plwd = 3, # Asegurar que los valores de línea sean válidos
        vlcex = 1.2
      )

      legend("topright", legend = c(input$pokemon_init, "Average"), col = colores, lwd = 3, bty = "n")
    })

    output$gender_piePlot <- plotly::renderPlotly({
      valor <- dataset |>
        dplyr::filter(name == input$pokemon_init) |>
        dplyr::select(percentage_male) |>
        dplyr::pull()

      if (is.na(valor)) {
        # Si es NA, el gráfico será completamente gris
        datos <- data.frame(
          categoria = c("Sin datos"),
          valor = c(100)
        )
        colores <- c("#9fa19f")
      } else {
        # Si es un número, el gráfico mostrará la comparación en rosa y azul
        datos <- data.frame(
          categoria = c("Female", "Male"),
          valor = c(100 - valor, valor)
        )
        colores <- c("#ff99ff", "#3399ff")
      }

      # Crear el gráfico de pastel
      plotly::plot_ly(datos,
        labels = ~categoria, values = ~valor, type = "pie",
        marker = list(colors = colores),
        textinfo = "percent+label", # Muestra el porcentaje y el nombre
        textfont = list(size = 20, weight = "bold")
      ) |>
        plotly::layout(title = "", showlegend = FALSE)
    })

    output$barPlot_height <- plotly::renderPlotly({
      valor <- dataset |>
        dplyr::filter(name == input$pokemon_init) |>
        dplyr::select(height_m) |>
        dplyr::pull()

      alturas <- data.frame(
        entidad = c(input$pokemon_init, "Human"),
        altura = c(valor, input$human_height)
      )

      # Crear gráfico de barras
      p <- plotly::plot_ly(alturas,
        x = ~entidad, y = ~altura, type = "bar",
        marker = list(color = c("red", "blue"))
      ) |>
        plotly::layout(
          title = "",
          xaxis = list(
            title = "", # Aumentar tamaño y negrilla
            tickfont = list(size = 18, weight = "bold")
          ),
          yaxis = list(title = "Height (m)")
        )


      p
    })

    output$barPlot_weight <- plotly::renderPlotly({
      valor <- dataset |>
        dplyr::filter(name == input$pokemon_init) |>
        dplyr::select(weight_kg) |>
        dplyr::pull()

      pesos <- data.frame(
        entidad = c(input$pokemon_init, "Human"),
        peso = c(valor, input$human_weight)
      )

      # Crear gráfico de barras
      p <- plotly::plot_ly(pesos,
        x = ~peso, y = ~entidad, type = "bar",
        marker = list(color = c("red", "blue"))
      ) |>
        plotly::layout(
          title = "",
          xaxis = list(
            title = "", # Aumentar tamaño y negrilla
            tickfont = list(size = 18, weight = "bold")
          ),
          yaxis = list(title = "Weight (kg)")
        )


      p
    })

    return(list(list_objects_init_ = list_objects_init_))
  })
}

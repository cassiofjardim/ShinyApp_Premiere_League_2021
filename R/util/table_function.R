table_style <-
  function(dataframe,
           height = 'auto',
           width = 350,
           fontsize = NULL,
           cellPadding = NULL,
           sizing_default = 10,
           page_size_opt_vec = c(4, 8, 12)) {


    dataframe %>%

      reactable::reactable(
        style = list(fontFamily = 'Roboto'),
        pagination = TRUE,
        height = height,
        width = width,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = sizing_default,
        # filterable = TRUE,
        showSortIcon = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = page_size_opt_vec,

        theme = reactable::reactableTheme(
          borderColor = "lightgray",
          borderWidth = .5,
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5f9",
          backgroundColor = '#fff', # cells background
          cellPadding = 4,
          style = list(fontFamily = "Exo 2",
                       fontSize = 12),
          searchInputStyle = list(width = "100%"),
          # tableStyle = list(border = '2px'),

          headerStyle = list(
            # padding = '7.5px',
            background = "whitesmoke"),

          footerStyle = list(backgroundColor = "whitesmoke")
          # cellStyle = list(padding = '10px')

        ),

        defaultColDef = reactable::colDef(
          maxWidth = 80,
          vAlign = "center",
          headerVAlign = "bottom",
          align = 'center',
          style  = list(fontWeight = "bold",
                        backgroundColor = 'whitesmoke'),
          headerStyle = list(background = "whitesmoke"),

          footerStyle = list(fontWeight = "bold")
        ),

        columns = list(

          Squad = reactable::colDef(
            name = "Equipes",
            width = 120,
            style = list(
              fontSize = 12,
              position = "sticky",
              left = 0,
              background = "white",
              zIndex = 1,
              background = "rgba(0, 0, 0, 0.05)"
            ),
            headerStyle = list(position = "sticky",
                               left = 0,
                               background = "white",
                               zIndex = 1),

            cell = function(value){
              img_src <- knitr::image_uri(paste0('www/img/2020/',value,'.png'))
              tagList(
                div(
                  style  ='display: flex;gap:1em;font-weight: 700;border-radius;50%;',
                  tags$img(src = img_src, width = '16px', height = '16px'),
                  value))
            }

            # cell = function(value) {
            #   # Render as an X mark or check mark
            #   if (value == "None") "\u274c No" else "\u2714\ufe0f Yes"
            # }

          ),

          Rk = reactable::colDef(
            name = "Position",

            style = list(
              fontSize = 12,
              background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            ),
            headerStyle = list(position = "sticky",
                               left = 0,
                               background = "white",
                               zIndex = 0)

          ),

          Season_End_Year = reactable::colDef(
            name = "Temporada",
            style = list(
              fontSize = 12,
              background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            )),

          MP = reactable::colDef(
            name = "Games",
            style = list(
              fontSize = 12
              # background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            )),

          W = reactable::colDef(
            name = "Winnings",
            style = list(
              fontSize = 12
              # background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            )),
          L = reactable::colDef(
            name = "Derrotas",
            style = list(
              fontSize = 12
              # background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            )),

          Gls = reactable::colDef(
            name = "Goals",
            width = 120,
            style = list(
              fontSize = 12
              # background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            )),

          Ast = reactable::colDef(
            name = "Assistances",
            width = 120,
            style = list(
              fontSize = 12
              # background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            )),

          G_plus_A = reactable::colDef(
            name = "Goals + Assistances",
            width = 120,
            style = list(
              fontSize = 12
              # background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            )),

          Cmp_Short = reactable::colDef(
            name = "Completed Short Passes",
            width = 140,
            style = list(
              fontSize = 12
              # background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            )),

          Cmp_Medium = reactable::colDef(
            name = "Completed Medium Passes",
            width = 140,
            style = list(
              fontSize = 12
              # background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            )),

          Cmp_Long = reactable::colDef(
            name = "Completed Long Passes",
            width = 140,
            style = list(
              fontSize = 12
              # background = "white"

              # background = "rgba(0, 0, 0, 0.05)"
            ))



            # headerStyle = list(position = "sticky",
            #                    left = 0,
            #                    background = "white",
            #                    zIndex = 0)

           ))
  }


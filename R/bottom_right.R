bottom_right_UI <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      style = "grid-area: bottom_right_two",
      class = 'bottom_right_two',
      h5("SHOOTING AND SHOOTS ON TARGET (SoT) - LAST 5 Games"),

      div(

        class = 'shoots', style = 'display: flex;',

        highchartOutput(ns('shoots_bar_chart'),height = 250, width = 300),

        highchartOutput(ns('shoots_pie_chart'),height = 250, width = 300))



      # checkboxGroupButtons(
      #   inputId = "tables",
      #   label = "Top-5 Players",size = 'xs',
      #   choices = c("A",
      #               "B", "C", "D"),
      #   status = "primary",
      #   checkIcon = list(
      #     yes = icon("ok",
      #                lib = "glyphicon"),
      #     no = icon("xmark",
      #               lib = "glyphicon")
      #   )
      # )

      # reactableOutput(ns('table_right'), width = 'auto')

    ),
    div(
      class = 'bottom_right_one',
      style = "grid-area: bottom_right_one",

      h5("ATTEMPTED PASSES (Comparision): Begining and End Season ", tags$img(src = 'img/exclamation.png', width = '18px', height = '18px')),

      checkboxGroupButtons(
        inputId = "tables",
        label = "Top-5 Players",size = 'xs',
        choices = c("A",
                    "B", "C", "D"),
        status = "primary",
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon"),
          no = icon("xmark",
                    lib = "glyphicon")
        )
      ),

      highchartOutput(ns('attempted_pass'),height = 250, width = 'auto')


    )

  )
}

bottom_right_Server <- function(id,app_control_input) {
  moduleServer(
    id,
    function(input, output, session) {

      output$name <- renderUI({
        div(
          app_control_input(),
          style = "font-family: 'Exo 2', sans-serif; display: flex;",
          class = 'animated_div',
          tags$img(src = str_replace(
            paste0('img/', app_control_input(), '.png'), " ", "_"
          ),
          style = 'border-radius:50%; height: 100px;'),



        )


      })

      output$shoots_bar_chart <- renderHighchart({

        highchart(type = 'chart') %>%
          hc_add_series(data = shooting_sot[[app_control_input()]],
                        showInLegend = TRUE,
                        hcaes(x = Opponent, y = Sh_Standard),
                        name = paste0(app_control_input(),' - Shoots'),
                        color = '#484848' ,

                        type = 'column')%>%

          hc_add_series(data = shooting_sot[[app_control_input()]],

                        hcaes(x = Opponent, y = SoT_Standard),
                        color  = '#FFB67A',
                        name = paste0(app_control_input(),' - Shoots on Target (soT)'),

                        plotBorderWidth = 0,

                        type = 'column') %>%
          hc_plotOptions(series = list(borderRadius = 2.5))%>%



          hc_credits(enabled = TRUE) %>%
          hc_title(text = "") %>%
          hc_subtitle(text = "") %>%
          hc_legend(enabled = TRUE) %>%
          hc_xAxis(enabled = FALSE,title = FALSE, categories = shooting_sot[[app_control_input()]]$Opponent) %>%
          hc_yAxis(enabled = FALSE,title = FALSE,
                   labels = list(enabled = FALSE)) %>%
          hc_size( height = 250, width = 300) %>%
          hc_boost(enabled = FALSE)


      })


      output$shoots_pie_chart <- renderHighchart({

        highchart(type = 'chart') %>%
          hc_add_series(data = shooting_sot[[app_control_input()]],
                        showInLegend = TRUE,
                        hcaes(x = Opponent, y = Sh_Standard),
                        name = 'Manchester City - Shoots',
                        colorByPoint = TRUE,
                        colors = list('#484848' , '#FFB67A','#484848' , '#FFB67A','#484848'),
                        type = 'pie') %>% hc_size( height = 250)


      })


      output$attempted_pass <- renderHighchart({
        highchart(type = 'chart')%>%
          hc_add_series(
            data = passing_df[[app_control_input()]],
            hcaes(x = Opponent, y = Att_Short, group = Opponent),
            colorByPoint = TRUE,
            colors = list('#484848' , '#FFB67A'),
            showInLegend = FALSE,
            type = 'lollipop'
          )%>%
          hc_tooltip(share = TRUE)%>%
          hc_size( height = 250) %>%
          hc_title(text = "How Passing evolved during this Season?")%>%
          hc_subtitle(text = 'A Beggining and End Season Comparision')%>%
           hc_xAxis(enabled = FALSE,
                   title = "First 18 Games and Last 18 Game",
                   categories = c('First 18 Games','Second 18 Games'))
      })




    }
  )
}

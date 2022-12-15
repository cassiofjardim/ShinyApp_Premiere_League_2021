overview_UI <- function(id) {
  ns <- NS(id)
  tagList(div(
    class = 'overview',
    div(style  ='display: flex; gap: 0em;justify-content: space-around;justify-content: space-between;
    padding: 0 1em; ',
    div(class = "typewriter main_title",
        h5(
          'Overview', tags$span(paste0("(", Sys.time(), ")"), class = 'date_time')
        ),
        h6(uiOutput(ns('name')))),
    div(class = 'small_boxes',
        h6('2022 - Seasons Number', style  ='grid-area: season;'),
        h4(style = 'line-height: 0.25em;',
          # tags$img(src = 'img/small_box_1.png', width = '14px', height = '14px'),
           htmlOutput(ns('age')),tags$br(),
           tags$span('AGE (Average)', style = 'color: gray;font-size: .65em;font-weight: 500;display: inline-block; padding: .75em 0;')),
        h4(style = 'line-height: 0.25em;',
          # tags$img(src = 'img/small_box_2.png', width = '14px', height = '14px'),
           htmlOutput(ns('games')),tags$br(), tags$span('GAMES', style = 'color: gray;font-size: .65em;font-weight: 500;display: inline-block; padding: .75em 0;')),
        h4(style = 'line-height: 0.25em;',
          # tags$img(src = 'img/small_box_3.png', width = '14px', height = '14px'),
           htmlOutput(ns('goals')),tags$br(), tags$span('GOALS', style = 'color: gray;font-size: .65em;font-weight: 500;display: inline-block; padding: .75em 0;')),
        h4(style = 'line-height: 0.25em;',
          # tags$img(src = 'img/small_box_4.png', width = '14px', height = '14px'),
           htmlOutput(ns('assistances')),tags$br(), tags$span('ASSISTANCES', style = 'color: gray;font-size: .65em;font-weight: 500;display: inline-block; padding: .75em 0;'))
    )),

    div(style = 'display: flex; border-bottom: 1px solid lightgray;justify-content: space-between;',
        div(h6(
          'On average the strikers has played 37 games this season '
        )),

        div(h6(
          'On average each player has played 43 games this season '
        ))),

    div(
      style = 'display: flex;
        gap: 2em;margin: 2em 0;
        border-bottom: 1px solid lightgray;',
      div(class = 'left_chart',style = 'flex: 1;',
        h6(
          'All Seasons Historical: Goals and Assistances',
          tags$img(
            src = 'img/icons/goal.png',
            width = '14px',
            height = '14px'
          )
        ),
        # style  = 'height: fit-content; width : fit-content; flex:1',
        highchartOutput(ns('chart_left_3'), width = 'auto', height = 250),

        # shinyWidgets::sliderTextInput(
        #   width = '100%',
        #   inputId =  ns("left_chart_overview"),
        #   label = "",
        #   choices = chart_1$Player,
        #   selected = '2007')
        ),


      div(class = 'right_chart',style = 'flex: 1;',
        h6(

          'TOP-10 PLAYERS: NATION, CARDS AND MINUTES PLAYING'
          # tags$img(
          #   src = 'img/icons/yellow_red_cards.png',
          #   width = '14px',
          #   height = '14px'
          # )
        ),
        # style  = 'height: fit-content; width : fit-content; flex:1',
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
        # ),

        reactableOutput(ns('table_left'), width = 'auto', height = 250))),

    div(style = 'display: flex;flex-direction: column; justify-content: space-between; border-bottom: 1px solid lightgray;',
        div(
          class = 'attrition',

          h5('GOALS AND EXPECTED GOALS (xG)'),

          checkboxGroupButtons(
            inputId = "Id059",
            label = "SELECT PERIOD",
            size = 'xs',
            choices = c("A",
                        "B", "C", "D"),
            status = "primary",
            checkIcon = list(
              yes = icon("ok",
                         lib = "glyphicon"),
              no = icon("xmark",
                        lib = "glyphicon")
            )
          )
        ),
        tags$span(class = 'subtitle_main',
                  tags$img(src = 'img/small_box_1.png', width = '14px', height = '14px'),
                  h5('85%', class = 'percent', style = '    display: inline;font-weight: 700;
    font-size: 1.25em;'),
                  tags$span('vs. previous month', style = "font-family: 'Exo 2';color: gray;")),

        highchartOutput(ns('main_4'), height = 250)
        )

  ))
}

overview_Server <- function(id, app_control_input) {
  moduleServer(id,
               function(input,
                        output,
                        session) {



                 output$name <- renderUI({
                   div(
                     str_to_lower(app_control_input()),
                     style = "font-family: 'Exo 2', sans-serif; display: flex;",
                     class = 'animated_div') })


#**************************************************************************************
                 output$age <- renderUI({
                   p(
                     # players_stats %>% mutate(Country = str_to_upper(Country)) %>% filter(Country ==   app_control_input()) %>% pull(Age),
                     overview_card %>% mutate(Squad = str_to_upper(c(sort(elements_names),sort(elements_names)))) %>% filter(Squad == app_control_input() & Team_or_Opponent == 'team') %>% pull(Age),
                     style = "font-family: 'Exo 2', sans-serif; display: flex;font-weight: 900;font-size: 2em;")


                 })

                 output$games <- renderUI({
                   p(
                     # players_stats %>% mutate(Country = str_to_upper(Country)) %>% filter(Country ==   app_control_input()) %>% pull(Games),
                     38,
                     style = "font-family: 'Exo 2', sans-serif; display: flex;font-weight: 900;font-size: 2em;")


                 })
                 output$goals <- renderUI({
                   p(
                     # players_stats %>% mutate(Country = str_to_upper(Country)) %>% filter(Country ==   app_control_input()) %>% pull(Goals),
                     overview_card %>% mutate(Squad = str_to_upper(c(sort(elements_names),sort(elements_names)))) %>% filter(Squad == app_control_input() & Team_or_Opponent == 'team') %>% pull(Gls),
                     style = "font-family: 'Exo 2', sans-serif; display: flex;font-weight: 900;font-size: 2em;")


                 })

                 output$assistances <- renderUI({
                   p(
                     # players_stats %>% mutate(Country = str_to_upper(Country)) %>% filter(Country ==   app_control_input()) %>% pull(Assistances),
                     overview_card %>% mutate(Squad = str_to_upper(c(sort(elements_names),sort(elements_names)))) %>% filter(Squad == app_control_input() & Team_or_Opponent == 'team') %>% pull(Ast),
                     style = "font-family: 'Exo 2', sans-serif; display: flex;font-weight: 900;font-size: 2em;")


                 })
#***********************************************************************************

                 output$chart_left_3 <- renderHighchart({

                    highchart(type = 'chart') %>%
                     hc_add_series(data = chart_1[[app_control_input()]],

                                   hcaes(x = Player, y = Gls),
                                   # groupPadding = .5,
                                   pointWidth = 10,
                                   name = 'Goals',
                                   color = '#484848' ,
                                   type = 'column')%>%



                     hc_add_series(data = chart_1[[app_control_input()]],

                                    hcaes(x = Player, y = Ast),
                                    # groupPadding = .5,
                                    pointWidth = 10,
                                    color  = '#FFB67A',
                                    name = 'Assistances',


                                    type = 'column') %>%
                      hc_plotOptions(series = list(borderRadius = 2.5))%>%



                      hc_credits(enabled = FALSE) %>%
                      hc_title(text = "") %>%
                      hc_subtitle(text = "") %>%
                      hc_legend(enabled = TRUE) %>%
                      hc_xAxis(enabled = FALSE,title = FALSE, categories = chart_1[[app_control_input()]]$Player) %>%
                      hc_yAxis(enabled = FALSE,title = FALSE,
                               labels = list(enabled = FALSE)) %>%
                      hc_size( height = 250) %>%
                      hc_boost(enabled = FALSE)


                 })


#***********************************************************************************
                 output$table_left <- renderReactable({

                     reactable(
                       table_overview_df[[app_control_input()]],
                       highlight = TRUE,

                       bordered = TRUE,
                       outlined = TRUE,
                       pagination = TRUE,
                       width = 'auto',
                       height = 250,
                       defaultColDef = colDef(headerVAlign = 'center',headerStyle = list(fontSize  ='12px'),
                         # filterable = TRUE,
                         minWidth = 75,
                         vAlign = "top",
                         style = list(fontSize = '11px',
                                      textAlign = 'center')
                       ),

                       theme = reactableTheme(backgroundColor = 'white',

                         cellPadding = '2px 2px',
                       ),
                       columns = list(
                         CrdY = colDef(name = "Yellow Card"),
                         CrdR = colDef(name = "Red Card"),
                         Min_Playing_Time = colDef(name = "Minutes")

                       )
                     )



                 })

                 output$main_4 <- renderHighchart({

                   highchart(type = 'chart') %>%
                     hc_add_series(data = chart_1[[app_control_input()]],

                                   hcaes(x = Player, y = round(mean(Gls),2)),
                                   shadow = list(
                                     color = 'black',
                                     width = 3.5,
                                     offsetX = 0,
                                     offsetY = 0
                                   ),

                                   name = 'Teams"s Average Goals',
                                   color = 'black' ,
                                   type = 'line')%>%
                     hc_add_series(data =  chart_2[[app_control_input()]],
                                   showInLegend = FALSE,
                                   hcaes(x = Player, y = xG_Expected),
                                   groupPadding = .5,
                                   pointWidth = 20,
                                   name = 'xG',


                                   color = '#484848' ,

                                   type = 'column')%>%

                     hc_add_series(data = chart_2[[app_control_input()]],
                                   showInLegend = FALSE,

                                   hcaes(x = Player, y = Gls),
                                   name = 'Goals',

                                   groupPadding = .5,
                                   pointWidth = 10,
                                   color  = '#FFB67A',
                                   plotBorderWidth = 0,


                                   type = 'column') %>%

                     hc_xAxis(enabled = FALSE,title = FALSE, categories = chart_2[[app_control_input()]]$Player) %>%

                     hc_plotOptions(series = list(borderRadius = 2.5,
                                                  dashStyle = 'shortdot'))
                 })




               })
}

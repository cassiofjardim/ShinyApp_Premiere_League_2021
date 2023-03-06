source('R/metrics_to_choose.R')



csvDownloadButton <-
  function(id,
           filename = "data.csv",
           label = "Download as CSV") {
    tags$button(
      style = '
    background: #C8102E; color: whitesmoke; font-weight: 500;
    padding: 0.5em; border-radius: 5px; border: none;',
      tagList(icon("download"), label),
      onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
    )
  }

module_league_UI <- function(id) {
  ns <- NS(id)
  tagList(div(
    class = 'league_level',

    fileInput(
      ns('datafile'),
      'Choose CSV file',
      accept = c('csv', 'comma-separated-values', '.csv')
    ),

    div(
      class = 'first_infos',

      uiOutput(outputId = ns('excel'))
    ),

    hr(class = 'horizontal_line'),

    div(
      class = 'chart1_table',
      div(
        style = 'display:flex; justify-content: space-between;',
        h6(
          glue::glue("All Season Historical: {standard_metrics[1]} and {standard_metrics[2]}"),
          tags$img(
            src = 'img/icons/goal.png',
            width = '14px',
            height = '14px'
          )
        ),
        h6(
          glue::glue('All Season Historical (Standardized Variable):
                     {paste0(standard_metrics[1],"_std")}'),
          tags$img(
            src = 'img/icons/goal.png',
            width = '14px',
            height = '14px'
          )
        )
      ),

      div(class = 'left_chart',

          highchartOutput(ns('chart_1')),
          highchartOutput(ns('chart_n'))),

      h6(
        'TOP-10 Teams: 2020 SEASON Ranking',
        tags$img(
          src = 'img/ranking.png',
          width = '14px',
          height = '14px'
        )
      ),

      div(class = 'right_chart',

          div(
            class = 'comments',
            tags$ul(
              tags$li(
                tags$span('About Data:', style = 'font-weight:700'),
                'In publishing and graphic design, Lorem ipsum is a placeholder text
          commonly used to demonstrate the visual form of a document or a typeface
          without relying on meaningful content. Lorem ipsum may be used as a
          placeholder before final copy is available.'
              ))
          ))
    ),

    div(
      class = 'tabpanels_row',

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      div(
        class = 'comments',

        h5(
          'GOALS, EXPECTED GOALS (xG), EXPECTED GOALS AGAINST (xGA), Aerials Duels Won(%)',
          tags$span('- vs. Statisticals Moments')
        ),
        h6('Variables are standardized')
      ),

    do.call(tabsetPanel,
    # MAximum 7 Metrics
      1:length(tabpanel_championship_metrics) %>% map( ~
      tabPanel(
        title = tags$span(
        class = 'subtitle_main',
          tags$img(
          class = 'svg_icon',
          src = 'img/icons/goal.png',
          width = '14px',
          height = '14px'
          ),
          h6(tabpanel_championship_metrics[.x],class = 'percent')
        ),

          highchartOutput(ns(paste0('chart_', .x+1)))

    ))),

      div(
        class = 'footer',
        tags$ul(
          tags$li(
            tags$span('About Data:', style = 'font-weight:700'),
            'In publishing and graphic design, Lorem ipsum is a placeholder text
          commonly used to demonstrate the visual form of a document or a typeface
          without relying on meaningful content. Lorem ipsum may be used as a
          placeholder before final copy is available.'
          )
        )
      )
    ),

    div(
      class = 'metrics_table',

      h5(
        'Others Metrics',
        tags$span('- vs. Statisticals Moments')
      ),
      h6('Variables are standardized')
    ),

    div(class = 'others_metrics_table',
      reactableOutput(ns('table_1')),
      csvDownloadButton(ns("table_1"),
                        filename = "league_table.csv")
    )

  ))
}

module_league_Server <- function(id, app_control_input) {
  moduleServer(id,
               function(input,
                        output,
                        session) {

#****************************************************************************
                 dataframe<-reactive({
                   if (is.null(input$datafile))
                     return(NULL)
                   data<-read.csv(input$datafile$datapath)

                   data
                 })
#****************************************************************************

                 output$excel <- renderUI({
                   if(is.null(data_csv_file)){

                   }else{

                   tagList(
                     h1(
                       'Champion -',
                       style = 'font-size: 5em;',
                       tags$span(
                         paste(champion_team),
                         style = glue::glue(
                           "color: {data_csv_file %>% filter(Rk == 1) %>% select(Cores) %>% pull};
                           font-weight: 900;"
                         )
                       ),

                       tags$img(
                         src = paste0(
                           'img/2020/',
                           paste0(champion_team),
                           '.png'
                         ),
                         width = '148px',
                         height = '148px'
                       )
                     ),

                     div(

                       class = 'champion_stats',
                       1:(length(metrics_introduction)-1) %>% map(~
                       h3(main_league_df %>% filter(Rk == 1) %>%
                            select(metrics_introduction[.x]) %>% pull ,
                          tags$br(),
                          tags$span(definition[.x])
                       )),

                      h3(main_league_df %>% filter(Rk == 1) %>%
                           select(metrics_introduction[18]) %>% pull ,
                         class = 'top_scorer',
                          tags$img(src ='img/salah.jpeg',
                                   width = '88px', height = '88px',
                                   style  ='float: right;border-radius:50%;margin: 0 auto;'),
                          tags$br(),
                          tags$span(definition[18])
                       )),
                     tags$ul(
                       tags$li(
                         tags$span('About Data:', style = 'font-weight:700'),
                         'In publishing and graphic design, Lorem ipsum is a placeholder text
          commonly used to demonstrate the visual form of a document or a typeface
          without relying on meaningful content. Lorem ipsum may be used as a
          placeholder before final copy is available.'
                       )
                     )
                   )


                   }
                 })
#***********************************************************************************
                 output$chart_1 <- renderHighchart({
                   if(is.null(data_csv_file)){

                   }else{

                     stacked_bar_chart_function(df = main_league_df,
                                                x_axis = 'Squad',
                                                main_color =team_color_champion,
                                                y_axis = standard_metrics_championship_bar_1,
                                                y2_axis = standard_metrics_championship_bar_2)
                   }
                 })
#***********************************************************************************
                 output$chart_n <- renderHighchart({

                   if(is.null(data_csv_file)){

                   }else{

                     scatter_chart_function(df = main_league_df,
                                            metric = paste0(standard_metrics_championship_bar_1,'_std'),
                                            main_color = team_color_champion,
                                            x_axis_title = paste0(standard_metrics_championship_bar_2,'_std'))

                   }
                 })
#***********************************************************************************
                 output_chart_function <- function(x,vars,titles){

                   output[[paste0('chart_',x)]] <- renderHighchart({

                     if(is.null(data_csv_file)){

                     }else{

                       scatter_chart_function(df = main_league_df,
                                              metric = vars,
                                              main_color = team_color_champion,
                                              x_axis_title = titles)
                     }
                   })
                 }

                  map2(2:(length(tabpanel_championship_metrics)+1),tabpanel_championship_metrics,
                       ~ output_chart_function(.x,
                                               vars = .y,
                                               titles = str_replace(tabpanel_championship_titles[.x-1],
                                                                    '_',' ')))
#***********************************************************************************
                  output$table_1 <- renderReactable({

                    if(is.null(data_csv_file)){

                    }else{
                      table_style(dataframe = main_league_df %>%
                                    select(all_of(table_championship_vector))%>%
                                    arrange(Rk),width = 'auto')
                    }
                  })

               })
}

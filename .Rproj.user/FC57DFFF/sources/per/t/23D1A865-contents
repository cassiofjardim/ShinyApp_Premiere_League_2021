

module_top_right_UI <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      class = 'top_right',
      style = "grid-area: top_right;",
      h5("Others Stats"),


     div(
       class = 'panels_controls',
      pickerInput(
        inputId = ns("categories"),
        width = '200px',
        label = "Metrics Categories",
        choices = c("Standard League - Stats",
                    "Passing League - Stats",
                    "Possession League - Stats",
                    "Misc - Stats")
      ),

      checkboxGroupButtons(
        inputId = ns("metrics"),
        label = "Choices (Max. 4 Choices):",
        width = '100%',
        choices = standard_metrics,
        selected = "",
        status = "success"
      )),


     uiOutput(ns('tabset_panel'))
    )
  )
}

module_top_right_Server <- function(id,app_control_input) {
  moduleServer(
    id,
    function(input, output, session) {



      observeEvent(input$categories,
                   {
                     if(input$categories == 'Standard League - Stats'){
                       updateCheckboxGroupButtons(inputId = 'metrics',
                                                  choices = paste0(standard_metrics,'_std'),
                                                  selected = paste0(standard_metrics,'_std')[1],
                                                  status = "success")
                     }else{
                       if(input$categories == 'Passing League - Stats'){
                         updateCheckboxGroupButtons(inputId = 'metrics',
                                                    choices = paste0(passing_metrics,'_std'),
                                                    selected = paste0(passing_metrics,'_std')[1],
                                                    status = "success")
                       }else{
                         if(input$categories == 'Possession League - Stats'){
                           updateCheckboxGroupButtons(inputId = 'metrics',
                                                      choices = paste0(possession_metrics,'_std'),
                                                      selected = paste0(possession_metrics,'_std')[1],
                                                      status = "success")
                         }else{

                           updateCheckboxGroupButtons(inputId = 'metrics',
                                                      choices = paste0(misc_metrics,'_std'),selected = "",
                                                      selected = paste0(misc_metrics,'_std')[1],
                                                      status = "success")
                         }
                       }

                     }

                   })

      output$tabset_panel <- renderUI({
        mainPanel(width = 12,id = 'tabset_panel',

                  do.call(tabsetPanel,
                          if(length(input$metrics) == 0){
                            list()

                          }else{
                            input$metrics %>%
                              map(~ tabPanel(class = 'tabpanel_choices',
                                             title = paste0(.x),
                                             div(
                                               scatter_chart_function(
                                                 df = main_league_df,
                                                 metric = .x,
                                                 x_axis_title = NULL,
                                                 main_color = main_league_df %>% filter(Rk == 1) %>% select(Cores) %>% pull
                                               ) %>%
                                                 hc_size(height = 400)
                                             )

                                             ))
                          }

                  )


        )}) %>%
                bindCache(input$metrics, cache = "app")

    }
  )
}

library(shiny)


#
source('R/charts/charts_functions.R')
source('R/cards_function/cards_function.R')

source('R/main.R')
source('R/overview.R')
source('R/top_right.R')
source('R/bottom_right.R')


# https://public.tableau.com/app/profile/pradeepkumar.g/viz/HRAttritionDashboardRWFD_16570446563570/viz
ui <- fluidPage(

  div(class = 'main_title_div',
    h1(class = 'main_title',
      'Premiere League 2021 - Dashboard'),
    # selectInput(inputId = 'season',
    #             label = 'Season',
    #             choices = 2012:2022
    #
    #
    # ),

    selectInput(inputId = 'custom_select',
                label = 'Clubs',
                choices = str_to_upper(elements_names))),

  # https://stackoverflow.com/questions/44159168/how-to-style-an-single-individual-selectinput-menu-in-r-shiny
   gridPanel(title = '',id = 'main_grid_panel',

            # style = 'border:5px solid red;',
            breakpoint_system = getBreakpointSystem(),
            includeCSS(path = 'www/css/style.css'),
            areas =   list(
              default = c(
                "overview  bottom_right_two",
                " overview bottom_right_one",
                " overview top_right"
              ),
              md = c(
                "overview ",
                "top_right ",
                "bottom_right_one",
                "bottom_right_two"
              )
              # xs = c(
              #   "header",
              #   "area-1",
              #   "area-2",
              #   "area-3",
              #   "area-4",
              #   "area-5",
              #   "area-6",
              #   "footer"
              # )
            ),
            columns = '750px auto',

            gap = "2.5em",
            overview_UI('overview'),
            bottom_right_UI('bottom_right'),
            top_right_UI('top_right')




  )
)

server <- function(input, output, session) {

#
   options_reactive <- reactive({
     input$custom_select
   }) %>% bindCache(input$custom_select)

   overview_Server('overview', app_control_input = options_reactive)
   bottom_right_Server('bottom_right', app_control_input = options_reactive)
   top_right_Server('top_right', app_control_input = options_reactive)


}

shinyApp(ui, server)

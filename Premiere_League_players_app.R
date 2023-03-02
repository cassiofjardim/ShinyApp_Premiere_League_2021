library(shiny)
library(shinyWidgets)
library(imola)
library(tidyverse)
library(highcharter)
library(reactable)



source(file = 'R/bar_chart_function.R')
source(file = 'R/scatter_chart_function.R')
source(file = 'R/pie_chart_function.R')
source(file = 'R/table_function.R')

source(file = 'R/modules/module_league_level.R')
source(file = 'R/modules/bottom_right.R')
source(file = 'R/modules/top_right.R')



ui <- fluidPage(

  # tags$meta(`http-equiv`="refresh" ,content="0"),


  div(class = 'main_title_div',
      h1(class = 'main_title',
         'Premiere League 2020 - Dashboard')),

  # https://stackoverflow.com/questions/44159168/how-to-style-an-single-individual-selectinput-menu-in-r-shiny
  gridPanel(title = '',
            id = 'main_grid_panel',

            # style = 'border:5px solid red;',
            breakpoint_system = getBreakpointSystem(),
            includeCSS(path = 'www/css/league_level_style.css'),
            areas =   list(
              default = c(
                "league_level  bottom_right_two",
                " league_level top_right",
                " league_level ."

                # " league_level ."
              ),
              md = c("league_level",
                     "bottom_right_two",
                     "top_right"

              )),

            columns = list(

              default = c('1fr','1fr'),
              md = '1fr'),

            gap = "2.5em",
            #
            module_league_UI('league_level'),
            module_bottom_right_UI('bottom_right'),
            module_top_right_UI('top_right')
  )

)

server <- function(input, output, session) {

  module_league_Server('league_level')
  module_bottom_right_Server('bottom_right')
  module_top_right_Server('top_right')




}

shinyApp(ui, server)




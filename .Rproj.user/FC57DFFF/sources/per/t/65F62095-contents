library(shiny)
library(tidyverse)
library(shinyWidgets)
library(highcharter)
library(reactable)

source('R/metrics_to_choose.R')

source(file = 'R/util/bar_chart_function.R')
source(file = 'R/util/scatter_chart_function.R')
source(file = 'R/util/pie_chart_function.R')
source(file = 'R/util/table_function.R')

source(file = 'R/modules/champion_league_level_module.R')


ui <- fluidPage(
  div(
    class = 'main_title_div',
    h1(class = 'main_title',
       'Premiere League 2020 - Dashboard')
  ),

  includeCSS(path = 'www/css/champion_league_style.css'),

  module_league_UI('champion_league_level')
)

server <- function(input, output, session) {
  module_league_Server('champion_league_level')
}

shinyApp(ui, server)

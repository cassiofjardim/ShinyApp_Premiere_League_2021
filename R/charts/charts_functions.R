################################################################################
############### BAR CHART ######################################################
################################################################################


players_chart <- function(data, type,color){

  data %>%


    hchart(
      type = paste0(type),

      hcaes(x = year, y = pop),
      visible = TRUE,
      showInLegend = TRUE,
      name  = "",
      zIndex = 9999999,
      zoneAxis = 'x',
      color = color,


      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      marginRight = 0,
      marginLeft = 0,
      marginTop = 0,
      marginBottom = 0,
      borderWidth = 0

    ) %>%

    hc_credits(enabled = FALSE) %>%
    hc_title(text = "") %>%
    hc_subtitle(text = "") %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(enabled = FALSE,title = FALSE,
             gridLineColor = 'lightgray',
             lineColor = 'lightgray') %>%
    hc_yAxis(enabled = FALSE,title = FALSE,
             labels = list(enabled = FALSE),

             gridLineColor = 'lightgray',
             lineColor = 'lightgray') %>%

    # hc_plotOptions(
    #   series = list(
    #
    #     lineWidth = 3,
    #     marker = list(
    #       enabled = FALSE
    #     ),
    #     borderRadius = 1
    #
    #   )) %>%
    hc_size( height = 250) %>%
    hc_boost(enabled = FALSE)


}

# yellow_red_function(type = 'column', color = c('#484848','#FFB67A'))



yellow_red_function <- function(data, type,color, stacking_type, show_in_legend = FALSE){

  data %>%


    hchart(
      type = paste0(type),

      hcaes(x = year, y = gdpPercap, group = country),
      visible = TRUE,
      showInLegend = TRUE,
      name  = c('Goals', 'Assistances'),
      zIndex = 9999999,
      zoneAxis = 'x',
      color = color,
      groupPadding = c(.5,.5),
      pointWidth = c(20,10),
      # spacingTop = 0,
      # spacingRight = 0,
      # spacingBottom = 0,
      # spacingLeft = 0,
      plotBorderWidth = 0,
      marginRight = 0,
      marginLeft = 0,
      marginTop = 0,
      marginBottom = 0,
      borderWidth = 0

    ) %>%

    hc_credits(enabled = FALSE) %>%
    hc_title(text = "") %>%
    hc_subtitle(text = "") %>%
    hc_legend(enabled = show_in_legend) %>%
    hc_xAxis(enabled = FALSE,title = FALSE) %>%
    hc_yAxis(enabled = FALSE,title = FALSE,
             labels = list(enabled = FALSE)) %>%

    # hc_plotOptions(
    #   column = list(stacking = stacking_type,pointWidth = 10)
    #   # series = list(
    #   #
    #   #   lineWidth = 3,
    #   #   marker = list(
    #   #     enabled = FALSE
    #   #   ),
    #   #   borderRadius = 1
    #   #
    #   # )
    #   )%>%
    hc_size( height = 250) %>%
    hc_boost(enabled = FALSE)




}

################################################################################
############################# GENERAL OVERVIEW #################################
################################################################################

# general_overview_charts <- function(data, var, year) {
#   highchart(type = 'chart') %>%
#     hc_add_series(data = data,
#                   showInLegend = FALSE,
#                   hcaes(x = year, y = .data[[var]], group = country),
#                   type = 'column') %>%
#     hc_title(text = paste0(unique(data$year)),
#              margin = 20,
#              align = "center",
#              style = list(color = "black",fontWeight = 'bold', useHTML = TRUE)) %>%
#     hc_size(height = 200, width = 250) %>% hc_boost(enabled = FALSE)
# }

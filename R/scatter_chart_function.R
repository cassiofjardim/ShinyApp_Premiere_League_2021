

scatter_chart_function <- function(df,metric, x_axis_title = NULL,
                                   main_color = NULL){
  df %>%

hchart(
  'scatter',
  names = 'SQUADS',
  hcaes(y = id,
        x = !!metric
  ),
  color = main_color
  # colorByPoint = TRUE
) %>%

  hc_yAxis(

    type = 'category',
    categories = df$Squad,
    tickAmount = 20,
    gridLineWidth = 0.5,
    gridLineColor = 'gray',
    gridLineDashStyle = "longdash",


    title = list(text = '<b>Squads</b>')) %>%


  hc_xAxis(
    title = list(text = glue::glue('<b>{x_axis_title}</b>')),

    plotLines = list(

      list(
        color = '#484848',
        width = 2,
        dashStyle = "shortdash",
        label = list(text = 'Mean',
                     verticalAlign = 'bottom',
                     # y = 120,
                     # x = -20,
                     align = 'right'),

        value = mean(df[[metric]])),

      list(
        color = '#C8102E',
        width = 2,
        dashStyle = "longdash",
        label = list(text = 'Median',
                     verticalAlign = 'bottom',
                     align = 'right'),

        value = median(df[[metric]])),
      list(
        color = '#484848',
        width = 2,
        dashStyle = "longdash",
        label = list(text = 'Std. Deviation',
                     verticalAlign = 'bottom',

                     align = 'right'),

        value = sd(df[[metric]])),

      list(
        color = '#C8102E',
        width = 2,
        dashStyle = "longdash",
        label = list(text = '2*Std. Deviation',
                     verticalAlign = 'bottom',
                     align = 'right'),

        value = 2*sd(df[[metric]]))

      )
    ) %>%

  hc_credits(enabled = TRUE) %>%
  hc_tooltip(
    pointFormat = paste0("<b>{point.Squad}<br>",x_axis_title,": {point.x}","</b>"),
    shared = TRUE,
    useHTML = TRUE,
    headerFormat= '{point.Squad}'

  ) %>%

  hc_plotOptions(
    scatter = list(

      dataLabels= list(
        enabled = TRUE,
        pointFormat = "<b style = 'font-size: 1em;'>{point.Squad}</b>",

        verticalAlign =
          'top'
      ))
  )
}

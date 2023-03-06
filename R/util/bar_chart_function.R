


stacked_bar_chart_function <- function(df,x_axis,y_axis,y2_axis,main_color){
  highchart(type = 'chart') %>%
    hc_add_series(data = df,

                  hcaes(x = !!x_axis, y = !!y_axis),
                  # groupPadding = .5,
                  # pointWidth = 10,

                  color = main_color ,
                  name = y_axis,
                  type = 'column')%>%
    hc_add_series(data = df,

                  hcaes(x = !!x_axis, y = !!y2_axis),
                  # groupPadding = .5,
                  # pointWidth = 10,
                  color  = 'black',
                  name = y2_axis,
                  type = 'column')%>%
    hc_xAxis(
      categories = df$Squad
    )%>%
    hc_plotOptions(series = list(borderRadius = 2.5))%>%

    hc_credits(enabled = FALSE) %>%
    hc_title(text = "") %>%
    hc_subtitle(text = "") %>%
    hc_credits(text = "") %>%

    hc_legend(enabled = TRUE) %>%
    hc_yAxis(enabled = FALSE,title = FALSE,
             labels = list(enabled = FALSE)) %>%
    hc_size( height = 250) %>%
    hc_boost(enabled = FALSE)
}

bar_chart_function <- function(df,x_axis,y_axis,y2_axis, main_color,
                               height = 250, width = 'auto'){
  df %>%

  hchart(
    'column',
    names = 'SQUADS',
    hcaes(x = !!x_axis,
          y = !!y_axis),
    color = main_color,
    name = 'Goals'
    # groupPadding = 0.5,
    # pointWidth = 17
    ) %>%

    hc_add_series(data = df,
                  showInLegend = TRUE,
                  hcaes(x = !!x_axis, y = !!y2_axis),
                  name = 'Ast.',
                  color = '#484848' ,
                  # groupPadding = 0.5, pointWidth = 8.5,

                  type = 'column')%>%

    hc_plotOptions(series = list(borderRadius = 2.5))%>%


    hc_title(text = "") %>%
    hc_subtitle(text = "") %>%
    hc_legend(enabled = TRUE) %>%
    # hc_xAxis(enabled = FALSE,title = FALSE, categories = standard[[!!metric]]$Opponent) %>%
    hc_yAxis(enabled = TRUE,
             title = 'Goals and Assists.',
             labels = list(enabled = TRUE),
             tickAmount = 10,
             gridLineWidth = 0.5,
             gridLineColor = 'gray',
             gridLineDashStyle = "shortdash") %>%

    hc_size( height = height, width = width) %>%
    hc_boost(enabled = TRUE)%>%
    hc_credits(enabled = TRUE)

}

bar_chart <- function(df,x_axis,y_axis,main_color, input = NULL){
  df %>%
  hchart(
    'column',
    names = 'SQUADS',
    hcaes(x = !!x_axis,
          y = !!y_axis),
    color = main_color,
    name = 'Short Passes',
    groupPadding = 0.5,
    pointWidth = 17) %>%

    hc_title(text = "") %>%
    hc_subtitle(text = "") %>%
    hc_legend(enabled = TRUE) %>%
    # hc_xAxis(enabled = FALSE,title = FALSE, categories = standard[[!!metric]]$Opponent) %>%
    hc_yAxis(enabled = TRUE,
             title = 'Goals and Assists.',
             labels = list(enabled = TRUE),
             tickAmount = 10,
             gridLineWidth = 0.5,
             gridLineColor = 'gray',
             gridLineDashStyle = "shortdash",
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

                 value = mean(df[[y_axis]])),

               list(
                 color = 'red',
                 width = 2,
                 dashStyle = "longdash",
                 label = list(text = 'Median',
                              verticalAlign = 'top',

                              align = 'right'),

                 value = median(df[[y_axis]])))) %>%


    hc_boost(enabled = TRUE)%>%
    hc_credits(enabled = TRUE) %>%

    hc_plotOptions(
      series = list(borderRadius = 2.5),
      column = list(

        dataLabels= list(
          enabled = TRUE,
          pointFormat = "<b style = 'font-size: 1em;'>{point.y}</b>",

          verticalAlign =
            'top'
        )))%>%
    hc_tooltip(
      pointFormat = paste0("<b>{point.Squad}: {point.y_axis}","</b>"),
      shared = TRUE,
      useHTML = TRUE,
      headerFormat= glue::glue('{input}<br>')

    )

}



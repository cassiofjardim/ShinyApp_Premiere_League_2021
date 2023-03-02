pie_chart_function <- function(df,x_axis,y_axis,title_slice, slices_colors_list){

  df  %>%

    hchart(
      'pie',

      names = 'SQUADS',
      hcaes(x = !!x_axis,
            y = !!y_axis),
      colors = slices_colors_list,
      colorByPoint = TRUE)%>%
    hc_plotOptions(
      pie = list(
        allowPointSelect = TRUE,
        cursor = 'pointer',
        dataLabels= list(
          enable = TRUE,
          # color = standard$cores,
          style = list(fontWeight = 'italic')

          # formatter = function() {
          #   const point = this.point;
          #   return '<span style="color: ' + point.color + '">' +
          #     point.name + ': ' + point.y + '%</span>';
          # }


        )
      )) %>%
    hc_tooltip(
      pointFormat = paste0("<b>{point.Squad}: {point.y}","</b>"),
      shared = TRUE,
      useHTML = TRUE,
      headerFormat= glue::glue('{title_slice}<br>')

    )

    # hc_colors(colors = df$cores)


}




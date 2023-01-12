# https://twitter.com/henrikbengtsson/status/1583520708470067201

library(shiny)
# library(sortable)
library(pdftools)
library(magick)

thumbs <- magick::image_read_pdf('www/img/Thumbnail_Reports_page/myfile.pdf', density = 200, pages = 1:3) |>
  image_resize("x250")
#
pages_pdf <- 3
#
1:pages_pdf %>% purrr::map(~ image_write(thumbs[.x],path = paste0('www/img/Thumbnail_Reports_page/','pagina_',.x,'.png'),format = '.png'))
#


ui <- fluidPage(
  tags$style(

    "
    .report:hover{
    box-shadow: 10px 10px 5px 0px rgb(0 0 0 / 75%);
    }
    "
  ),
  fluidRow(
    column(
      width = 12,
      h1("Thumbnail - extracting from first pdf file page to jpg"),
      div(style = 'display: flex;gap: 2em;',
          div(style = 'display: flex; flex-direction: column;',
              tags$img(src = 'img/Thumbnail_Reports_page/pagina_1.png', width = '100px', height = '100px'),
              tags$a(href = 'img/Thumbnail_Reports_page/pagina_1.png', 'Download PDF', target = '_blank')),
          div(style = 'display: flex; flex-direction: column;',
              tags$img(src = 'img/Thumbnail_Reports_page/pagina_2.png', width = '100px', height = '100px'),
              tags$a(href = 'img/Thumbnail_Reports_page/pagina_2.png', 'Download PDF', target = '_blank')),
          div(style = 'display: flex; flex-direction: column;',
              tags$img(src = 'img/Thumbnail_Reports_page/pagina_3.png', width = '100px', height = '100px'),
              tags$a(href = 'img/Thumbnail_Reports_page/pagina_3.png', 'Download PDF', target = '_blank'))


          ),
      h1("Thumbnail - from jpg"),
      # tags$h2("PDF page reorder"),
      # tags$b("Exercise"),
      # rank_list_basic,
      # tags$b("Order"),
      # verbatimTextOutput("results_basic"),
      # tags$a(href = 'thumbs/output.pdf', 'Download PDF', target = '_blank'),
      # imageOutput(outputId = 'thumb'),
    div(style = 'display: flex;gap: 2em;',
      div(class = 'report',style = 'display: flex; flex-direction: column;',tags$img(src = 'img/Thumbnail_Reports_page/example_1_1.jpg', width = '100px', height = '100px'),
      tags$a(href = 'img/Thumbnail_Reports_page/example_1_1.jpg', 'Download PDF', target = '_blank')),
      div(class = 'report',style = 'display: flex; flex-direction: column;',tags$img(src = 'img/Thumbnail_Reports_page/example_1_2.jpg', width = '100px', height = '100px'),
      tags$a(href = 'img/Thumbnail_Reports_page/example_1_2.jpg', 'Download PDF', target = '_blank')),
      div(class = 'report',style = 'display: flex; flex-direction: column;',tags$img(src = 'img/Thumbnail_Reports_page/example_1_3.jpg', width = '100px', height = '100px'),
      tags$a(href = 'img/Thumbnail_Reports_page/example_1_3.jpg', 'Download PDF', target = '_blank')),
      div(class = 'report',style = 'display: flex; flex-direction: column;',tags$img(src = 'img/Thumbnail_Reports_page/example_1_4.jpg', width = '100px', height = '100px'),
      tags$a(href = 'img/Thumbnail_Reports_page/example_1_4.jpg', 'Download PDF', target = '_blank')),
      div(class = 'report',style = 'display: flex; flex-direction: column;',tags$img(src = 'img/Thumbnail_Reports_page/example_1_5.jpg', width = '100px', height = '100px'),
      tags$a(href = 'img/Thumbnail_Reports_page/example_1_5.jpg', 'Download PDF', target = '_blank')),
      div(class = 'report',style = 'display: flex; flex-direction: column;',tags$img(src = 'img/Thumbnail_Reports_page/example_1_6.jpg', width = '100px', height = '100px'),
      tags$a(href = 'img/Thumbnail_Reports_page/example_1_6.jpg', 'Download PDF', target = '_blank'))
)



    )
  )
)

server <- function(input, output, session) {
  # output$results_basic <- renderPrint({
  #   pages <- as.numeric(input$rank_list_basic)
  #   if(!length(pages)){
  #     pages <- seq_along(thumbs)
  #   }
  #   qpdf::pdf_subset(pdffile, pages = pages, output = file.path('output.pdf'))
  #   pages
  # })

}

shinyApp(ui, server)

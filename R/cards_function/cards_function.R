# data <- readxl::read_xlsx(path = 'www/data/cards_players.xlsx') %>% mutate(Date = as.Date(Date))
# data <-

cards_players_function <- function(vector,data,player_team){


  vector %>% map(~
                   div(class = 'cards_scroll', style = 'height: fit-content;',
                       div(class = 'div_result',
                             div(
                               h4('Competition', class = 'competitions_logo'),
                               tags$img(src = 'img/premiere.svg', width  ='60px', height = '60px', style = 'border-radius: 50%;')),
                           div(
                             class = 'top_right_card_center',
                             style  = "line-height: .5;font-size: .8em;padding: 0.75em 0;font-weight: 700; ",

                             tags$p(paste0('Date: ', data[.x, ]$Date)),
                             tags$p(paste0('Captain: ', data[.x, ]$Captain)),
                             tags$p(paste0('Formation: ', data[.x,]$Formation),
                             tags$p(paste0('Expected Goal (xG): ', round(data[.x,]$xG,2)),
                             tags$p(paste0('Ball Possession: ', data[.x,]$Poss),'%'),
                             tags$p(paste0('Opponent: ', data[.x,]$Opponent))
                             )
                             )


                           ),

                           div(class = 'top_right_card_right',


                               p(tags$img(src = paste0('img/Clubs',player_team,'.png'), width = '45px', height = '45px', style = '    border-radius: 50%;'),
                                 tags$span(data[.x,]$GF, style = 'font-size: 1.5em;'),
                                 tags$span('  x  '),
                                 tags$span(data[.x,]$GA, style = 'font-size: 1.5em;'),
                                 tags$img(src = paste0('img/Clubs',data[.x,]$Opponent,'.png'), width = '45px', height = '45px'), style = 'font-size: 1.25em;margin: 0px;'
                                 )

                           )



                       ))

  )


}

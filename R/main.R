library(tidyverse)
library(ggplot2)
library(imola)
library(highcharter)
library(reactable)

library(shinyWidgets)
library(worldfootballR)
library(stringr)
library(stringi)

 # First Chart

 league_url <- fb_league_urls(country = "ENG", gender = "M",
                              season_end_year = 2021, tier = "1st")
 team_urls <- fb_teams_urls(league_url)

#************************* Clubs Summary *********
overview_card <- fb_season_team_stats(country = 'ENG',gender = "M",season_end_year = '2021',tier = '1st',stat_type = 'standard')



choices <- overview_card %>% filter(Team_or_Opponent == 'team') %>% pull(Squad)
#************************* Clubs Stats *********

# standard_stats <- vector(mode = 'list')
#
# for(i in 1:length(choices)){
#
#   standard_stats[[i]] <- fb_team_player_stats(team_urls[i],
#                                         stat_type = "standard")%>%
#     select(Season, Squad, Player,Nation,Age,Gls, Ast, G_minus_PK,
#            PK,xG_Expected,Min_Playing_Time,xG_Expected,
#            PlayerURL)
#
# }



#
# standard_stats_df <- standard_stats %>% bind_rows() %>% arrange(Squad) %>% group_split(Squad)
# write.csv(x = bind_rows(standard_stats), file = 'www/data/standard_stats.csv')

standard_stats_df <- read.csv(file = 'www/data/standard_stats.csv',sep = ',')

elements_names <- standard_stats_df %>% bind_rows() %>%  group_split(Squad) %>% map(~ .x %>% select(Squad) %>% pull() %>% unique()) %>% unlist() %>% unique()

standard_stats <- standard_stats_df %>% group_split(Squad)%>%  set_names(str_to_upper(elements_names))

#
chart_1 <- standard_stats %>% map(
  ~ .x %>%
    mutate(index = as.numeric(Gls) + as.numeric(Ast)) %>%
    arrange(desc(index)) %>%
    top_n(10) %>% select(Season:Ast)
)



#
chart_2 <- standard_stats %>% map(~ .x %>%
     mutate(index = as.numeric(Gls) + as.numeric(Ast)) %>%
     arrange(desc(index)) %>%
     top_n(10))

# table_overview <- vector(mode = 'list')
# for(i in 1:length(choices)){
#
#   table_overview[[i]] <- fb_team_player_stats(team_urls[i],
#                                               stat_type = "standard")%>%
#  select(Squad,Player, Nation,CrdY, CrdR, Min_Playing_Time)
#
# }


# write.csv(x = bind_rows(table_overview), file = 'www/data/table_overview_df.csv')

table_overview <- read.csv(file = 'www/data/table_overview_df.csv',sep = ',')

table_overview <- table_overview %>% bind_rows()

# elements_names <- table_overview %>% select(Squad:last_col()) %>% pull(Squad) %>% unique()

table_overview_df <-table_overview %>% group_split(Squad)%>% map(~ .x %>% select(Squad,Player, Nation,CrdY, CrdR, Min_Playing_Time) %>%
                                                                   top_n(10))%>%  set_names(str_to_upper(elements_names))


# #
# last_five_games <- vector(mode = 'list')
#
# for(i in 1:length(choices)){
#
# last_five_games[[i]] <-
# fb_team_match_results(team_urls[i]) %>%
#   filter(Comp == 'Premier League')%>%
#     select(Venue, Team, Date, Opponent,GF,GA, Date,Captain, Formation, xG,Poss) %>% head(10)
#
#  }


# write.csv(x = last_five_games, file = 'www/data/last_five_games.csv')

last_five_games <- read.csv(file = 'www/data/last_five_games.csv',sep = ',')

last_five_games <- last_five_games %>% bind_rows()
# elements_names <- last_five_games %>% select(Team:last_col()) %>% pull(Team) %>% unique()


last_five_games_df <- last_five_games %>% group_split(Team)%>% set_names(str_to_upper(sort(elements_names)))

#
# shooting_list <- vector('list')
# for(i in 1:length(choices)){
#
#   shooting_list[[i]] <- worldfootballR::fb_team_match_log_stats(team_url = team_urls[i], stat_type = 'shooting' )
#
# }

# write.csv(x = shooting_sot, file = 'www/data/shooting_sot.csv')

shooting_sot <- read.csv(file = 'www/data/shooting_sot.csv',sep = ',')

shooting_sot <- shooting_sot %>% filter(Comp %in% 'Premier League',ForAgainst %in% 'For') %>%
  select(Comp,Team,Round,Opponent, Sh_Standard,SoT_Standard)%>%
  mutate(Round = as.double(str_remove_all(Round,'Matchweek ')))%>%
  arrange(desc(Round)) %>% group_split(Team)%>% set_names(str_to_upper(sort(elements_names)))

shooting_sot<-shooting_sot %>% map(~ .x[1:5,])

#
#
# passing_list <- vector('list')
# for(i in 1:length(choices)){
#
#   passing_list[[i]] <-  worldfootballR::fb_team_match_log_stats(team_url = team_urls[i], stat_type = 'passing' )
#
# }
#
#
#
# write.csv(bind_rows(passing_list),file = 'www/data/passing_list_df.csv')

passing_list_df <- read.csv(file = 'www/data/passing_list_df.csv',sep = ',')



passing <- passing_list_df %>% bind_rows()%>% filter(Comp %in% 'Premier League',ForAgainst %in% 'For')


passing_df <- passing %>% select(Team,Opponent, Att_Short) %>% group_split(Team)%>% set_names(str_to_upper(sort(elements_names)))

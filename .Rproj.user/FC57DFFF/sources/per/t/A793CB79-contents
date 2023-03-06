library(tidyverse)
library(scales)
colors <- alpha(c("#6CADDF", "#C8102E", "#034694", "#132257", "#EF0107",
                  "#E03A3E", "#7A263A", "#0053A0", "#0057B8", "#FDB913",
                  "#241F20", "#1B458F", "#F5A3C7", "#95BFE5", "#D71920",
                  "#274488", "#FFCD00", "#6C1D45", "#FBEE23", "#00A650"),.5)

main_league_df <- read.csv(file = 'www/database_sql/main_league_df.csv') %>%
  select(-X.1,-X)

data_csv_file <- read.csv(file = 'www/database_sql/main_league_df.csv')

metrics_and_definitions_df <- tibble(metrics = main_league_df %>% select(Rk:last_col()) %>% names())

squad_metric <- 'Squad'
champion_team <- main_league_df %>% filter(Rk == 1) %>% select(Squad) %>% pull
team_color_champion <- main_league_df %>%filter(Rk == 1) %>% select(Cores) %>% pull
player_top_scorer <- main_league_df %>% filter(Rk == 1) %>% select(Top.Team.Scorer) %>% pull

#*******************************************************************************
#**********************Treemap League Level Dashboard **************************
#*******************************************************************************
standard_metrics <- c("xG" , "xGA", "xGD" , "xGD.90")
passing_metrics <- c("xAG","xA","A_minus_xAG","KP")
possession_metrics <- c("Touches_Touches","Def.Pen_Touches","Def.3rd_Touches",
                        "Mid.3rd_Touches")
misc_metrics <- c("Mis_Carries", "Dis_Carries","Rec_Receiving","PrgR_Receiving")

#*******************************************************************************
#********************* League Level Championship Dashboard *********************
#*#*****************************************************************************


metrics_introduction <- c('MP','Pts', 'W', 'L', 'Age','Gls', 'xG', 'xGA', 'Poss', 'Ast', 'xA',
             'KP','Cmp_percent_Total','Cmp_percent_Short', 'Cmp_percent_Medium',
             'Cmp_percent_Long', 'PPA',   'Top.Team.Scorer')

definition <- c('Games','Pts', 'Win', 'Lost', 'Average Age','Goals', 'Exp. Goals(xG)',
                'Exp. Goals Against(xGA)','Ball Poss.', 'Assists', 'Exp. Assist.(xA)',
                'Key Passings','Completed Passes (%)','Short Passings (%)',
                'Medium Passings (%)', 'Long Passings (%)', 'Passings - Pen. Area',
                'Top Team Scorer')


#************
#*** Stacked Bar Chart
#************
standard_metrics_championship_bar_1 <- 'xG'
standard_metrics_championship_bar_2 <- 'xGA'




#************
#*** Scatter Plot
#************
# scatter_plot_1 <- paste0(standard_metrics_league_level_bar_1,'_std')
# scatter_plot_2 <- paste0(standard_metrics_league_level_bar_1,'_std')

#************
#*** TabPanels Metrics
#************
tabpanel_championship_metrics <- c('xG_std','xGA_std', 'xGD.90_std', 'xG_Home_std', 'xG_Away_std',
                   'xG_Per_Minutes_std', 'Won_percent_Aerial_Duels')

tabpanel_championship_titles <- c('xG - Standardized', 'xGA - Standardized', 'xGD.90 - Standardized',
                     'xG_Home - Standardized', 'xG_Away - Standardized', 'xG Per. Minutes - Standardized',
                     'Aerials Duels Won (%)')

table_championship_vector <- main_league_df %>% select(Squad:Attendance,-Pts.MP,-Team_or_Opponent) %>% names()
#*******************************************************************************
#********************* League Level Complete - Dashoboard  *********************
#*#*****************************************************************************

#***********************************
#***** league_level module
#***********************************

metrics_introduction_top <- c('MP','Pts', 'Gls', 'xG', 'xGA', 'Poss', 'Ast',
                          'KP','PPA',   'Top.Team.Scorer')

table_displayed <- main_league_df %>%  select(Squad:Attendance)%>%  arrange(Rk)

#***********************************
#***** top_right module
#***********************************
#* radio metrics options

tabpanels_top_module <- c("Standard League - Stats",
"Passing League - Stats", "Possession League - Stats","Misc - Stats")

#* metrics options
standard_metrics_top <- c("xG" , "xGA", "xGD" , "xGD.90")
passing_metrics_top <- c("xAG","xA","A_minus_xAG","KP","Final_Third","PPA", "CrsPA",  "PrgP")
possession_metrics_top <- c("Touches_Touches","Def.Pen_Touches","Def.3rd_Touches",
                        "Mid.3rd_Touches","Att.3rd_Touches","Att.Pen_Touches","Live_Touches")
misc_metrics_top <- c("Mis_Carries", "Dis_Carries","Rec_Receiving","PrgR_Receiving")


#***********************************
#***** bottom_right module
#***********************************


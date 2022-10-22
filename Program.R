library(tidyverse)
library(nflfastR)
library(rvest)
library(stringr)
library(yardstick)
library(gt)
library(paletteer)
library(gtExtras)
`%nin%` = Negate(`%in%`)

# Get Player Stats --------------------------------------------------------

# load Regular season's pbp data 2000 to 2020
future::plan("multisession")
player_stats_reg <- load_pbp(2000:2020) %>% filter(season_type == "REG")

# fix aaron rodgers' name from 2021 data
player_stats_reg <- player_stats_reg %>% 
  mutate(passer_player_name2 = ifelse(passer_player_name == 'Aa.Rodgers', 'A.Rodgers', passer_player_name),
         rusher_player_name2 = ifelse(rusher_player_name == 'Aa.Rodgers', 'A.Rodgers', rusher_player_name))

# aggregate QB passing stats
qb_pass_stats <- player_stats_reg %>% 
  group_by(season, passer_player_name2, posteam) %>% 
  summarise(epa_play = mean(epa, na.rm = T), 
            tot_completions = sum(complete_pass, na.rm = T),
            tot_attempts = sum(pass_attempt, na.rm = T),
            tot_passing_yards = sum(passing_yards, na.rm = T),
            tot_passing_tds = sum(pass_touchdown, na.rm = T),
            tot_ints = sum(interception, na.rm = T)) %>% 
  ungroup() %>% 
  filter(tot_attempts >= 150)

# aggregate QB rushing stats
qb_rush_stats <- player_stats_reg %>% 
  mutate(passer_player_name2 = rusher_player_name2) %>% 
  group_by(season, passer_player_name2, posteam) %>% 
  summarise(tot_rushing_yards = sum(rushing_yards, na.rm = T),
            tot_rushing_tds = sum(rush_touchdown, na.rm = T))

# join pass and rush stats on season, player_name, and posteam
# calculate total yards, tds, and rank of each player in a given season
qb_overall_stats <- qb_pass_stats %>% 
  left_join(qb_rush_stats, by = c("season", "passer_player_name2", "posteam")) %>% 
  mutate(tot_yards = tot_passing_yards + tot_rushing_yards,
         tot_tds = tot_rushing_tds + tot_passing_tds) %>% 
  
  # calculate ranks
  group_by(season) %>% 
  mutate(tot_yard_rank = rank(desc(tot_yards), ties.method = 'average'),
         tot_td_rank = rank(desc(tot_tds), ties.method = 'average'),
         epa_rank = rank(desc(epa_play), ties.method = 'average')) %>% 
  rename(passer_player_name = passer_player_name2)

# league standings
standings <- readr::read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/standings.csv") %>% 
  mutate(team = case_when(
    team == 'OAK' ~ 'LV',
    team == 'SD' ~ 'LAC',
    team == 'STL' ~ 'LA',
    TRUE ~ team))

# pull team wins from standings to qb_overall_stats
# filter for data from 2003 to 2020
qb_overall_stats_merged <- qb_overall_stats %>% 
  filter(season >= 2003) %>% 
  left_join(standings %>% select(season, team, wins), by = c('posteam'= 'team', 'season')) %>% 
  group_by(season) %>% 
  mutate(win_rank = rank(desc(wins), ties.method = 'average'))

# get mvp table
link = "https://en.wikipedia.org/wiki/Associated_Press_NFL_Most_Valuable_Player_Award"
content <- read_html(link)
tables <- html_table(content)
mvps <- tables[[3]]

# put names in the same form as nflfastR
name.formatter <- function(name) {
  paste0(
    substring(  strsplit(name, " ")[[1]][1], 1, 1), 
    ".", 
    strsplit(name, " ")[[1]][2])
}

mvps <- mvps %>% mutate(Player = sapply(Player, name.formatter))
mvps[mvps$Season == 1963,]$Player = "Y.Tittle" # special case
mvps[mvps$Season == 1973,]$Player = "O.Simpson" # special case
mvps <- mvps %>% filter(Season != 2003)
mvps <- bind_rows(mvps, 
                  tibble(Season=2003,Player="P.Manning",Position="Quarterback",Team="Indianapolis Colts",Ref=""),
                  tibble(Season=2003,Player="S.McNair",Position="Quarterback",Team="Tennessee Titans",Ref="")) %>%
  arrange(Season)

mvps_final <- mvps %>% mutate(MVP = 1) %>% 
  mutate(passer_player_name = Player, season = Season) %>% 
  select(passer_player_name, Position, MVP, season)

# pull MVP variable (response) to qb_overall_stats_merged
qb_final_stats <- qb_overall_stats_merged %>% 
  ungroup() %>% 
  left_join(mvps_final %>% select(-Position), by = c("passer_player_name", "season")) %>% 
  replace_na(list(MVP = 0)) %>% 
  arrange(season, passer_player_name)

# sanity checks--------------------------------

# Any missing values across each row for season, passer_player_name, and posteam?
qb_final_stats %>% 
  rowwise() %>% 
  mutate(num_missing = sum(is.na(c_across(epa_play:last_col())))) %>% 
  select(season:posteam, num_missing) %>% 
  arrange(desc(num_missing))

# Does the MVP col besides 0 and 1?
table(qb_final_stats$MVP)

# create the training set
df_train <- qb_final_stats %>% 
  filter(season >= 2003 & season <= 2016 & season %nin% c(2005, 2006, 2012))

# create the test set
df_test <- qb_final_stats %>% filter(season >= 2017)

# double check training and test sets
table(df_train$season)
table(df_test$season)

# Logistic Regression -----------------------------------------------------
m <- glm(data = df_train, family = "binomial", 
         MVP ~ tot_td_rank + tot_yard_rank + win_rank + epa_rank + tot_ints)

# coefficient review
summary(m)

# making predictions on training data
options(scipen = 99)

df_train <- df_train %>% 
  mutate(pred = predict(m, df_train, type = "response")) %>% 
  group_by(season) %>%
  mutate(pred_mvp = as.numeric(pred == max(pred, na.rm=TRUE))) %>%
  mutate(mvp_prob = pred / sum(pred)) %>% # normalize the probabilities
  ungroup()

# training data's confusion matrix
# training accuracy is 98%
confusion_train <- conf_mat(table(df_train$pred_mvp, df_train$MVP))
summary(confusion_train, event_level = 'second')

# which seasons' predictions were incorrect?
df_train %>% 
  filter((MVP == 1 & pred_mvp == 0) | (MVP == 0 & pred_mvp == 1)) %>% 
  mutate(outcome = case_when(MVP == 1 & pred_mvp == 0 ~ "Actual MVP", TRUE ~ "Predicted MVP")) %>% 
  select(season, passer_player_name, outcome) %>% 
  pivot_wider(names_from = outcome, values_from = passer_player_name)

# mutate predictions on testing data
df_test <- df_test %>% 
  mutate(pred = predict(m, df_test, type = "response")) %>% 
  group_by(season) %>%
  mutate(pred_mvp = as.numeric(pred == max(pred, na.rm=TRUE))) %>%
  mutate(mvp_prob = pred / sum(pred)) %>% # normalize the probabilities
  ungroup()

# testing data's confusion matrix
# model correctly predicts every MVP from 2017-2020
confusion_test <- conf_mat(table(df_test$pred_mvp, df_test$MVP))
summary(confusion_test, event_level = 'second')

# Example tables
cols_needed <- c("passer_player_name", "mvp_prob", "pred_mvp", #"MVP", 
                 "tot_td_rank", "tot_yard_rank", "win_rank", "epa_rank", "tot_ints")

get_gt_table <- function(df, year, num_rows = 5) {
  
  df %>% 
    filter(season == year) %>% 
    arrange(season, -mvp_prob) %>% 
    filter(row_number() <= num_rows) %>% 
    select(all_of(cols_needed)) %>% 
    gt() %>% 
    
    # add a title
    tab_header(title = paste0(year, " MVP Race")) %>%
    
    # change column labels
    cols_label(
      passer_player_name = "Name",
      mvp_prob = md("MVP<br>Prob"),
      pred_mvp = md("Predicted<br>MVP"),
      #MVP = md("Actual<br>MVP"),
      tot_td_rank = md("TD<br>Rank"),
      tot_yard_rank = md("YDS<br>Rank"),
      win_rank = md("Wins<br>Rank"),
      epa_rank = md("EPA<br>Rank"),
      tot_ints = md("Total<br>INTs")
    ) %>% 
    fmt_percent(columns = mvp_prob, decimals = 1) %>% 
    
    # add data color to change
    data_color(
      columns = mvp_prob,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>% 
    
    cols_align(align = "center", columns = 3:8) %>% 
    
    # finishing touches
    tab_source_note(source_note = md("**Source**: nflfastR | **Note:** Code heavily borrowed from Ryan Brill and Ryan Weisman ")) %>% 
    gtExtras::gt_theme_538() %>% 
    tab_options(
      table.font.names = "Consolas", 
      data_row.padding = px(.5)
    )
}

# TABLES for example seasons
get_gt_table(df_test, 2021) %>% gtsave("MVP_table.png")
get_gt_table(df_test, 2020)
get_gt_table(df_train, 2013)
get_gt_table(df_train, 2003)
get_gt_table(df_train, 2009)
get_gt_table(df_train, 2015)







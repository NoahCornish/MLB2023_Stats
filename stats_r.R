library(tidyverse)
library(janitor)
library(lubridate)
library(RJSONIO)
library(jsonlite)
library(dplyr)
library(scales)
library(baseballr)

if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("BillPetti/baseballr")

date <- Sys.Date()
x <- Sys.Date()


#### MLB SCHEDULE ####
league_schedule <- mlb_schedule(season = 2023, level_ids = "1")

today_schedule <- league_schedule %>% 
  filter(date == x) %>% 
  select(date, day_night, status_detailed_state,
         teams_away_team_name, teams_away_score,
         teams_home_team_name, teams_home_score)

#### Blue Jays Schedule ####
BlueJays_Home_Schedule <- league_schedule %>% 
  filter(teams_home_team_name == "Toronto Blue Jays") %>% 
  filter(game_type == "R")%>% 
  select(date, game_date, day_night, status_detailed_state, teams_home_team_name,
         teams_home_score, teams_away_team_name,teams_away_score)


#### Position Player Statistics ####
Pos_Player_Stats <- bref_daily_batter("2023-03-30", x) 
Pos_Player_Stats[is.na(Pos_Player_Stats)] <- 0

Pos_Player_Stats <- Pos_Player_Stats %>% 
  mutate(REQ = PA/G) %>% 
  filter(REQ >= 3.1) %>% 
  select(Name, Level, Team, G, PA, AB, BA, OBP, SLG, OPS,
         H, HR, RBI, BB, `X2B`, `X3B`)

AL_Pos_Stats <- Pos_Player_Stats %>% 
  filter(Level == "Maj-AL")

NL_Pos_Stats <- Pos_Player_Stats %>% 
  filter(Level == "Maj-NL")

write.csv(Pos_Player_Stats,
          file = "MLB_2023_Position_Players_Stats.csv",
          row.names = F)

#### Pitcher Player Stats ####
Pitcher_Stats <- bref_daily_pitcher("2023-03-30", x)
Pitcher_Stats[is.na(Pitcher_Stats)] <- 0




AL_Pitch_Stats <- Pitcher_Stats %>% 
  filter(Level == "Maj-AL")

NL_Pitch_Stats <- Pitcher_Stats %>% 
  filter(Level == "Maj-NL")



#### BLUE JAYS STATS ####

BlueJays_Batters <- AL_Pos_Stats %>% 
  filter(Team == "Toronto") %>% 
  select(Name, Age, G, PA, AB, BB, SO, R, H, BA,
         X2B, X3B, HR, RBI, OBP, SLG, OPS,) %>% 
  filter(PA > 0) %>% 
  mutate(`SO%` = (SO/PA)*100) %>% 
  mutate(`Req` = PA/G)
  BlueJays_Batters$`SO%` <- round(BlueJays_Batters$`SO%`, digits = 1)
  BlueJays_Batters$`Req` <- round(BlueJays_Batters$`Req`, digits = 1)

write.csv(BlueJays_Batters,
          file = "bluejays_2023_position_players.csv",
          row.names = F)

BlueJays_Pitchers <- AL_Pitch_Stats %>% 
  filter(Team == "Toronto")

#### Dodgers Stats ####

Dodgers_Batters <- NL_Pos_Stats %>% 
  filter(Team == "Los Angeles") %>% 
  select(Name, Age, G, PA, AB, BB, SO, R, H, BA,
         X2B, X3B, HR, RBI, OBP, SLG, OPS,)

Dodgers_Pitchers <- NL_Pitch_Stats %>% 
  filter(Team == "Los Angeles")


#### STANDINGS ####

AL_Standings <- bref_standings_on_date(date, "AL Overall", from = FALSE)
NL_Standings <- bref_standings_on_date(date, "NL Overall", from = FALSE)



#### Individual Player Game per Game Statistics ####

vladdy <-  fg_batter_game_logs(playerid = 19611, year = 2023) %>% 
  select(PlayerName, Date, Opp, BatOrder, Pos,
         AB, PA, H, AVG, `1B`, `2B`, `3B`, HR, R, RBI, BB, SO,
         HBP, SF, GDP, GB, FB, LD)

bobo <-  fg_batter_game_logs(playerid = 19612, year = 2023) %>% 
  select(PlayerName, Date, Opp, BatOrder, Pos,
         AB, PA, H, AVG, `1B`, `2B`, `3B`, HR, R, RBI, BB, SO,
         HBP, SF, GDP, GB, FB, LD)

matt_chapman <- fg_batter_game_logs(playerid = 16505, year = 2023) %>% 
  select(PlayerName, Date, Opp, BatOrder, Pos,
         AB, PA, H, AVG, `1B`, `2B`, `3B`, HR, R, RBI, BB, SO,
         HBP, SF, GDP, GB, FB, LD)


# for these I need to download the CSV file of all the players
# playerid numbers and join them to the initial download so I can
# easily select individual players vs. having to always look
# them up online





i <- (mlb_teams(season = 2023, sport_ids = c(1))) %>% 
  select(team_id, team_full_name)



# Plot Blue Jays Pitcher Statisitcs using GGPLOT2


ggplot(BlueJays_Pitchers, aes(x=Name, y=ERA)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = ERA), hjust = +2.0) +
  coord_flip()







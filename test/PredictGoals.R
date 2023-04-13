library(worldcup)
library(tidyverse)

# Quick EDA review of utilised dataset
# Dataset provided via worldcup database installed via:
# devtools::install_github("jfjelstul/worldcup")

glimpse(squads)
glimpse(player_appearances)
glimpse(manager_appointments)
glimpse(manager_appearances)
glimpse(goals)

# Store team composition of each match using player_id, team_id, and match_id

df <- player_appearances %>%
  # Only filter for the starting eleven
  filter(starter == 1) %>%
  select('team_id','match_id', 'player_id','position_code') %>%
  # Dumb down positions into only FW, MF, and DF otherwise too many positions...
  # Based on https://yoursoccerhome.com/soccer-positions-a-complete-and-easy-to-understand-guide/
  mutate(position_code2 = case_when(
    position_code == "AM" ~ "MF", # Attacking Midfield
    position_code == "CB" ~ "DF", # Center Back
    position_code == "CF" ~ "FW", # Center Forward
    position_code == "CM" ~ "MF", # Center Midfielder
    position_code == "DM" ~ "MF", # Defending Midfielder
    position_code == "LB" ~ "DF", # Left Fullback
    position_code == "LF" ~ "FW", # Left Forward?
    position_code == "LM" ~ "MF", # Left Midfield
    position_code == "LW" ~ "MF", # Left Wing
    position_code == "LWB" ~ "DF", # Left Wingback
    position_code == "RB" ~ "DF", # Right Fullback
    position_code == "RF" ~ "FW", # Right Forward
    position_code == "RM" ~ "MF", # Right Midfield
    position_code == "RW" ~ "MF", # Right Wing
    position_code == "RWB" ~ "DF", # Right Wingback
    position_code == "SS" ~ "FW", # Second Striker
    position_code == "SW" ~ "DF", # Sweeper
    .default = position_code
  )) %>%
  # Drop position code
  select(-position_code) %>%
  # select(-c(position_code, position_code2)) %>%
  group_by(team_id, match_id, position_code2) %>%
  summarise("players" = toString(player_id), .groups = "keep") %>%
  spread(position_code2, players) %>%
  # Create unique team key from team and match id
  # unite("key", c("team_id",'match_id'), sep="-", remove=FALSE) %>%
  ungroup() %>%
  left_join(
  manager_appearances %>%
    select(match_id, team_id, manager_id, home_team, away_team),
  by = c('team_id','match_id')
  )


# Find out how many goals were scored by each team for each match
n_goals <- goals %>% 
  select('team_id','match_id') %>%
  group_by(team_id, match_id) %>%
  count() %>%
  rename(goals = n)

# Join n_goals with df to find how many goals are scored for each team composition
df %>%
  left_join(n_goals, by = c('team_id','match_id')) %>%
  mutate_at("goals", ~replace_na(.,0))
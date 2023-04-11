# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("ggthemes")
# devtools::install_github("jfjelstul/worldcup")

library(worldcup)
library(tidyverse)
library(ggthemes)

# Explore the fourth group of worldcup dataset including:

# goals
# penalty_kicks
# bookings
# substitutions

# glimpse overview
glimpse(goals)
glimpse(penalty_kicks)
glimpse(bookings)
glimpse(substitutions)

# Home advantage?
goals %>%
  count(home_team) %>%
  mutate(across(home_team, as.character)) %>%
  mutate(perc = n/sum(n)) %>%
  mutate(labels = scales::percent(perc)) %>%
  
  ggplot(aes(x="",y=n,fill=home_team)) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  geom_text(aes(label=labels),
            position=position_stack(vjust=0.5)) +
  labs(
    title = "Number of goals scored by home vs visitor teams",
    subtitle = "Data shows home team scored more goals than visitors"
  )

# Teams that scored the most goals
goals %>%
  group_by(team_code) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n=10) %>%

  ggplot(aes(x=reorder(team_code, -count),y=count,fill=team_code)) +
  geom_bar(stat="identity") +
  labs(
    title = "Top 10 teams with highest number of goals"
  ) +
  xlab("Team") +
  ylab("No. of goals across the years")

# Earlier but on world map and not limited to 10 teams
world_map <- map_data("world")

countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column() %>%
  rename(team_name = region)

goals %>%
  group_by(team_name) %>%
  summarise(count = n()) %>%
  right_join(countries, by = "team_name") %>%
  
  ggplot(aes(fill=count, map_id=team_name)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  theme_map() +
  labs(
    title = "Number of goals scored by country",
    subtitle = "South America and Europe are the highest scorers"
  )

# @Marzuk - use block of player_goals for the bubble plot
# Players with most goals
player_goals <- goals %>% 
  group_by(player_id) %>%
  count() %>%
  arrange(desc(n)) %>%
  rename(goals = n) %>%
  left_join(players %>%
              select(player_id, family_name, given_name, goal_keeper, defender, midfielder, forward)
            , by = "player_id")

summary(player_goals)

player_goals %>%
  filter(goal_keeper == 1)

# Managers with most goals
goals %>%
  select(match_id) %>%
  left_join(manager_appearances %>%
              select(match_id, manager_id)
              , by = "match_id") %>%
  group_by(manager_id) %>%
  count() %>%
  arrange(desc(n)) %>%
  left_join(
    managers %>%
      select(manager_id, family_name, given_name, country_name),
    by = "manager_id"
  ) %>%
  head(n=20)

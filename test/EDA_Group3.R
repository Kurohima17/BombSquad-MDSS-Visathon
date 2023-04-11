# install.packages("devtools")
# install.packages("tidyverse")
# devtools::install_github("jfjelstul/worldcup")

library(worldcup)
library(tidyverse)

# Explore the third group of worldcup dataset including:

# team_appearances
# player_appearances
# manager_appearances
# referee_appearances

# squads - players who start a game on the bench but not substituted in
# will appear here but not in player_appearances

# glimpse overview
glimpse(team_appearances)
glimpse(player_appearances)
glimpse(manager_appearances)
glimpse(referee_appearances)
glimpse(squads)

head(player_appearances)
head(squads)

# Exploring fully benched players
benched <- anti_join(squads, player_appearances, by="player_id")
dim(benched)
head(benched)

# Top 10 teams that leave players on the bench the most
# Remove the slice and this plot could be used on Marzuk's world map
benched %>%
  group_by(team_code) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n=10) %>%
  ggplot(aes(x=reorder(team_code, -count),y=count, fill=team_code)) +
  geom_bar(stat="identity") +
  labs(
    title = "Top 10 teams that leave the most number of players on benches"
  ) +
  xlab("Team") +
  ylab("No. of players benched across the years")

# Visualising composition of the starting 11 of each team
# make interactive by allowing filter for tournament_id and/or team_id

starting_eleven <- player_appearances %>%
  filter(starter == 1) %>%
  group_by(team_id, team_code, match_id, position_code) %>%
  count() %>%
  unite("team_match" ,team_code:match_id)

starting_eleven %>%
  ggplot(aes(x=team_match, y=n, fill=position_code)) +
  geom_bar(position="stack", stat="identity") +
  coord_polar(start = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Visualisation of the starting-eleven for every FIFA match since 1970"
  )


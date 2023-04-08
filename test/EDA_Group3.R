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



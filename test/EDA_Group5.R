# install.packages("devtools")
# install.packages("tidyverse")
# devtools::install_github("jfjelstul/worldcup")

library(worldcup)
library(tidyverse)

# Explore the fifth group of worldcup dataset including:

# host_countries
# tournament_stages
# groups
# group_standings
# tournament_standings
# award_winners

# squads - players who start a game on the bench but not substituted in
# will appear here but not in player_appearances

# glimpse overview
glimpse(host_countries)
glimpse(tournament_stages)
glimpse(groups)
glimpse(group_standings)
glimpse(tournament_standings)
glimpse(award_winners)

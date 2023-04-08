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

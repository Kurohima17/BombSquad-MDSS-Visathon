# install.packages("devtools")
# install.packages("tidyverse")
# devtools::install_github("jfjelstul/worldcup")

library(worldcup)
library(tidyverse)

# Explore the fourth group of worldcup dataset including:

# goals
# penalty_kicks
# bookings
# substitutions

# squads - players who start a game on the bench but not substituted in
# will appear here but not in player_appearances

# glimpse overview
glimpse(goals)
glimpse(penalty_kicks)
glimpse(bookings)
glimpse(substitutions)

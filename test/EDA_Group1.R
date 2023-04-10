# install.packages("devtools")
# install.packages("tidyverse")
# devtools::install_github("jfjelstul/worldcup")

library(worldcup)
library(tidyverse)

# Explore the first group of worldcup dataset including:

# tournaments
# confederations
# teams
# players 
# managers
# referees
# stadium
# matches
# awards

teams %>%
  filter(team_code == "ARG")

# glimpse overview
glimpse(tournaments)
glimpse(confederations)
glimpse(teams)
glimpse(players)
glimpse(managers)
glimpse(referees)
glimpse(stadiums)
glimpse(worldcup::matches)
glimpse(awards)

head(players)
head(awards)

dim(players)

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

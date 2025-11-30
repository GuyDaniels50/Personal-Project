library(tidyverse)
library(readxl)
library(performance)

team_averages <- read_excel("NZNBL Team averages.xlsx")

team_averages <- team_averages %>%
  mutate(win_margin = PPG - `Ave Pts Agt`)

team_averages |> 
  ggplot(aes(x = Team, y = win_margin)) + 
  geom_col() +
  geom_text(aes(label = round(win_margin, 1)), vjust = -0.5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
    )

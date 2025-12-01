library(tidyverse)
library(readxl)
library(performance)

team_averages <- read_excel("NZNBL Team averages.xlsx")

team_averages <- team_averages %>%
  mutate(win_margin = PPG - `Ave Pts Agt`)

team_averages |> 
  ggplot(aes(x = Team, y = win_margin)) + 
  geom_col(
    aes(fill = win_margin >= 0)
  ) +
  geom_text(
    aes(
    label = round(win_margin, 1),
    vjust = ifelse(win_margin >= 0, -0.5, 1.2))
    ) +
  scale_fill_manual(
    values = c("FALSE" = "red", "TRUE" = "darkgreen"),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(
    min(team_averages$win_margin) * 1.2,
    max(team_averages$win_margin) * 1.2
  )) +
  labs(
    title = "Win Margin by Team",
    x = "Team",
    y = "Win Margin"
  ) +
  theme(
  axis.text.x = element_text(angle = 45, hjust = 1)
  )

margin_proj1 <- lm(win_margin ~ 
                     RPG + APG + STPG + BLKPG + TOPG, data = team_averages)
summary(margin_proj1)

margin_proj2 <- lm(win_margin ~ `FG%` + `FT%`, data = team_averages)
summary(margin_proj2)

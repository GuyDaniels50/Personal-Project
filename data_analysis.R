library(tidyverse)
library(readxl)
library(performance)

team_averages <- read_excel("NZNBL Team averages.xlsx")

team_averages <- team_averages %>%
  mutate(win_margin = PPG - `Ave Pts Agt`,
         `3PTA` = `3PMPG` / `3P%` * 100)

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

team_averages |>
  ggplot(aes(x = `FG%`, y = win_margin)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

fg_wm <- lm(win_margin ~ `FG%`, data = team_averages)
summary(fg_wm)


team_averages |>
  ggplot(aes(x = `3PTA`, y = win_margin)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

a3_wm <- lm(win_margin ~ `3PTA`, data = team_averages)
summary(a3_wm)


team_averages |>
  ggplot(aes(x = FTAPG, y = win_margin)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

fta_wm <- lm(win_margin ~ FTAPG, data = team_averages)
summary(fta_wm)


margin_proj1 <- lm(win_margin ~ 
                     RPG + APG + STPG + BLKPG + TOPG, data = team_averages)
summary(margin_proj1)

margin_proj2 <- lm(win_margin ~ `FG%` + `FT%`, data = team_averages)
summary(margin_proj2)

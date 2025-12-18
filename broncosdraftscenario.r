library(tidyverse)
library(gt)
library(scales)

# read annotated shortlist
young_qb <- read_csv(
  "annotated_qb_table_2025.csv",
  show_col_types = FALSE
)

gt_young_qb <- young_qb %>%
  mutate(Rank = row_number()) %>%
  select(
    Rank,
    Player = player_name,
    Team = offense_team,
    Plays = total_plays,
    `YAC Creator Score` = YAC_creator_score,
    `Reliable Target Score` = reliable_target_score
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Young QB Targets — 2025 Season**"),
    subtitle = md("Receivers Who Consistently Perform Above Average in YAC and Reliability (min plays 150)")
  ) %>%
  fmt_number(
    columns = c(`YAC Creator Score`, `Reliable Target Score`),
    decimals = 3
  ) %>%
  fmt_number(
    columns = Plays,
    decimals = 0
  ) %>%
  data_color(
    columns = c(`YAC Creator Score`, `Reliable Target Score`),
    colors = col_numeric(
      palette = c("#DEEBF7", "#db990bff"),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = c(
      Rank,
      Plays,
      `YAC Creator Score`,
      `Reliable Target Score`
    )
  ) %>%
  cols_align(
    align = "left",
    columns = c(Player, Team)
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(6)
  )

gt_young_qb


gtsave(
  gt_young_qb,
  "young_qb_annotated_leaderboard.png",
  vwidth = 900,
  vheight = 450
)


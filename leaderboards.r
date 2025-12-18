library(tidyverse)
library(gt)
library(scales)

SCORES_CSV <- "wr_arch_scores.csv"

# Read Data
wr <- read_csv(SCORES_CSV, show_col_types = FALSE)

# Minimum Threshold
MIN_PLAYS <- 500
wr_filt <- wr %>%
  filter(total_plays >= MIN_PLAYS)

# Leaderboard Fuction
make_leaderboard <- function(df, score_col, score_label, title_txt, palette) {

  if (!("player_name" %in% colnames(df))) {
    stop("Column 'player_name' not found in the dataset.")
  }
  if (!("total_plays" %in% colnames(df))) {
    stop("Column 'total_plays' not found in the dataset.")
  }
  if (!(score_col %in% colnames(df))) {
    stop(paste0("Score column '", score_col, "' not found in the dataset."))
  }

  df %>%
    filter(!is.na(.data[[score_col]])) %>%
    arrange(desc(.data[[score_col]])) %>%
    slice_head(n = 10) %>%
    mutate(Rank = row_number()) %>%
    transmute(
      Rank,
      Player = player_name,
      Plays = total_plays,
      !!score_label := .data[[score_col]]
    ) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title_txt, "**"))
    ) %>%
    fmt_number(
      columns = Plays,
      decimals = 0
    ) %>%
    fmt_number(
      columns = all_of(score_label),
      decimals = 3
    ) %>%
    data_color(
      columns = all_of(score_label),
      colors = col_numeric(palette = palette, domain = NULL)
    ) %>%
    cols_align(
      align = "center",
      columns = c(Rank, Plays, all_of(score_label))
    ) %>%
    cols_align(
      align = "left",
      columns = Player
    ) %>%
    tab_options(
      table.font.size = px(14),
      heading.title.font.size = px(22),
      data_row.padding = px(6)
    )
}

# Build Tables
gt_vertical <- make_leaderboard(
  df          = wr_filt,
  score_col   = "vertical_stress_score",
  score_label = "Vertical Stress Score",
  title_txt   = paste0("Top 10 WRs — Vertical Stress (min ", MIN_PLAYS, " plays)"),
  palette     = c("#FEE0D2", "#b802a8ff")
)

gt_sep <- make_leaderboard(
  df          = wr_filt,
  score_col   = "separation_skill_score",
  score_label = "Separation Skill Score",
  title_txt   = paste0("Top 10 WRs — Separation Skill (min ", MIN_PLAYS, " plays)"),
  palette     = c("#DEEBF7", "#08306B")
)

gt_yac <- make_leaderboard(
  df          = wr_filt,
  score_col   = "YAC_creator_score",
  score_label = "YAC Creation Score",
  title_txt   = paste0("Top 10 WRs — YAC Creation (min ", MIN_PLAYS, " plays)"),
  palette     = c("#E5F5E0", "#006D2C")
)

gt_reliable <- make_leaderboard(
  df          = wr_filt,
  score_col   = "reliable_target_score",
  score_label = "Reliable Target Score",
  title_txt   = paste0("Top 10 WRs — Reliable Target (min ", MIN_PLAYS, " plays)"),
  palette     = c("#FFF7BC", "#D95F0E")
)

# Save
gtsave(gt_vertical, "vertical_stress_leaderboard.png")
gtsave(gt_sep,      "separation_skill_leaderboard.png")
gtsave(gt_yac,      "yac_creation_leaderboard.png")
gtsave(gt_reliable, "reliable_target_leaderboard.png")





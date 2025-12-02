##########################################
# players_code.R
# Goal-Scoring Careers – 4 Forwards
# Messi, Ronaldo, Neymar, Lewandowski
#
# How to run in RStudio:
# 1. Open this file.
# 2. Go to: Session -> Set Working Directory -> To Source File Location
# 3. Then click "Source" (or run the lines step by step).
##########################################

library(tidyverse)
library(lubridate)

#---------------------------------
# Helper function for one player
#---------------------------------
process_player <- function(file_path, player_name) {
  raw <- read_csv(file_path, show_col_types = FALSE)
  
  df_yearly <- raw %>%
    mutate(
      Date_parsed = dmy(Date),   # change to ymd(Date) if your Date is like "2009-05-01"
      Year        = year(Date_parsed)
    ) %>%
    group_by(Year) %>%
    summarise(
      goals = n(),
      .groups = "drop"
    ) %>%
    arrange(Year) %>%
    mutate(
      Player      = player_name,
      t           = Year - min(Year),     # years since first recorded goal
      delta_goals = goals - lag(goals)   # year-to-year change
    )
  
  return(df_yearly)
}

#---------------------------------
# Read & aggregate all 4 players
# (CSV files must be in the same folder
#  as this script OR in the working dir)
#---------------------------------

messi_yearly <- process_player("messi.csv.xlsx",        "Lionel Messi")
ron_yearly   <- process_player("ronaldo.csv.xlsx",      "Cristiano Ronaldo")
ney_yearly   <- process_player("neymar.csv.xlsx",       "Neymar")
lew_yearly   <- process_player("lewandowski.csv.xlsx",  "Robert Lewandowski")

players_yearly <- bind_rows(messi_yearly, ron_yearly, ney_yearly, lew_yearly)

# Save table for Excel / Tableau
write_csv(players_yearly, "players_goals_per_year.csv")

#---------------------------------
# Simple line plot (optional)
#---------------------------------
ggplot(players_yearly, aes(x = Year, y = goals, color = Player)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Goals per Year by Player",
    x = "Year",
    y = "Number of Goals"
  ) +
  theme_minimal()

#---------------------------------
# Quadratic model & derivative
#---------------------------------
players_modelled <- players_yearly %>%
  group_by(Player) %>%
  group_modify(~ {
    m <- lm(goals ~ t + I(t^2), data = .x)   # goals = a t^2 + b t + c
    coefs <- coef(m)
    a <- coefs[["I(t^2)"]]
    b <- coefs[["t"]]
    
    .x %>%
      mutate(
        fitted_goals = predict(m, newdata = .x),
        slope        = 2 * a * t + b        # derivative
      )
  }) %>%
  ungroup()

# Peak year (max fitted goals) for each player
peak_years <- players_modelled %>%
  group_by(Player) %>%
  slice_max(fitted_goals, n = 1, with_ties = FALSE) %>%
  mutate(
    Peak_Year  = Year,
    Peak_Goals = round(fitted_goals, 1)
  ) %>%
  select(Player, Peak_Year, Peak_Goals)

peak_years


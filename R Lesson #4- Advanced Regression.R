library(tidyverse)  
library(nflreadr)
library(ggimage)
library(gt)
library(gtExtras)

future::plan("multisession")
pbp <- load_pbp(2020:2024)


# Logistic Regression
## Using regression techniques to predict an outcome (yes/no)

# Quarter/time, Wind Speed, Turf-Type(?), Game Type

fg_pbp <- pbp |> filter(!is.na(field_goal_result) & field_goal_result != "blocked") |>  # we are assessing for kicker quality
  select(posteam, kicker_player_name, kicker_player_id, season, season_type, posteam_type, yardline_100, qtr, 
         half_seconds_remaining, kick_distance, score_differential, field_goal_result, wind, surface) |> 
  mutate(field_goal_outcome = ifelse(field_goal_result == "made", 1, 0),
         wind_factor = ifelse(wind < 5 | is.na(wind), 0, 1),
         wind = ifelse(is.na(wind), 0, wind),
         take_lead_or_tie = ifelse(score_differential >= -3 & score_differential <= 0, 1, 0))


# Make a binomial Regression

# First try
field_goal_model <- glm(field_goal_outcome ~ kick_distance + half_seconds_remaining*take_lead_or_tie +
                        + season_type + wind + posteam_type, data = fg_pbp, family = binomial)

summary(field_goal_model) # We can see a lot of these variables aren't significant or even close to being so

# Best try (you may have to play around a little)
field_goal_model <- glm(field_goal_outcome ~ kick_distance + half_seconds_remaining +
                          + season_type + wind + posteam_type, data = fg_pbp, family = binomial)

summary(field_goal_model)


# Now add the predictions to the data
fg_pbp_final <- fg_pbp |> mutate(fg_predict = predict(field_goal_model, type = "response"), # Type response turn from log odds to probs
                                 fg_oe = field_goal_outcome - fg_predict)  


# Now that we have a fg_oe metric, we can analyze kicker performance

kicker_performance <- fg_pbp_final |> group_by(kicker_player_name, season, posteam) |> 
  select(posteam, season, kicker_player_name, kicker_player_id, fg_oe) |> 
  mutate(
  fgoe = sum(fg_oe)
  ) |> 
  select(-fg_oe) |> 
  ungroup() |> 
  unique()


### We want to show tables of the best kicker season and the worst kicker season:
rosters <- load_rosters(2020:2024) |> 
  select(full_name, headshot_url, gsis_id, season)

kicker_performance_roster <- kicker_performance |> 
  right_join(rosters, by = c("kicker_player_id" = "gsis_id", "season")) |> 
  unique() |> 
  drop_na() |>  
  right_join(teams_colors_logos |> select(team_abbr, team_logo_espn), by = c("posteam" = "team_abbr")) |> 
  select(full_name, headshot_url, season, team_logo_espn, fgoe)

# Top 10 kicker performances
kicker_performance_roster |> 
  mutate(fgoe = round(fgoe, 2)) |> 
  arrange(desc(fgoe)) |> 
  slice_head(n = 10) |> 
  gt() |> 
  gt_img_rows(headshot_url) |> 
  gt_img_rows(team_logo_espn) |> 
  cols_label(
    team_logo_espn = "Team",
    season = "Season",
    full_name = "",
    headshot_url = "",
    fgoe = "FGOE"
  ) |> 
  cols_align(align = "center") |> 
  gt_theme_espn() |> 
  tab_header(
    title = "Top 10 Seasons in Field Goals Over Expected 2020-2024"
  )
  
# Bottom 10 kicker performances
kicker_performance_roster |> 
  mutate(fgoe = round(fgoe, 2)) |> 
  arrange((fgoe)) |> 
  slice_head(n = 10) |> 
  gt() |> 
  gt_img_rows(headshot_url) |> 
  gt_img_rows(team_logo_espn) |> 
  cols_label(
    team_logo_espn = "Team",
    season = "Season",
    full_name = "",
    headshot_url = "",
    fgoe = "FGOE"
  ) |> 
  cols_align(align = "center") |> 
  gt_theme_espn() |> 
  tab_header(
    title = "Bottom 10 Seasons in Field Goals Over Expected 2020-2024"
  )



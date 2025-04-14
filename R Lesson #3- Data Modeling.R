library(tidyverse) 
library(nflreadr) 
library(ggimage)
library(gt)
library(gtExtras)


pbp_ptdiff_comb <- read.csv("pbp_ptdiff_comb.csv") # Lets load in the CSV from last time

schedules <- load_schedules(2023) |> filter(game_type == "REG") # Load in schedules // filter only regular season

# We will learn how to create a function for this:
records_home <- function(schedules) {
  schedules <- schedules |> 
  group_by(home_team) |> 
  mutate(wins = sum(result > 0),
         losses = sum(result < 0),
         ties = sum(result == 0)) |> 
  ungroup() |> 
    select(home_team, wins, losses, ties) |> 
    unique()
  
  return(schedules)
}

records_away <- function(schedules) {
  schedules <- schedules |> 
    group_by(away_team) |> 
    mutate(wins = sum(result < 0),
           losses = sum(result > 0),
           ties = sum(result == 0)) |> 
    ungroup() |> 
    select(away_team, wins, losses, ties) |> 
    unique()
  
  return(schedules)
}


schedules_home <- records_home(schedules) |> rename(team = home_team) # Use the functions to find the home and away records
schedules_away <- records_away(schedules)|> rename(team = away_team)
  
  
total_records <- rbind(schedules_home, schedules_away) |>  # Form the final dataset
  group_by(team) |> 
  mutate(wins = sum(wins),
         losses = sum(losses),
         ties = sum(ties)) |> 
  unique() |> 
  mutate(win_pct = (wins + 0.5*ties) / (wins + losses + ties)) |> 
  ungroup()


records_ptdiff <- total_records |> left_join(pbp_ptdiff_comb, by = "team") |> select(-X) |>  # Merge the pts and record datasets
  mutate(pt_diff = pf - pa)


records_ptdiff |> 
  ggplot(aes(x = pt_diff, y = win_pct)) +
  geom_image(aes(image = team_logo_espn), size = 0.04, asp = 16/9) +
  labs(x = "Point Differential",
       y = "Win %",
       title = "NFL Point Differential vs. Win Percentage",
       subtitle = "2023 Season W1-18 | Data via NFLReadR") +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_text(),
    axis.title.x = element_text()
  )

# Lets build the linear model
win_pct_model <- lm(win_pct ~ pt_diff, data = records_ptdiff)

summary(win_pct_model) # See the model stats

# Use our model to predict win percentage and wins over expected
records_ptdiff_predicted <- records_ptdiff |> 
  mutate(expected_wp = predict(win_pct_model),  
         wpoe = win_pct - expected_wp,
         expected_wins = round(expected_wp*17, 1),
         wins_oe = round(wpoe*17, 1))


## Redo the graph and add the regression line and residuals
records_ptdiff_predicted |> 
  ggplot(aes(x = pt_diff, y = win_pct)) +
  #############################
  geom_smooth(method = "lm", color = "black", se = F) +
  ############################# 
  geom_segment(aes(x = pt_diff, y = win_pct, xend = pt_diff, yend = expected_wp), 
               color = "gray", linetype = "dashed") +
  #############################
  geom_image(aes(image = team_logo_espn), size = 0.04, asp = 16/9) +
  labs(x = "Point Differential",
       y = "Win %",
       title = "NFL Point Differential vs. Win Percentage",
       subtitle = "2023 Season W1-18 | Data via NFLReadR") +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_text(),
    axis.title.x = element_text()
  ) + 
  annotate("text", x = -150, y = 0.75, label = "R^2 = 0.8009") # Add the R-squared as a label (80.09% of the variance is explained by point differential



### Next we are going to make an expected wins vs actual wins plot (just similar to residual plo)
records_ptdiff_predicted |> 
  ggplot(aes(x = wins, y = expected_wins)) +
  geom_image(aes(image = team_logo_espn), size = 0.03, asp = 16/9) +
  labs(x = "Wins",
       y = "Expected Wins",
       title = "Wins vs. Expected Wins",
       subtitle = "2023 Season W1-18 | Data via NFLReadR") +
  theme_bw() +
  xlim(1,14) + # Limits of the graph
  ylim(1,14) + # Limits of the graph
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_text(),
    axis.title.x = element_text()
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") # Line for matches






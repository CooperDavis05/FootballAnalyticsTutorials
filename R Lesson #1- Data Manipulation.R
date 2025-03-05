# Start with how to access a workspace and creating a new script

# https://mathcenter.oxford.emory.edu/site/math117/installRStudio/


########################################################################################################################
#First we are going to learn how to write a comment:

# See when I type like this the error it causes (you can type anything you want)
## Error: unexpected symbol in "See when"

# To write a comment, simply put a hashtag symbol


########################################################################################################################
# Next we will learn how to install packages

install.packages("tidyverse") # You can use the "help" function to aid you in the calling of functions

library(tidyverse)            # Load the package in (have to do this every time)

# Install the other necessary packages for this:

install.packages("nflreadr")  # For NFL Data
install.packages("ggimage")   # For images in Plots
install.packages("gt")        # For tables 
install.packages("gtExtras")  # Adds to tables

library(nflreadr) # Make sure these are not in quotes (quotes for install, none for library)
library(ggimage)
library(gt)
library(gtExtras)

########################################################################################################################
# We are gonna play around with the data
install.packages("future") # We need the package "Future"
library(future)

future::plan("multisession")
pbp <- load_pbp(2023)

head(pbp) # Lets us see the first few rows

head(pbp, n = 10) # Lets us change the amount of rows we want to see

dim(pbp) # Gives you the dimensions (you can see in your environment as well)



# |> is the pipe operator, a short cut is command-shift-m on mac 
### an alternative is %>% (both the same, so don't worry)

# Select to select the columns you want to look at
pbp_selected <- pbp |> 
  select(game_id, week, home_team, away_team, season_type, game_date, result, home_score, away_score)

# Unique to get each individual game
pbp_selected <- pbp_selected |> 
  unique()

# Filter only postseason games
pbp_selected_post <- pbp_selected |> 
  filter(season_type == "POST")

# Mutate to find the winners
pbp_post_winners <- pbp_selected_post |> 
  mutate(home_winner = result > 0,
         # Now use "ifelse" to create a column of who won
         winning_team = ifelse(home_winner == T, home_team, away_team))

# Use this new column to find out who won the Super Bowl:
pbp_post_winners |> filter(week == 22) |> select(winning_team)



# We are going to move back to pbp_selected a to find point differential (using group_by)

pbp_selected_ptdiff <- pbp_selected |> 
  select(game_id, week, home_team, away_team, season_type, result, home_score, away_score) |> 
  # Make sure regular season first
  filter(season_type == "REG") |> 
  group_by(home_team) |> # First group by home team to find total home differential
  mutate(home_pf = sum(home_score),
         home_pa = sum(away_score)) |> 
  ungroup() |>           # Always need this after a group_by
  group_by(away_team) |> # Total away differential
  mutate(away_pf = sum(away_score),
         away_pa = sum(home_score)) |> 
  ungroup()

# For convenience we will split this into 2 datasets and recombine

pbp_selected_ptdiff1 <- pbp_selected_ptdiff |> 
  # Reframe lets kind of combine mutate and select
  reframe(team = home_team, pf = home_pf, pa = home_pa) |>
  unique()

pbp_selected_ptdiff2 <- pbp_selected_ptdiff |> 
  reframe(team = away_team, pf = away_pf, pa = away_pa) |>  
  unique()

# rbind binds the rows (need the exact same row names)
pbp_ptdiff_comb <- rbind(pbp_selected_ptdiff1, pbp_selected_ptdiff2) |> 
  # Now find the total of pf and pa using a similar tactic as above
  group_by(team) |> 
  mutate(pf = sum(pf), pa = sum(pa)) |> 
  unique()


### We are going to learn how to save a dataset

write.csv(pbp_ptdiff_comb, "point_differential.csv")


  

  


  
  



  
  












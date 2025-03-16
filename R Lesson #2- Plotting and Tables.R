library(tidyverse) 
library(nflreadr) 
library(ggimage)
library(gt)
library(gtExtras)


pbp_ptdiff_comb <- read.csv("point_differential.csv")


########################################################################################################################
# Plot of pf in terms of a bar chart
teams_colors_logos <- load_teams()
view(teams_colors_logos) # This has the team logos

teams_colors_logos_logo <- teams_colors_logos |> select(team_abbr, team_logo_espn, team_wordmark, team_color, team_color2)

pbp_ptdiff_comb <- pbp_ptdiff_comb |> 
  left_join(teams_colors_logos_logo, by = c("team" = "team_abbr"))



pbp_ptdiff_comb |> 
  ggplot(aes(x = reorder(team, pf), y = pf)) +
  # Make the y axis go opposite (fewer points at the top)
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_image(aes(y = pf + 11, image = team_logo_espn), asp = 16/9, size = 0.025)
  





########################################################################################################################
# Plot of pf and pa
pbp_ptdiff_comb |> 
  ggplot(aes(x = pf, y = pa)) +
  # Make the y axis go opposite (fewer points at the top)
  scale_y_reverse() + 
  geom_point()

# Now how to do we edit the plot to make it look better?


pbp_ptdiff_comb |> 
  ggplot(aes(x = pf, y = pa)) +
  geom_abline(slope = -1, intercept = 0, color = "gray", alpha = 0.5) +
  geom_abline(slope = -1, intercept = -50, color = "gray", alpha = 0.5) +
  geom_abline(slope = -1, intercept = 50, color = "gray", alpha = 0.5) +
  geom_abline(slope = -1, intercept = -100, color = "gray", alpha = 0.5) +
  geom_abline(slope = -1, intercept = 100, color = "gray", alpha = 0.5) +
  geom_abline(slope = -1, intercept = -150, color = "gray", alpha = 0.5) +
  geom_abline(slope = -1, intercept = 150, color = "gray", alpha = 0.5) +
  geom_abline(slope = -1, intercept = -200, color = "gray", alpha = 0.5) +
  geom_abline(slope = -1, intercept = 200, color = "gray", alpha = 0.5) +
  scale_y_reverse() +
  geom_image(aes(image = team_logo_espn), size = 0.04, asp = 16/9) +
  # ggplot has many themes, but we are going to make it look nice using black and white theme
  theme_bw() +
  labs(x = "Points For",
       y = "Points Against",
       title = "NFL Points For and Against",
       subtitle = "2023 Season W1-18 | Data via NFLReadR") +
  # Time to add more theme-like stuff
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave('NFLPFPA.png', width = 14, height = 10, dpi = "retina")

########################################################################################################################
# Create a table now

pbp_ptdiff_comb |> 
  summarize(team_wordmark,pf, pa) |> 
  mutate(pt_diff = pf - pa) |> 
  # Arrange by differential
  arrange(-pt_diff) |> 
  gt() |> 
  #Include team wordmarks in the plot
  gt_img_rows(team_wordmark) |> 
  #Label the columns
  cols_label(
    team_wordmark = "Team",
    pf = "PF",
    pa = "PA",
    pt_diff = "PD"
  ) |> 
  # Align the columns
  cols_align(align = "center") |>
  gt_theme_espn() |> 
  # Give the plot a title
  tab_header(
    title = "Team Point Differentials 2023"
  ) |> 
  # Color the PD column based on performance
  data_color(
    columns = c(pt_diff),
    target_columns = c(pt_diff),
    colors = scales::col_numeric(
      palette = c("#FF9999", "#99CC99"),
      domain = NULL
    )
  )

write.csv(pbp_ptdiff_comb, "pbp_ptdiff_comb.csv")





  
  

library(dplyr)
library(ggplot2)
library(data.table)

# merge to get player name and player position
player_play_distance_full <- player_play_distance %>%
  left_join(
    input_all[, .(nfl_id, player_name, player_position)] %>% distinct(),
    by = "nfl_id"
  )

# targeted receivers only
receivers <- player_play_distance_full %>%
  filter(player_role == "Targeted Receiver")

# player level summary stats
player_stats <- receivers %>%
  filter(player_position %in% c('WR', 'RB', 'TE')) %>%
  group_by(nfl_id, player_name, player_position,down) %>%
  summarise(
    mean_distance   = mean(total_distance, na.rm = TRUE),
    sd_distance     = sd(total_distance, na.rm = TRUE),
    min_distance    = min(total_distance, na.rm = TRUE),
    max_distance    = max(total_distance, na.rm = TRUE),
    median_distance = median(total_distance, na.rm = TRUE),
    n_plays         = n(),
    .groups = "drop"
  ) %>%
  arrange(player_position, player_name, down)
player_stats


# position level summary stats
position_stats <- player_stats %>%
  group_by(player_position, down) %>%
  summarise(
    mean_distance   = mean(mean_distance, na.rm = TRUE),
    min_distance    = min(min_distance, na.rm = TRUE),
    max_distance    = max(max_distance, na.rm = TRUE),
    median_distance = median(median_distance, na.rm = TRUE),
    n_players       = n(),
    .groups = "drop"
  )
position_stats



# Top 20 receivers by overall mean distance
top20_receivers <- player_stats %>%
  group_by(player_name) %>%
  summarise(overall_mean = mean(mean_distance, na.rm = TRUE)) %>%
  arrange(desc(overall_mean)) %>%
  slice_head(n = 20) %>%
  pull(player_name)

top20_stats <- player_stats %>% filter(player_name %in% top20_receivers)

#**** Heatmap: mean distance per down per player
ggplot(top20_stats, aes(x = factor(down), y = reorder(player_name, -mean_distance), fill = mean_distance)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Top 20 Receivers (Mean distance on all plays) Mean Distance per Down ",
    x = "Down",
    y = "Player",
    fill = "Mean Distance (yards)"
  )

#**** Boxplot: mean distance by position and down
ggplot(player_stats, aes(x = factor(down), y = mean_distance, fill = player_position)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Receiver Movement by Position and Down ",
    x = "Down",
    y = "Mean Total Distance (yards)",
    fill = "Position"
  )


#***** Bar chart: average movement by position and down
position_down_summary <- player_stats %>%
  group_by(player_position, down) %>%
  summarise(mean_distance = mean(mean_distance, na.rm = TRUE), .groups = "drop")

ggplot(position_down_summary, aes(x = player_position, y = mean_distance, fill = factor(down))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Average Movement by Position and Down",
    x = "Position",
    y = "Mean Total Distance (yards)",
    fill = "Down"
  )

#**** Heat map: receivers position and downs
ggplot(position_down_summary, aes(x = factor(down), y = reorder(player_position, -mean_distance), fill = mean_distance)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Receiver Mean Distance",
    x = "Down",
    y = "Player",
    fill = "Mean Distance (yards)"
  )

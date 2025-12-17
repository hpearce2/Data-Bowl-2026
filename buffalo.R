
######################### buffalo analysis

# filter for BUF and receivers only
buf_receivers <- player_play_distance %>%
  filter(possession_team == "BUF", 
         player_role == "Targeted Receiver")


buf_plot_data <- buf_receivers %>%
  group_by(down, player_role) %>%
  summarise(
    mean_distance = mean(total_distance, na.rm = TRUE),
    se_distance = sd(total_distance, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  )

# total distance by down and role
ggplot(buf_plot_data, aes(x = factor(down), y = mean_distance)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_distance - se_distance, ymax = mean_distance + se_distance),
                position = position_dodge(width = 0.7), width = 0.2) +
  #scale_fill_manual(values = c("Targeted Receiver" = "cornflowerblue")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "BUF Receiver Movement by Down",
    x = "Down",
    y = "Mean Total Distance (yards)"
  ) 
#theme(legend.position = "top", plot.title = element_text(hjust = 0.5))


# boxplot, distrigution of total distance
ggplot(buf_receivers, aes(x = factor(down), y = total_distance)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  #scale_fill_manual(values = c("Targeted Receiver" = "steelblue", "Other Route Runner" = "orange")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "BUF Receiver Movement Distribution by Down",
    x = "Down",
    y = "Total Distance (yards)"
  ) 




# Merge player_name and player_position from input_all into player_play_distance
player_play_distance_full <- player_play_distance %>%
  left_join(
    input_all[, .(nfl_id, player_name, player_position)] %>% distinct(),
    by = "nfl_id"
  )

# Filter for BUF and receivers only
buf_receivers <- player_play_distance_full %>%
  filter(possession_team == "BUF",
         player_role %in% c("Targeted Receiver", "Other Route Runner"))

# player summary stats
buf_stats <- buf_receivers %>%
  group_by(nfl_id, player_name, player_position, player_role, down) %>%
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
buf_stats



# position summary stats
position_stats <- buf_stats %>%
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

#### boxplot mean distance position and downs
ggplot(buf_stats, aes(x = factor(down), y = mean_distance, fill = player_position)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "BUF Receiver Movement by Player Position and Down",
    x = "Down",
    y = "Mean Total Distance (yards)",
    fill = "Position"
  )


####  top 5 recievers bar plot
# Find top 5 receivers by overall mean distance
top5_receivers <- buf_stats %>%
  group_by(player_name) %>%
  summarise(overall_mean = mean(mean_distance, na.rm = TRUE)) %>%
  arrange(desc(overall_mean)) %>%
  slice_head(n = 5) %>%
  pull(player_name)

# Top 5 receivers (already calculated in top5_receivers)
buf_top5_stats <- buf_stats %>%
  filter(player_name %in% top5_receivers)

# Plot: dodged bar chart
ggplot(buf_top5_stats, aes(x = factor(down), y = mean_distance, fill = player_name)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  theme_minimal() +
  labs(
    title = "Top 5 BUF Receivers: Mean Distance by Down",
    x = "Down",
    y = "Mean Total Distance (yards)",
    fill = "Player"
  ) +
  scale_x_discrete(limits = c("1", "2", "3", "4")) +
  theme(legend.position = "top")



##  heat map of mean distance per down per player
ggplot(buf_stats, aes(x = factor(down), y = reorder(player_name, -mean_distance), fill = mean_distance)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Heatmap: BUF Receivers' Mean Distance per Down",
    x = "Down",
    y = "Player",
    fill = "Mean Distance (yards)"
  )





### avreage movement by position of targeted receiver and down

# Summarize mean distance by position and down
position_down_summary <- buf_stats %>%
  group_by(player_position, down) %>%
  summarise(mean_distance = mean(mean_distance, na.rm = TRUE), .groups = "drop")

# Bar chart: positions x downs
ggplot(position_down_summary, aes(x = player_position, y = mean_distance, fill = factor(down))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Average Movement by Position and Down (BUF Receivers)",
    x = "Position",
    y = "Mean Total Distance (yards)",
    fill = "Down"
  )




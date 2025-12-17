## include only OLS and random intercept models in report


# OLS/fixed effects with down * player_role
mod <- lm(total_distance ~ down * player_role, data = player_play_distance)
summary(mod)
# defensive coverage, 1st down, baseline, 1.98 yards per play
# other route runner and passer = -1.98 yards, essentially 0 distnance for 1st down
# targeted reciever = 4.21, so 6.2 yards on down 1
# down 2 = -0.18 so 1.8 yards for defense
# down 3 = 0.26 so so 2.25 yards for defense
# down 4 = 0.5 so 2.48 yards for defense
# 

# targeted recievers move the most while ball is in the air
# defensive coverage about 2 yards
# passters and other route runners barely move post throw
# down has small effect, defense moves slighlyy more on 3rd and 4th downs
# targeted receivers seem to move less on 4th down, could be because most 4th down plays are rushes not passes


# estimated marginal means for all roles
emmeans(mod, ~ player_role | down)



#### random intercept model
re_model <- lmer(
  total_distance ~ down + player_role +
    (1 | nfl_id),
  data = player_play_distance
)

summary(re_model)
# adds random intercepts per player to account for repeated measures
# variance of player intercepts = 0.108, individual differences in total distance are small relative to residual variance =10.19
# fixed effects estimates similar to OLS, so repeated measures don't drastically change average effects

# player identity introduces small variability, but main patterns(targeted receivers move most) are unchanged



### random slope model
re_slope_model <- lmer(
  total_distance ~ down + player_role +
    (1 + down | nfl_id),
  data = player_play_distance,
  control = lmerControl(optimizer = "bobyqa")
)

summary(re_slope_model)
# allows the effect of down to vary per player
# singular warning, some random sloes near zero or perfectly correlated
# interpretation of fixed effects similar to random intercept model so main conclusions are robust
# random slope correlations show some near +-1 which fits singular warning

# adding slopers per player adds complexity but doesn't change the qualitative pattern
# targeted recievers move the most, defensive players move moderately, passers/route runners don't move much


# Post-hoc comparisons between downs
emmeans(re_model, pairwise ~ down)



#### team-level random effects

team_model <- lmer(
  total_distance ~ down + player_role +
    (1 | nfl_id) +
    (1 | possession_team),
  data = player_play_distance
)

summary(team_model)
# add team level random intercept, variance =0.032, small team differences in average movement
# player level variance still larger = 0.109
# fixed effects again same

# most variabilty is within players rather than between teams so team membershipd doesn't strongly affect total movement



# plots
# actually use plot A or B

# only targeted receiver and defense
plot_data <- player_play_distance %>%
  filter(player_role %in% c("Targeted Receiver", "Defensive Coverage")) %>%
  group_by(down, player_role) %>%
  summarise(
    mean_distance = mean(total_distance, na.rm = TRUE),
    se_distance = sd(total_distance, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# plot A
# distance of targeted receiver and defense for each down
ggplot(plot_data, aes(x = factor(down), y = mean_distance, fill = player_role)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_distance - se_distance, ymax = mean_distance + se_distance),
                position = position_dodge(width = 0.7), width = 0.2) +
  scale_fill_manual(values = c("Targeted Receiver" = "cornflowerblue", "Defensive Coverage" = "red3")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Player Movement While Ball is in Air by Player Role",
    x = "Down",
    y = "Mean Total Distance (yards)",
    fill = "Player Role"
  ) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )


# plot B
# line 
ggplot(plot_data, aes(x = factor(down), y = mean_distance, color = player_role, group = player_role)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_distance - se_distance, ymax = mean_distance + se_distance),
                width = 0.2) +
  scale_color_manual(values = c("Targeted Receiver" = "steelblue", "Defensive Coverage" = "orange")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Average Player Movement Across Downs",
    x = "Down",
    y = "Mean Total Distance (yards)",
    color = "Player Role"
  ) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5))


# Boxplot
plot_box <- player_play_distance %>%
  filter(player_role %in% c("Targeted Receiver", "Defensive Coverage"))

ggplot(plot_box, aes(x = factor(down), y = total_distance, fill = player_role)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Targeted Receiver" = "steelblue", "Defensive Coverage" = "orange")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of Player Movement by Role and Down",
    x = "Down",
    y = "Total Distance (yards)",
    fill = "Player Role"
  ) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5))



### violin plot
plot_violin <- player_play_distance %>%
  filter(player_role %in% c("Targeted Receiver", "Defensive Coverage"))

ggplot(plot_violin, aes(x = factor(down), y = total_distance, fill = player_role)) +
  geom_violin(alpha = 0.6, position = position_dodge(width = 0.8)) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, color = "black",
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Targeted Receiver" = "steelblue", "Defensive Coverage" = "orange")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of Player Movement (Violin Plot)",
    x = "Down",
    y = "Total Distance (yards)",
    fill = "Player Role"
  ) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  ylim(0,10)





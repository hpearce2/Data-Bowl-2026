library(data.table)
library(tidyverse)
library(zoo)
library(lme4)
library(emmeans)
library(ggplot2)

# pre throw data
input_files <- sprintf("input_2023_w%02d.csv", 1:18)

input_all <- rbindlist(
  lapply(input_files, fread),
  use.names = TRUE,
  fill = TRUE
)

input_all[, phase := "pre_throw"]

# post throw data
output_files <- sprintf("output_2023_w%02d.csv", 1:18)

output_all <- rbindlist(
  lapply(output_files, fread),
  use.names = TRUE,
  fill = TRUE
)

output_all[, phase := "post_throw"]

# combine pre and post throw
tracking_all <- rbindlist(
  list(input_all, output_all),
  use.names = TRUE,
  fill = TRUE
)

# play data
plays <- fread("supplementary_data.csv")

tracking_all <- plays[
  tracking_all,
  on = .(game_id, play_id)
]

tracking_all <- tracking_all[!is.na(frame_id)]


# clean and forward-fill player_role
tracking_all[, player_role := trimws(player_role)]

setorder(tracking_all, game_id, play_id, nfl_id, frame_id)

tracking_all[, player_role := na.locf(
  player_role,
  na.rm = FALSE
), by = .(game_id, play_id, nfl_id)]

# Filter to post-throw frames for all downs
air_tracking <- tracking_all[
  phase == "post_throw" &
    down %in% 1:4 &
    !is.na(nfl_id)
]

# Sort for distance calculations
setorder(air_tracking, game_id, play_id, nfl_id, frame_id)

# Frame-to-frame distances
air_tracking[, `:=`(
  dx = ifelse(!is.na(x), x - shift(x), NA_real_),
  dy = ifelse(!is.na(y), y - shift(y), NA_real_)
), by = .(game_id, play_id, nfl_id)]

air_tracking[, step_dist := sqrt(dx^2 + dy^2)]

# Summarize per player-play, keeping all player roles
player_play_distance <- air_tracking[, .(
  total_distance = sum(step_dist, na.rm = TRUE),
  mean_speed     = mean(s, na.rm = TRUE),
  max_speed      = max(s, na.rm = TRUE),
  n_frames       = sum(!is.na(step_dist)),
  down           = first(down),
  player_role    = first(player_role),
  possession_team = first(possession_team)
), by = .(game_id, play_id, nfl_id)]


# Include players who may have zero post-throw frames
all_players <- unique(tracking_all[, .(game_id, play_id, nfl_id, player_role, possession_team, down)])
player_play_distance <- merge(
  all_players,
  player_play_distance,
  by = c("game_id","play_id","nfl_id","player_role","possession_team","down"),
  all.x = TRUE
)

# Replace NA distances/speeds with 0
player_play_distance[is.na(total_distance), total_distance := 0]
player_play_distance[is.na(mean_speed), mean_speed := 0]
player_play_distance[is.na(max_speed), max_speed := 0]
player_play_distance[is.na(n_frames), n_frames := 0]

# Normalize distance by time (10 frames/sec)
player_play_distance[, dist_per_sec := total_distance / (pmax(n_frames,1) / 10)]



# Keep all original 4 roles for now
all_roles <- unique(input_all$player_role)
player_play_distance[, player_role := factor(player_role, levels = all_roles)]
player_play_distance[, down := factor(down)]
player_play_distance[, possession_team := factor(possession_team)]



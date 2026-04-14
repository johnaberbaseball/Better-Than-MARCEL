# Building MARCEL
reorganized_data <- data %>%
select(bbref_id, Age, PA1, PA2, PA3, OPSY1, OPSY2, OPSY3) %>%
mutate(OPS_next_year = ((5*OPSY1) + (4*OPSY2) + (3*OPSY3))/(12)) %>%
mutate(reliability = (PA1 + PA2 + PA3)/(PA1 + PA2 + PA3 + 1200)) %>%
mutate(regressed_rate = (reliability * OPS_next_year) + ((1 - reliability) * .720)) %>%
mutate(age_adjustment = case_when(
    Age > 29 ~ (Age - 29) * -.003,
    Age < 29 ~ (29 - Age) * .006,
    TRUE ~ 0)) %>%
mutate(MARCEL_OPS = (1 + age_adjustment) * regressed_rate)
reorganized_data %>% head()


# Computing primary position (most games played)
primary_pos <- player_data %>%
  group_by(bbref_player_id, primary_position) %>%
  summarize(total_games = sum(`g...6`, na.rm = TRUE), .groups = "drop") %>%
  group_by(bbref_player_id) %>%
  slice_max(total_games, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(bbref_player_id, primary_position)

# Joining to MARCEL dataset
reorganized_data <- reorganized_data %>%
  left_join(primary_pos, by = c("bbref_id" = "bbref_player_id"))

# Building positional adjustment
league_avg <- mean(reorganized_data$MARCEL_OPS, na.rm = TRUE)
pos_avg <- reorganized_data %>%
    group_by(primary_position) %>%
    summarize(pos_mean = mean(MARCEL_OPS, na.rm = TRUE), .groups = "drop")

# Joining back and computing adjusted BETTER_THAN_MARCEL
reorganized_data <- reorganized_data %>%
    left_join(pos_avg, by = "primary_position") %>%
    mutate(BETTER_THAN_MARCEL_OPS = MARCEL_OPS + (pos_mean - league_avg)) %>%
    select(-pos_mean)
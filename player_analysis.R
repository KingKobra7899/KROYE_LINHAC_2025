games_per_player <- data %>%
  select(gameid, playerid) %>%
  distinct() %>%
  group_by(playerid) %>%
  summarise(games_played = n())


player_assists <- data %>%
  filter(eventname == "assist") %>%
  group_by(playerid) %>%
  summarise(assists = n())


player_pxg <- all_passes %>%
  group_by(passing_id) %>%
  summarise(
    total_pxg = sum(PxG, na.rm = TRUE),
    passes = n(),
    avg_pxg_per_pass = total_pxg / passes
  )


player_analysis <- player_pxg %>%
  left_join(player_assists, by = c("passing_id" = "playerid")) %>%
  left_join(games_per_player, by = c("passing_id" = "playerid")) %>%
  mutate(
    assists = ifelse(is.na(assists), 0, assists),
    games_played = ifelse(is.na(games_played), 0, games_played),
    pxg_per_game = total_pxg / games_played,
    assists_per_game = assists / games_played
  ) %>%
  filter(games_played >= 5)  # Minimum games threshold for sample size

ggplot(player_analysis, aes(x = pxg_per_game, y = assists_per_game)) +
  geom_point(alpha = 0.7, aes(size = games_played)) +
  stat_poly_line()+
  stat_poly_eq() +
  
  labs(
    x = "Pass Expected Goals (PxG) per Game",
    y = "Assists per Game",
    size = "Games Played"
  )+
  lncs_theme


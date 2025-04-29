library(readr)
library(ggplot2)
library(sportyR)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(sportyR)  
library(viridis) 
library(gridExtra)

data <- read_csv("Linhac24-25_Sportlogiq.csv")



data <- data %>%
  mutate(
         successful_pass = eventname == "pass" & outcome == "successful",
         successful_reception = eventname == "reception" & outcome == "successful",
         pass_id = ifelse(successful_pass, sprintf("%20.0f", runif(n(), 1e8, 1e9)), NA),
         reception_id = lag(pass_id))


data <- data %>%
  mutate(prev_was_reception = FALSE, play_after_reception_id = NA)

# Loop through each row where a reception happened
for (i in which(data$successful_reception)) {
  row <- data[i, ]
  n <- i + 1
  
  # Track the last play of the same player after the reception
  last_play_idx <- i
  shot_idx <- NA
  
  while (n <= nrow(data) && data[n, ]$playerid == row$playerid) {
    if (data[n, ]$eventname == "shot") {
      shot_idx <- n  # Record the first shot after the reception
      break  # Stop at the first shot
    }
    last_play_idx <- n  # Update last play index
    n <- n + 1
  }
  
  # If there's a shot, use its index; otherwise, use the last play index
  target_idx <- ifelse(!is.na(shot_idx), shot_idx, last_play_idx)
  
  # Update the output variables
  data$prev_was_reception[target_idx] <- TRUE
  data$play_after_reception_id[target_idx] <- row$reception_id
}

pass <- data %>% filter(successful_pass)
reception <- data %>% filter(successful_reception)
plays_after <- data %>% filter(prev_was_reception,
                               eventname == "shot",
                               !is.na(play_after_reception_id),
                               !is.na(opposingteamgoalieoniceid))

pass_dataset = merge(pass, reception, by.x = "pass_id", by.y = "reception_id")

pass_dataset <- pass_dataset %>% reframe(
  play_id = pass_id,
  passing_id = playerid.x,
  recieving_id = playerid.y,
  pass_type = factor(type.x),
  reception_type = factor(type.y),
  pass_time = compiledgametime.x,
  reception_time = compiledgametime.y,
  dt = reception_time - pass_time,
  pass_x = xadjcoord.x,
  pass_y = yadjcoord.x,
  reception_x = xadjcoord.y,
  reception_y = yadjcoord.y,
  distance = sqrt((reception_x - pass_x)^2 + (reception_y - pass_y)^2),
  vel = distance / dt,
  game_state = manpowersituation.x
)%>%filter(dt < 10 & dt > 0)

plays_after <- plays_after%>%reframe(play_id = play_after_reception_id,
                                     xG = xg_allattempts,
                                     is_goal = outcome == "successful",
                                     shot_type = type,
                                     shot_x = xadjcoord,
                                     shot_y = yadjcoord)

pass_dataset <- merge(pass_dataset, plays_after, by = "play_id")

ggplot(pass_dataset, aes(x = shot_x, y = shot_y, color = xG))+
  geom_point()

library(keras)

training_data <- model.matrix(~pass_x + pass_y + 
                                reception_x + 
                                reception_y + dt + distance + vel +
                                reception_type +
                                game_state - 1, data = pass_dataset)



means <- colMeans(pass_dataset[, c("pass_x", "pass_y", "reception_x", "reception_y", "dt", "distance", "vel")])
sds <- apply(pass_dataset[, c("pass_x", "pass_y", "reception_x", "reception_y", "dt", "distance", "vel")], 2, sd)


model <- keras_model_sequential() %>%
  layer_lambda(input_shape = c(ncol(training_data)), 
               f = function(x) {
                
                 x_cont <- x[, 1:7]
                 x_cat <- x[, 8:ncol(x)]
                 
                 # Apply normalization to continuous features
                 x_cont <- (x_cont - means) / sds
                 
                 # Concatenate back with categorical features
                 k_concatenate(list(x_cont, x_cat), axis = -1)
               }) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 4, activation = "relu") %>%
  layer_dense(units = 2, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = optimizer_rmsprop(0.0005),
  loss = 'mean_squared_error',
  metrics = c("mae")
)



history <- model %>% fit(
  x = training_data,
  y = pass_dataset$xG,
  validation_split = 0.1,
  shuffle = T,
  epochs = 32
)

epochs <- 1:32
training_loss <- history$metrics$loss
validation_loss <- history$metrics$val_loss
training_mae <- history$metrics$mae
validation_mae <- history$metrics$val_mae

# Combine data for plotting
plot_data <- data.frame(
  Epoch = rep(epochs, 4),
  Value = c(training_loss, validation_loss, training_mae, validation_mae),
  Metric = rep(c("MSE", "MSE", "MAE", "MAE"), each = 32),
  Dataset = rep(rep(c("Training", "Validation"), each = 32), 2)
)

# Create publication-quality plot suitable for LNCS
ggplot(plot_data, aes(x = Epoch, y = Value, color = Dataset, linetype = Dataset)) +
  geom_line(size = 0.7) +
  geom_point(size = 1.5) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Training" = "black", "Validation" = "darkgray")) +
  scale_linetype_manual(values = c("Training" = "solid", "Validation" = "dashed")) +
  theme_minimal(base_size = 9) +  # Smaller base font size for LNCS
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.1),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "lines"),
    legend.title = element_blank(),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "mm"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    strip.text = element_text(size = 9, face = "bold"),
    strip.background = element_rect(fill = "gray95", color = "black", size = 0.5)
  ) +
  labs(
    x = "Epoch",
    y = NULL,
    title = NULL,  # No title for LNCS
    subtitle = NULL,
    caption = NULL
  ) +
  scale_x_continuous(breaks = seq(0, 32, by = 4))


plot(history) + theme_bw()
hist(predict(model, training_data), breaks = 50)

pass_dataset$PxG <- c(predict(model, training_data))



prep_passing_data <- function(data){
  data <- data %>% mutate(
                          distance = sqrt((reception_x - pass_x)^2 + (reception_y - pass_y)^2),
                          vel = distance / dt,)
  return( model.matrix(~pass_x + pass_y + 
                         reception_x + 
                         reception_y + dt + distance + vel + reception_type +
                         game_state - 1, data = data))
}

all_passes <- merge(pass, reception, by.x = "pass_id", by.y = "reception_id") %>% reframe(
  teamid = teamid.x,
  team_against = opposingteamid.x,
  gameid = gameid.x,
  play_id = pass_id,
  passing_id = playerid.x,
  recieving_id = playerid.y,
  pass_type = factor(type.x),
  reception_type = factor(type.y),
  pass_time = compiledgametime.x,
  reception_time = compiledgametime.y,
  dt = reception_time - pass_time,
  pass_x = xadjcoord.x,
  pass_y = yadjcoord.x,
  reception_x = xadjcoord.y,
  reception_y = yadjcoord.y,
  game_state = manpowersituation.x
)%>%filter(dt > 0,
           dt < 10)



all_passes$PxG <- predict(model, prep_passing_data(all_passes))
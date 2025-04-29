all_passes$distance <- sqrt((all_passes$pass_x - all_passes$reception_x)^2 + (all_passes$pass_y - all_passes$reception_y)^2)
all_passes$vel <- all_passes$distance / all_passes$dt

create_rink <- function() {
  geom_hockey("nhl", display_range = "offense") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

lncs_theme <- theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.1),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "lines"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    plot.margin = margin(5, 5, 5, 5),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 10),
    strip.text = element_text(size = 9),
    strip.background = element_rect(fill = "gray95", color = "black", size = 0.5)
  )


p8 <- ggscatter(
  all_passes, 
  x = "vel", 
  y = "PxG",
  alpha = 0.3,
  color = "black",
  conf.int = TRUE,
  conf.int.level = 0.95,
  cor.coef = TRUE,
  cor.method = "spearman",
  xlab = "Pass Velocity",
  ylab = "Pass Danger (PxG)"
) + 
  lncs_theme +
  labs(title = NULL)

p11a <- ggscatter(
  pass_vectors, 
  x = "x_movement", 
  y = "PxG",
  alpha = 0.1,
  color = "black",
  conf.int = TRUE,
  conf.int.level = 0.95,
  cor.coef = TRUE,
  cor.method = "spearman",
  xlab = "Longitudinal Movement",
  ylab = "Pass Danger (PxG)"
) + 
  lncs_theme +
  labs(title = NULL) 

p11b <- ggscatter(
  pass_vectors, 
  x = "y_movement", 
  y = "PxG",
  alpha = 0.1,
  conf.int = TRUE,
  conf.int.level = 0.95,
  cor.coef = TRUE,
  cor.method = "spearman",
  color = "black",
  xlab = "Lateral Movement",
  ylab = NULL
) + 
  lncs_theme +
  labs(title = NULL)

p11c <- ggscatter(
  pass_vectors, 
  x = "angle", 
  y = "PxG",
  alpha = 0.1,
  color = "black",
  conf.int = TRUE,
  conf.int.level = 0.95,
  cor.coef = TRUE,
  cor.method = "spearman",
  ylab = NULL
) + 
  lncs_theme +
  labs(title = NULL)

# Arrange plots for publication
p_vector_metrics <- ggarrange(p11a, p11b, p11c, p8, 
                              labels = c("a", "b", "c", "d"),
                              font.label = list(size = 9, face = "bold"),
                              ncol = 2, nrow = 2)
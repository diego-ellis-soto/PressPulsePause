# Disentangling the anatomy of the anthropause
# Load the ggplot2 library
library(ggplot2)

# Create time variable
time <- seq(0, 10, by = 0.1)  # General time sequence
time_fine <- seq(0, 10, by = 0.01)  # Finer time sequence for sharper pulses

# Define wave characteristics
waves <- data.frame(
  Time = c(rep(time, 2), time_fine),  # Combine general and finer time for pulses and pauses
  Amplitude = c(
    0.025 * time,                    # Press: Gradual increase, max at 0.25
    ifelse(
      time >= 2.5 & time <= 3.5,    # Shorter pause at time 3
      0.15 * sin(2 * pi * (time - 3) / 1) - 0.375,  # Scaled to range -0.6 to 0.15
      ifelse(
        time >= 6.5 & time <= 8.0,  # Longer pause at time 7
        0.15 * sin(2 * pi * (time - 7) / 1.5) - 0.525,  # Scaled to range -0.9 to 0.15
        0
      )
    ),
    ifelse(
      time_fine <= 2,                # First pulse (Gaussian-like)
      1 * exp(-3 * (time_fine - 1)^2),  # Max at 1.0
      ifelse(
        time_fine >= 5 & time_fine <= 5.2,  # Second short, sharp pulse
        1 * exp(-100 * (time_fine - 5)^2),  # Max at 1.0
        0
      )
    )
  ),
  Type = c(rep("Press", length(time)), 
           rep("Pause", length(time)), 
           rep("Pulse", length(time_fine)))
)

# Plot the waves with facet_grid
ggplot(waves, aes(x = Time, y = Amplitude, color = Type)) +
  geom_line(size = 1) +
  facet_grid(Type ~ ., scales = "free_y", switch = "y") +  # Allow each panel to have its own y-scale
  labs(
    title = "Presses, Pauses, and Pulses in the Anthropocene",
    x = "Time",
    y = "Amplitude"
  ) +
  scale_color_manual(values = c("Press" = "antiquewhite4", "Pause" = "antiquewhite4", "Pulse" = "antiquewhite4")) +
  theme_classic() +
  theme(
    text = element_text(size = 19, face = "bold"),        # Set font size and bold for all text
    axis.title = element_text(size = 19, face = "bold"),  # Axis titles
    axis.text = element_text(size = 19, face = "bold"),   # Axis tick labels
    strip.text = element_text(size = 19, face = "bold"),  # Facet labels
    strip.placement = "outside",                         # Move facet labels outside the axis
    plot.title = element_text(hjust = 0.5, size = 19, face = "bold"),  # Center and bold title
    legend.position = "none"                              # Remove legend
  ) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +       # Increase x-axis tick density
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))+ylim(-1,1)  # Ensure space for positive/negative values

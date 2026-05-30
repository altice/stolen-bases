# ============================================
# The Art of the Steal
# Stolen Base Trends in MLB Since 1871
# Kulwant Saluja | github.com/altice
# ============================================

library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# Load data
batting <- read_csv("data/Batting.csv")
people  <- read_csv("data/People.csv")

# Aggregate stolen bases by year
sb_by_year <- batting %>%
  filter(!is.na(SB), !is.na(AB), AB > 0) %>%
  group_by(yearID) %>%
  summarise(
    total_SB   = sum(SB, na.rm = TRUE),
    total_AB   = sum(AB, na.rm = TRUE),
    total_G    = sum(G,  na.rm = TRUE),
    SB_per_game = total_SB / (total_G / 9)
  ) %>%
  filter(yearID >= 1871)

# Define eras
sb_by_year <- sb_by_year %>%
  mutate(era = case_when(
    yearID < 1920 ~ "Dead Ball Era",
    yearID < 1942 ~ "Live Ball Era",
    yearID < 1961 ~ "Integration Era",
    yearID < 1977 ~ "Expansion Era",
    yearID < 1994 ~ "Free Agency Era",
    yearID < 2005 ~ "Steroid Era",
    yearID < 2016 ~ "Post-Steroid Era",
    TRUE          ~ "Modern Era"
  ))

# Plot
p <- ggplot(sb_by_year, aes(x = yearID, y = SB_per_game, color = era)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.2, alpha = 0.6) +

  # Annotate key moments
  geom_vline(xintercept = 1920, linetype = "dashed", color = "gray50", alpha = 0.5) +
  annotate("text", x = 1921, y = max(sb_by_year$SB_per_game) * 0.95,
           label = "Live Ball Era", size = 2.8, color = "gray40", hjust = 0) +
  geom_vline(xintercept = 1973, linetype = "dashed", color = "gray50", alpha = 0.5) +
  annotate("text", x = 1974, y = max(sb_by_year$SB_per_game) * 0.95,
           label = "DH Rule", size = 2.8, color = "gray40", hjust = 0) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "gray50", alpha = 0.5) +
  annotate("text", x = 2022, y = max(sb_by_year$SB_per_game) * 0.88,
           label = "Bigger bases", size = 2.8, color = "gray40", hjust = 1) +

  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(1880, 2025, by = 20)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +

  labs(
    title    = "The Art of the Steal",
    subtitle = "Stolen bases per game across 150 years of MLB history",
    x        = "Year",
    y        = "Stolen Bases per Game",
    color    = "Era",
    caption  = "Data: Lahman Baseball Database | github.com/altice"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Save
ggsave("stolen_bases.png", plot = p, width = 12, height = 7, dpi = 150)
cat("Chart saved to stolen_bases.png\n")
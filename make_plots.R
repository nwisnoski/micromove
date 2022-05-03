library(tidyverse)
library(ggridges)


movements <- read_csv("./movement_rates.csv")

random_distributions <- read_csv(file = "random_movements.csv")
random_distributions_noback <- read_csv(file = "random_movements_noback.csv")
twitching_distributions <- read_csv(file = "twitching_movements.csv")
twitching_distributions_noback <- read_csv(file = "twitching_movements_noback.csv")

random_move_fig <- random_distributions %>% 
  filter(Organism %in% subset(movements, Domain == "Prokaryote")$Organism) %>% 
  mutate(dist_from_center = sqrt((x^2) + (y^2))) %>% 
  mutate(dist_from_center = dist_from_center*(10^-6)) %>% 
  arrange(Speed) %>% 
  mutate(Organism = factor(Organism, levels = unique(Organism), ordered = TRUE)) %>% 
  ggplot(aes(x = dist_from_center, color = as.factor(Speed), y = Organism, fill = as.factor(Speed))) + 
  geom_density_ridges(alpha = 0.5) +
  #facet_wrap(~Organism, scales = "free_y")+
  scale_x_log10() +
  theme_minimal() +
  labs(color = "Speed, um/s", fill = "Speed, um/s",
       x = "Distance traveled (m/d)") +
  scale_color_viridis_d() + 
  scale_fill_viridis_d()
random_move_fig


random_twitching_fig <- random_distributions %>% 
  filter(Organism %in% subset(movements, Domain == "Prokaryote")$Organism) %>% 
  bind_rows(twitching_distributions) %>% 
  mutate(dist_from_center = sqrt((x^2) + (y^2))) %>% 
  mutate(dist_from_center = dist_from_center*(10^-6)) %>% 
  arrange(Speed) %>% 
  mutate(Organism = factor(Organism, levels = unique(Organism), ordered = TRUE)) %>% 
  ggplot(aes(x = dist_from_center, color = as.factor(Speed), y = Organism, fill = as.factor(Speed))) + 
  geom_density_ridges(alpha = 0.5) +
  #facet_wrap(~Organism, scales = "free_y")+
  scale_x_log10() +
  theme_minimal() +
  labs(color = "Speed, um/s", fill = "Speed, um/s",
       x = "Distance traveled (m/d)") +
  scale_color_viridis_d() + 
  scale_fill_viridis_d()
ggsave(filename = "figure_distances_per_day.pdf", plot = random_twitching_fig, width = 8, height = 6)



random_move_noback_fig <- random_distributions_noback %>% 
  filter(Organism %in% subset(movements, Domain == "Prokaryote")$Organism) %>% 
  mutate(dist_from_center = sqrt((x^2) + (y^2))) %>% 
  mutate(dist_from_center = dist_from_center*(10^-6)) %>% 
  arrange(Speed) %>% 
  mutate(Organism = factor(Organism, levels = unique(Organism), ordered = TRUE)) %>% 
  ggplot(aes(x = dist_from_center, color = as.factor(Speed), y = Organism, fill = as.factor(Speed))) + 
  geom_density_ridges(alpha = 0.5) +
  #facet_wrap(~Organism, scales = "free_y")+
  scale_x_log10() +
  theme_minimal() +
  labs(color = "Speed, um/s", fill = "Speed, um/s",
       x = "Distance traveled (m/d)") +
  scale_color_viridis_d() + 
  scale_fill_viridis_d()
random_move_noback_fig


random_twitching_noback_fig <- random_distributions_noback %>% 
  filter(Organism %in% subset(movements, Domain == "Prokaryote")$Organism) %>% 
  bind_rows(twitching_distributions_noback) %>% 
  mutate(dist_from_center = sqrt((x^2) + (y^2))) %>% 
  mutate(dist_from_center = dist_from_center*(10^-6)) %>% 
  arrange(Speed) %>% 
  mutate(Organism = factor(Organism, levels = unique(Organism), ordered = TRUE)) %>% 
  ggplot(aes(x = dist_from_center, color = as.factor(Speed), y = Organism, fill = as.factor(Speed))) + 
  geom_density_ridges(alpha = 0.5) +
  #facet_wrap(~Organism, scales = "free_y")+
  scale_x_log10() +
  theme_minimal() +
  labs(color = "Speed, um/s", fill = "Speed, um/s",
       x = "Distance traveled (m/d)") +
  scale_color_viridis_d() + 
  scale_fill_viridis_d()
random_twitching_noback_fig

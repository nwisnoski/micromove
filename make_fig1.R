library(tidyverse)
library(data.table)
library(ggridges)

set.seed(82734)
movements <- read_csv("./movement_rates.csv")

seconds_per_day <- 60 * 60 * 24
um_to_m <- 1e-6


# make random path
start <- c(0,0)  
speed <- movements$Speed_umps[1] # per second speed
trajectory_i <- matrix(nrow = seconds_per_day, ncol = 2, dimnames = list(NULL, c("x", "y")))

for(t in 1:seconds_per_day){
  
  dist_i <- runif(1, 0, speed)
  
  theta <- runif(1, 0, 2*pi)
  
  if(t == 1){
    pos_t <- c(dist_i*sin(theta), dist_i*cos(theta))
  } else {pos_t <- c(trajectory_i[t-1,1] + dist_i*sin(theta),
                     trajectory_i[t-1,2] + dist_i*cos(theta))}
  
  trajectory_i[t, 1] <- pos_t[1]
  trajectory_i[t, 2] <- pos_t[2]
}

random_walk_fig <- trajectory_i %>%
  as_tibble() %>%
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 0.2) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())



truncated_walk <- trajectory_i %>%
  as_tibble() %>%
  slice(6500:9000) %>%
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 0.2) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())

ggsave("figure_random_walk.pdf", random_walk_fig, width = 8, height = 8)
ggsave("figure_short_walk.pdf", truncated_walk, width = 8, height = 8)


start_endpoints <- tibble(
  bind_rows(
    trajectory_i %>% 
      as_tibble() %>% 
      slice_head(n = 1),
    trajectory_i %>% 
      as_tibble() %>% 
      slice_tail(n = 1)
  )
)

net_dist <- dist(start_endpoints)[1]

# work on difference table to compute distances
total_dist <- diff(trajectory_i) %>% 
  as_tibble() %>% 
  mutate(dist = sqrt(x^2 + y^2)) %>% 
  sum(.$dist)

xrng <- range(trajectory_i[,1])
yrng <- range(trajectory_i[,2])

random_walk_annotated <- trajectory_i %>% 
  as_tibble() %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_path(size = 0.2, alpha = 0.5) +
  geom_path(data = trajectory_i %>% 
              as_tibble() %>% 
              slice(6500:9000),
            color = "red") +
  geom_point(data = start_endpoints[1,], color = "blue", size = 3) + 
  geom_point(data = start_endpoints[2,], color = "blue", size = 3) + 
  geom_segment(data = start_endpoints, 
               mapping = aes(x = x[1], xend = x[2], 
                             y = y[1], yend = y[2]),
               color = "blue", size = 1) + 
  theme_bw() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) + 
  annotate("text", x = xrng[2]- 10000, y = yrng[1] + 5000, label = paste("Net distance =", round(net_dist)), color = "blue", hjust = 1) +
  annotate("text", x = xrng[2]- 10000, y = yrng[1] - 3000, label = paste("Total distance =", round(total_dist)), hjust = 1) + 
  annotate("text", x = 29000, y = -4000, label = "Movement phase\ntransition", color = "red")
random_walk_annotated  
ggsave("figure_random_walk_annotated.pdf", random_walk_annotated, width = 8, height = 8)


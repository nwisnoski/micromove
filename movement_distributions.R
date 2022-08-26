library(tidyverse)
library(data.table)
library(ggridges)

movements <- read_csv("./movement_rates.csv")

movements %>% 
  filter(Domain == "Prokaryote") %>% 
  arrange(desc(Speed_umps)) %>% 
  mutate(Organism = factor(Organism, levels = Organism, ordered = T)) %>% 
  ggplot(aes(x = Organism, y = Speed_umps)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

speed <- movements[1]

seconds_per_day <- 60 * 60 * 24
um_to_m <- 1e-6

# straight_line: 
for(i in 1:nrow(movements)){
  dist_i <- movements$Speed_umps[i] * seconds_per_day * um_to_m
  movements$Max_dist_m[i] <- dist_i
}

# make random path
start <- c(0,0)  
speed <- movements$Speed_umps[i] # per second speed
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



# random directions: 
nreps = 1000
random_distributions <- data.table()

for(i in 1:nrow(movements)){
  
  start <- c(0,0)  
  
  speed <- movements$Speed_umps[i] # per second speed
  
  for(r in 1:nreps){
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
    
    this_endpoint <- data.frame(Organism = movements$Organism[i], 
                       Speed = movements$Speed_umps[i], 
                       x = trajectory_i[t,1], 
                       y = trajectory_i[t,2])
    random_distributions <- bind_rows(random_distributions, this_endpoint)
  }
  print(paste("Done with taxon", movements$Organism[i]))
}

write_csv(random_distributions,file = "random_movements.csv")

trajectory_i[1:100,] %>% 
  as.data.frame() %>%
  ggplot(aes(x = x, y = y)) + 
  geom_path()


random_move_fig <- random_distributions %>% 
  filter(Organism %in% subset(movements, Domain == "Prokaryote")$Organism) %>% 
  mutate(dist_from_center = sqrt((x^2) + (y^2))) %>% 
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

# twitching directions: 
nreps = 1000
twitching_distributions <- data.table()


start <- c(0,0)  

#speed <- movements$Speed_umps[i] # per second speed
speed <- 1 # 1 micron / sec is the typical speed here.

for(r in 1:nreps){
  trajectory_i <- matrix(nrow = seconds_per_day, ncol = 2, dimnames = list(NULL, c("x", "y")))
  
  theta = pi/2 # start in a single direction
  
  for(t in 1:seconds_per_day){
    
    dist_i <- runif(1, 0, speed)
    
    if(runif(1) < 0.25){
      theta <- runif(1, 0, 2*pi) # a quarter of a the time, change directions
    }
    
    if(t == 1){
      pos_t <- c(dist_i*sin(theta), dist_i*cos(theta))
    } else {pos_t <- c(trajectory_i[t-1,1] + dist_i*sin(theta),
                       trajectory_i[t-1,2] + dist_i*cos(theta))}
    
    trajectory_i[t, 1] <- pos_t[1]
    trajectory_i[t, 2] <- pos_t[2]
  }
  
  this_endpoint <- data.frame(Organism = "Neisseria gonorrhoeae", 
                              Speed = speed, 
                              x = trajectory_i[t,1], 
                              y = trajectory_i[t,2])
  twitching_distributions <- bind_rows(twitching_distributions, this_endpoint)
}

# now try for even slower
start <- c(0,0)  
speed <- 5/60 # 5 micron / min aligns with slow myxococcus

for(r in 1:nreps){
  trajectory_i <- matrix(nrow = seconds_per_day, ncol = 2, dimnames = list(NULL, c("x", "y")))
  
  theta = pi/2 # start in a single direction
  
  for(t in 1:seconds_per_day){
    
    dist_i <- runif(1, 0, speed)
    
    if(runif(1) < 0.25){
      theta <- runif(1, 0, 2*pi) # a quarter of a the time, change directions
    }
    
    if(t == 1){
      pos_t <- c(dist_i*sin(theta), dist_i*cos(theta))
    } else {pos_t <- c(trajectory_i[t-1,1] + dist_i*sin(theta),
                       trajectory_i[t-1,2] + dist_i*cos(theta))}
    
    trajectory_i[t, 1] <- pos_t[1]
    trajectory_i[t, 2] <- pos_t[2]
  }
  
  this_endpoint <- data.frame(Organism = "Myxococcus xanthus", 
                              Speed = speed, 
                              x = trajectory_i[t,1], 
                              y = trajectory_i[t,2])
  twitching_distributions <- bind_rows(twitching_distributions, this_endpoint)
}


write_csv(twitching_distributions,file = "twitching_movements.csv")
 
random_distributions %>% 
  filter(Organism %in% subset(movements, Domain == "Prokaryote")$Organism) %>% 
  bind_rows(twitching_distributions) %>% 
  mutate(dist_from_center = sqrt((x^2) + (y^2))) %>% 
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





# no back migration
# random directions: 
nreps = 1000
random_distributions_noback <- data.table()

for(i in 1:nrow(movements)){
  
  start <- c(0,0)  
  
  speed <- movements$Speed_umps[i] # per second speed
  
  for(r in 1:nreps){
    
    dists <- runif(seconds_per_day, 0, speed)
    
    
    this_endpoint <- data.frame(Organism = movements$Organism[i], 
                                Speed = movements$Speed_umps[i], 
                                x = sum(dists), 
                                y = 0) # only one dimension with no changes in orientation
    random_distributions_noback <- bind_rows(random_distributions_noback, this_endpoint)
  }
  print(paste("Done with taxon", movements$Organism[i]))
}


write_csv(random_distributions_noback, file = "random_movements_noback.csv")


# twitching directions, no backmigration: 
nreps = 1000
twitching_distributions_noback <- data.table()


start <- c(0,0)  

#speed <- movements$Speed_umps[i] # per second speed
speed <- 1 # 1 micron / sec is the typical speed here.

for(r in 1:nreps){
  
  this_endpoint <- data.frame(Organism = "Twitching motility", 
                              Speed = speed, 
                              x = sum(runif(seconds_per_day), 0, speed), 
                              y = 0)
  twitching_distributions_noback <- bind_rows(twitching_distributions_noback, this_endpoint)
}

write_csv(twitching_distributions_noback,file = "twitching_movements_noback.csv")

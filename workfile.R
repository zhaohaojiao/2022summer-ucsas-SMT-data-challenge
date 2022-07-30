player_pos <- read.csv('player_pos.csv')
ball_pos <- read.csv('ball_pos.csv')
game_events <- read.csv('game_events.csv')
game_info <- read.csv('game_info.csv')
team_info <- read.csv('team_info.csv')

library(tidyverse)

play_info <- game_events %>%
  group_by(game_str, play_id, play_per_game) %>%
  summarise(duration = max(timestamp)-min(timestamp),
            bat = (4 %in% event_code) %>% as.numeric) %>% 
  filter(duration>100 & duration<50000)

play_info$if_bat[play_info$bat==1] <- 'Yes'
play_info$if_bat[play_info$bat==0] <- 'No'

for (i in 1:nrow(play_info)){play_info$season[i]=strsplit(play_info$game_str[i], '_')[[1]][1] %>% as.numeric}

season_info <- play_info %>% 
  group_by(season) %>% 
  summarise(bat_rate=mean(bat), num_play=n(),
            num_game=length(unique(game_str)),
            `duration(ms)`=mean(duration)) %>% as.data.frame
season_info$plays_per_game <- season_info$num_play/season_info$num_game
season_info

player_info <- player_pos %>% 
  group_by(game_str, play_id) %>%
  summarise(if_pitcher = (1 %in% player_position) %>% as.numeric,
            if_batter = (10 %in% player_position) %>% as.numeric) %>% 
  filter(if_pitcher==1 & if_batter==1)

df_info <- play_info %>% merge(game_info[,c(1:3,5:9,17)]) %>% merge(player_info)

ggplot(data=play_info)+
  geom_histogram(aes(x=duration), fill='#6495ED', bins=30)+
  scale_x_log10()+
  theme_classic()+labs(x='Duration(ms)', y='Count')

ggplot(data=play_info)+
  geom_boxplot(aes(x=if_bat, y=duration, group=if_bat), varwidth=TRUE,
               outlier.size=0.1, notchwidth=0.2, fill='#6495ED')+
  scale_y_log10()+
  theme_classic()+labs(x='If bat', y='Duration(ms)')

df_info1 <- df_info[df_info$bat==1, ]
df_info2 <- df_info[df_info$bat==0, ]

ball1_pos <- data.frame()
player1_pos <- data.frame()
for (i in 1:nrow(df_info1)){
  event_tmp <- game_events %>% 
    filter(game_str==df_info1$game_str[i] & play_id==df_info1$play_id[i])
  bat_t = event_tmp$timestamp[event_tmp$event_code==4]
  
  ball_tmp <- ball_pos %>% 
    filter(game_str==df_info1$game_str[i] & play_id==df_info1$play_id[i]) %>%
    filter(timestamp<=bat_t)
  player_tmp <- player_pos %>% 
    filter(game_str==df_info1$game_str[i] & play_id==df_info1$play_id[i]) %>% 
    filter(timestamp<=bat_t)
  
  ball1_pos <- rbind(ball1_pos, ball_tmp)
  player1_pos <- rbind(player1_pos, player_tmp)
}

for (i in 1:nrow(df_info2)){
  ball_tmp <- ball_pos %>% filter(game_str==df_info2$game_str[i] & play_id==df_info2$play_id[i])
  player_tmp <- player_pos %>% filter(game_str==df_info2$game_str[i] & play_id==df_info2$play_id[i])
  
  ball1_pos <- rbind(ball1_pos, ball_tmp)
  player1_pos <- rbind(player1_pos, player_tmp)
}

ball_pitch <- ball1_pos %>% 
  group_by(game_str, play_id) %>%
  summarise(start_x = ball_position_x[which.min(timestamp)],
            start_y = ball_position_y[which.min(timestamp)],
            start_z = ball_position_z[which.min(timestamp)],
            start_t = min(timestamp),
            second_x = ball_position_x[2],
            second_y = ball_position_y[2],
            second_z = ball_position_z[2],
            second_t = timestamp[2],
            bat_t = max(timestamp))

distance3 <- function(x1,y1,z1,x2,y2,z2){
  return (sqrt((z2-z1)^2+(y2-y1)^2+(x2-x1)^2))
}
distance2 <- function(x1,y1,x2,y2){
  return (sqrt((y2-y1)^2+(x2-x1)^2))
}

ball_pitch$initial_speed <- distance3(ball_pitch$start_x, ball_pitch$start_y,　ball_pitch$start_z, 
                                      ball_pitch$second_x,　ball_pitch$second_y,　ball_pitch$second_z)/
  (ball_pitch$second_t-ball_pitch$start_t)*1000*0.3048

ball_pitch$vertical_angle <- ((ball_pitch$second_z-ball_pitch$start_z)/
                                distance3(ball_pitch$start_x,　ball_pitch$start_y,　ball_pitch$start_z, 
                                          ball_pitch$second_x,　ball_pitch$second_y,　ball_pitch$second_z)) %>% asin()
ball_pitch$vertical_angle <- ball_pitch$vertical_angle*180/pi

ball_pitch$vertical_direction[ball_pitch$vertical_angle>0] <- 'Up'
ball_pitch$vertical_direction[ball_pitch$vertical_angle<0] <- 'Down'

ball_pitch <-  ball_pitch %>% filter(initial_speed<100)

player_pitch <- player_pos %>% 
  group_by(game_str, play_id) %>%
  summarise(pitcher_x = field_x[player_position==1][which.min(timestamp)],
            pitcher_y = field_y[player_position==1][which.min(timestamp)],
            batter_x = field_x[player_position==10][which.min(timestamp)],
            batter_y = field_y[player_position==10][which.min(timestamp)])

df_info <- df_info %>% merge(player_pitch) %>% merge(ball_pitch) %>% na.omit

df_info$horizontal_diff <- (df_info$batter_x-df_info$pitcher_x)/
  distance2(df_info$pitcher_x, df_info$pitcher_y, 
            df_info$batter_x, df_info$batter_y)-
  (df_info$second_x-df_info$start_x)/
  distance2(df_info$start_x, df_info$start_y, 
            df_info$second_x, df_info$second_y)

df_info$horizontal_direction[df_info$horizontal_diff>0] <- 'Left'
df_info$horizontal_direction[df_info$horizontal_diff<0] <- 'Right'

df_info <- df_info %>% 
  filter(batter %in% unique(team_info$player_id)) %>%
  group_by(batter) %>% summarise(batter_rate=mean(bat)) %>% merge(df_info)

df_info <- df_info %>% 
  filter(pitcher %in% unique(team_info$player_id)) %>%
  group_by(pitcher) %>% summarise(pitcher_rate=mean(bat)) %>% merge(df_info)

ggplot(data=ball_pitch)+
  geom_histogram(aes(x=initial_speed), fill='#6495ED', bins=40)+
  theme_classic()+labs(x='Initial Speed (m/s)', y='Count')

glm(data=df_info, family=binomial, 
    bat~initial_speed+horizontal_direction+vertical_direction) %>% summary

glm(data=df_info, family=binomial, 
    bat~initial_speed+horizontal_direction+vertical_direction+factor(season)+factor(batter)) %>% summary

glm(data=df_info, family=binomial, 
    bat~initial_speed+horizontal_direction+vertical_direction+factor(season)+batter_rate) %>% summary

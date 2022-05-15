#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/Best-Pitch-Shape-Given-Velocity")

#Import data

data_first_15 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2015 First Half Raw.csv")
data_last_15 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2015 Second Half Raw.csv")
data_first_16 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2016 First Half Raw.csv")
data_last_16 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2016 Second Half Raw.csv")
data_first_17 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2017 First Half Raw.csv")
data_last_17 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2017 Second Half Raw.csv")
data_first_18 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2018 First Half Raw.csv")
data_last_18 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2018 Second Half Raw.csv")
data_first_19 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2019 First Half Raw.csv")
data_last_19 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2019 Second Half Raw.csv")
data_20<- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2020 Season Raw.csv")
data_first_21 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2021 First Half Raw.csv")
data_last_21 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2021 Second Half Raw.csv")

#Load libraries

library(tidyverse)
library(ggplot2)
library(scales)

#Combine data

data <- bind_rows(data_first_15, data_last_15,
                  data_first_16, data_last_16,
                  data_first_17, data_last_17,
                  data_first_18, data_last_18,
                  data_first_19, data_last_19,
                  data_20,
                  data_first_21, data_last_21)

#Select only the columns I need

reduced_columns_data <- select(data, release_speed, player_name, pitcher, events, description,
                               stand, p_throws, pfx_x, pfx_z, launch_speed, launch_angle,
                               woba_value, woba_denom, pitch_name
                               )

#Convert movement into inches instead of feet, create hand neutral horizontal column & swinging strike column

reduced_columns_data <- reduced_columns_data %>%
  mutate(h_break = pfx_x*12) %>%
  mutate(v_break = pfx_z*12) %>%
  mutate(h_break_norm = ifelse(p_throws == 'R', h_break * -1, h_break)) %>%
  mutate(swing_miss = ifelse(description == 'foul_tip' | description == 'swinging_pitchout' | description == 'swinging_strike' | description == 'swinging_strike_blocked', 1, 0))

################################################################################

#Create data frame for FB 96+ MPH and group by 3 inch diameter

FB_96 <- reduced_columns_data %>%
  filter(release_speed >= 95.5 & (pitch_name == '2-Seam Fastball' | pitch_name == '4-Seam Fastball' | pitch_name == 'Sinker')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for FB 96+ MPH visual

FB_96_binned_data <- FB_96 %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in FB 96+ MPH visual

FB_96_binned_data <- FB_96_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for FB 96+ MPH

FB_96_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(-6, 27, 3), limits = c(-6, 27)) +
  scale_y_continuous(breaks = seq(-6, 30, 3), limits = c(-6, 30)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .20), breaks = seq(.05, .20, .025)) +
  labs(title = "Swinging Strike % on FB 96+ MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for FB 93-95 MPH and group by 3 inch diameter

FB_93_95 <- reduced_columns_data %>%
  filter(release_speed >= 92.5 & release_speed < 95.5 & (pitch_name == '2-Seam Fastball' | pitch_name == '4-Seam Fastball' | pitch_name == 'Sinker')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for FB 93-95 MPH visual

FB_93_95_binned_data <- FB_93_95  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in FB 93-95 MPH visual

FB_93_95_binned_data <- FB_93_95_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for FB 93-95 MPH

FB_93_95_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(-6, 27, 3), limits = c(-6, 27)) +
  scale_y_continuous(breaks = seq(-6, 30, 3), limits = c(-6, 30)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .20), breaks = seq(.05, .20, .025)) +
  labs(title = "Swinging Strike % on FB 93-95 MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for FB 90-92 MPH and group by 3 inch diameter

FB_90_92 <- reduced_columns_data %>%
  filter(release_speed >= 89.5 & release_speed < 92.5 & (pitch_name == '2-Seam Fastball' | pitch_name == '4-Seam Fastball' | pitch_name == 'Sinker')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for FB 90-92 MPH visual

FB_90_92_binned_data <- FB_90_92  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in FB 90-92 MPH visual

FB_90_92_binned_data <- FB_90_92_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for FB 90-92 MPH

FB_90_92_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(-6, 27, 3), limits = c(-6, 27)) +
  scale_y_continuous(breaks = seq(-6, 30, 3), limits = c(-6, 30)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .20), breaks = seq(.05, .20, .025)) +
  labs(title = "Swinging Strike % on FB 90-92 MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for 90+ MPH breaking balls and group by 3 inch diameter

BB_90 <- reduced_columns_data %>%
  filter(release_speed >= 89.5 & (pitch_name == 'Cutter' | pitch_name == 'Slider' | pitch_name == 'Curveball' | pitch_name == 'Knuckle Curve' | pitch_name == 'Eephus')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for 90+ MPH breaking balls visual

BB_90_binned_data <- BB_90  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in 90+ MPH breaking balls visual

BB_90_binned_data <- BB_90_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for 90+ MPH breaking balls

BB_90_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(-24, 12, 3), limits = c(-24, 12)) +
  scale_y_continuous(breaks = seq(-24, 24, 3), limits = c(-24, 24)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .25), breaks = seq(.05, .25, .05)) +
  labs(title = "Swinging Strike % on Breaking Balls 90+ MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for 85-89 MPH breaking balls and group by 3 inch diameter

BB_85_89 <- reduced_columns_data %>%
  filter(release_speed >= 84.5 & release_speed < 89.5 & (pitch_name == 'Cutter' | pitch_name == 'Slider' | pitch_name == 'Curveball' | pitch_name == 'Knuckle Curve' | pitch_name == 'Eephus')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for 85-89 MPH breaking balls visual

BB_85_89_binned_data <- BB_85_89  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in 85-89 MPH breaking balls visual

BB_85_89_binned_data <- BB_85_89_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for 85-89 MPH breaking balls

BB_85_89_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(-24, 12, 3), limits = c(-24, 12)) +
  scale_y_continuous(breaks = seq(-24, 24, 3), limits = c(-24, 24)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .25), breaks = seq(.05, .25, .05)) +
  labs(title = "Swinging Strike % on Breaking Balls 85-89 MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for 80-84 MPH breaking balls and group by 3 inch diameter

BB_80_84 <- reduced_columns_data %>%
  filter(release_speed >= 79.5 & release_speed < 84.5 & (pitch_name == 'Cutter' | pitch_name == 'Slider' | pitch_name == 'Curveball' | pitch_name == 'Knuckle Curve' | pitch_name == 'Eephus')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for 80-84 MPH breaking balls visual

BB_80_84_binned_data <- BB_80_84  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in 80-84 MPH breaking balls visual

BB_80_84_binned_data <- BB_80_84_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for 80-84 MPH breaking balls

BB_80_84_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(-24, 12, 3), limits = c(-24, 12)) +
  scale_y_continuous(breaks = seq(-24, 24, 3), limits = c(-24, 24)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .25), breaks = seq(.05, .25, .05)) +
  labs(title = "Swinging Strike % on Breaking Balls 80-84 MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for 75-79 MPH breaking balls and group by 3 inch diameter

BB_75_79 <- reduced_columns_data %>%
  filter(release_speed >= 74.5 & release_speed < 79.5 & (pitch_name == 'Cutter' | pitch_name == 'Slider' | pitch_name == 'Curveball' | pitch_name == 'Knuckle Curve' | pitch_name == 'Eephus')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for 75-79 MPH breaking balls visual

BB_75_79_binned_data <- BB_75_79  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in 75-79 MPH breaking balls visual

BB_75_79_binned_data <- BB_75_79_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for 75-79 MPH breaking balls

BB_75_79_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(-24, 12, 3), limits = c(-24, 12)) +
  scale_y_continuous(breaks = seq(-24, 24, 3), limits = c(-24, 24)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .25), breaks = seq(.05, .25, .05)) +
  labs(title = "Swinging Strike % on Breaking Balls 75-79 MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for 70-74 MPH breaking balls and group by 3 inch diameter

BB_70_74 <- reduced_columns_data %>%
  filter(release_speed >= 69.5 & release_speed < 74.5 & (pitch_name == 'Cutter' | pitch_name == 'Slider' | pitch_name == 'Curveball' | pitch_name == 'Knuckle Curve' | pitch_name == 'Eephus')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for 70-74 MPH breaking balls visual

BB_70_74_binned_data <- BB_70_74  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in 70-74 MPH breaking balls visual

BB_70_74_binned_data <- BB_70_74_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for 70-74 MPH breaking balls

BB_70_74_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(-24, 12, 3), limits = c(-24, 12)) +
  scale_y_continuous(breaks = seq(-24, 24, 3), limits = c(-24, 24)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .25), breaks = seq(.05, .25, .05)) +
  labs(title = "Swinging Strike % on Breaking Balls 70-74 MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for 90+ MPH changeups and group by 3 inch diameter

CH_90 <- reduced_columns_data %>%
  filter(release_speed >= 89.5 & (pitch_name == 'Changeup' | pitch_name == 'Split-Finger' | pitch_name == 'Forkball' | pitch_name == 'Screwball')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for 90+ MPH changeups visual

CH_90_binned_data <- CH_90  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in 90+ MPH changeups visual

CH_90_binned_data <- CH_90_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for 90+ MPH changeups

CH_90_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  scale_y_continuous(breaks = seq(-6, 24, 3), limits = c(-6, 24)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .25), breaks = seq(.05, .25, .05)) +
  labs(title = "Swinging Strike % on Changeups 90+ MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for 85-89 MPH changeups and group by 3 inch diameter

CH_85_89 <- reduced_columns_data %>%
  filter(release_speed >= 84.5 & release_speed < 89.5 & (pitch_name == 'Changeup' | pitch_name == 'Split-Finger' | pitch_name == 'Forkball' | pitch_name == 'Screwball')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for 85-89 MPH changeups visual

CH_85_89_binned_data <- CH_85_89  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in 85-89 MPH changeups visual

CH_85_89_binned_data <- CH_85_89_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for 85-89 MPH changeups

CH_85_89_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  scale_y_continuous(breaks = seq(-6, 24, 3), limits = c(-6, 24)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .25), breaks = seq(.05, .25, .05)) +
  labs(title = "Swinging Strike % on Changeups 85-89 MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for 80-84 MPH changeups and group by 3 inch diameter

CH_80_84 <- reduced_columns_data %>%
  filter(release_speed >= 79.5 & release_speed < 84.5 & (pitch_name == 'Changeup' | pitch_name == 'Split-Finger' | pitch_name == 'Forkball' | pitch_name == 'Screwball')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for 80-84 MPH changeups visual

CH_80_84_binned_data <- CH_80_84  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in 80-84 MPH changeups visual

CH_80_84_binned_data <- CH_80_84_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for 80-84 MPH changeups

CH_80_84_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  scale_y_continuous(breaks = seq(-6, 24, 3), limits = c(-6, 24)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .25), breaks = seq(.05, .25, .05)) +
  labs(title = "Swinging Strike % on Changeups 80-84 MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

#################################################################################

#Create data frame for 75-79 MPH changeups and group by 3 inch diameter

CH_75_79 <- reduced_columns_data %>%
  filter(release_speed >= 74.5 & release_speed < 79.5 & (pitch_name == 'Changeup' | pitch_name == 'Split-Finger' | pitch_name == 'Forkball' | pitch_name == 'Screwball')) %>%
  drop_na(h_break_norm) %>%
  drop_na(v_break) %>%
  mutate(v_break_bin = round(v_break / 3, 0)) %>%
  mutate(h_break_norm_bin = round(h_break_norm / 3, 0))

#Summarize into bins for 80-84 MPH changeups visual

CH_75_79_binned_data <- CH_75_79  %>%
  group_by(v_break_bin, h_break_norm_bin) %>%
  summarise(Swinging_Strikes = sum(swing_miss),
            Pitches = n(),
            SS_Pct = sum(swing_miss) / n()) %>%
  filter(Pitches >= 500) %>%
  ungroup()

#Create columns for x and y values in 75-79 MPH changeups visual

CH_75_79_binned_data <- CH_75_79_binned_data %>%
  mutate(x = h_break_norm_bin * 3) %>%
  mutate(y = v_break_bin * 3)

#Create visual for 75-79 MPH changeups

CH_75_79_binned_data %>%
  ggplot(aes(x = x, y = y, fill = SS_Pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::percent(SS_Pct, 0.1)), size = 3) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  scale_y_continuous(breaks = seq(-6, 24, 3), limits = c(-6, 24)) +
  scale_fill_distiller(palette = 'RdBu', name = "Swinging Strike %", labels = scales::percent, limits = c(.05, .25), breaks = seq(.05, .25, .05)) +
  labs(title = "Swinging Strike % on Changeups 75-79 MPH",
       x = "Horizontal Break (In)",
       y = "Vertical Break (In)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
## ----warning=FALSE, message=FALSE---------------------------------------
library(dplyr)
library("nycflights13")
library(tidyverse)
library(knitr)




## -----------------------------------------------------------------------
flights_delayed <- flights %>%
  filter(arr_delay >= 120)
flights_delayed



## -----------------------------------------------------------------------

iah_hou <- flights %>%
  filter (dest == "IAH"|dest == "HOU") %>%
  head(10)


## -----------------------------------------------------------------------
flights_airlines <- flights %>%
  filter(carrier %in% c("UA", "AA", "DL"))


## -----------------------------------------------------------------------
flights_dept_winter <- flights %>%
  filter(month %in% c("7", "8", "9"))


## -----------------------------------------------------------------------
flights_arr_late_dep_on_time <- flights %>%
  filter(arr_delay > 120, dep_delay <= 0)


## -----------------------------------------------------------------------

flights %>%
  filter (is.na(dep_time)) %>%
  nrow()

summarise(flights, across(everything(), ~sum(is.na(.))))


## -----------------------------------------------------------------------
flights_missing_variables <- flights %>% 
  filter(is.na(dep_time)) %>% 
  summarise(missing_arr_time = sum(is.na(arr_time)),
            missing_dep_delay = sum(is.na(dep_delay)),
            missing_arr_delay = sum(is.na(arr_delay)),
            missing_air_time = sum(is.na(air_time)))


## -----------------------------------------------------------------------
most_delayed_flights <- flights %>%
  arrange(desc(dep_delay))
  
earliest_flight_departure <- flights %>%
  arrange(dep_delay)


## -----------------------------------------------------------------------
shortest_flight <- flights %>%
  arrange(distance)
farthest_flight <- flights %>%
  arrange(desc(distance))


## -----------------------------------------------------------------------
Multiple_var_flights <- flights %>%
  select(arr_delay, arr_delay, arr_delay)


## -----------------------------------------------------------------------
flights_minutes_after_midnight <- flights %>% 
  mutate(dep_minutes = (dep_time %/% 100) * 60 + (dep_time %% 100),
sched_dep_minutes = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100))


## -----------------------------------------------------------------------
arr_dep_minutes_midnight <- flights %>%
  mutate(arr_time = (arr_time %/% 100) * 60 + (arr_time %% 100),
         dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
         sched_airtime = (arr_time - dep_time),
         sched_airtime = if_else(sched_airtime < 0, sched_airtime + 1440,
                                 sched_airtime),
         diff = air_time - sched_airtime) %>%
  summarise(mean_diff = mean(diff, na.rm = TRUE),
            sd_diff = sd(diff, na.rm = TRUE),
            min_diff = min(diff, na.rm = TRUE),
            max_diff = max(diff, na.rm = TRUE))


## -----------------------------------------------------------------------
cancelled_per_season <- flights %>%
  filter(is.na(dep_time)) %>%
  mutate(season = case_when(
    month %in% c(3, 4, 5) ~ "Spring",
    month %in% c(6, 7, 8) ~ "Summer",
    month %in% c(9, 10, 11) ~ "Fall",
    month %in% c(12, 1, 2) ~ "Winter"
  )) %>% 
  group_by(season, year, month, day) %>%
  summarise(n_cancelled_day = n(),
             .groups = "drop") %>%
  group_by(season) %>%
  summarise(total_cancelled = sum(n_cancelled_day),
            avg_cancelled_per_day = mean(n_cancelled_day),
            .groups = "drop")
           


## -----------------------------------------------------------------------
worst_on_time_record <- flights %>%
  filter(!is.na(tailnum)) %>% 
  group_by(tailnum) %>%
  summarise(
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    n_flights = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_dep_delay)) %>%
  slice_head(n = 1)
  



## -----------------------------------------------------------------------
avg_delay_by_time_of_day <- flights %>%
  mutate(dep_hour = sched_dep_time %/% 100) %>% 
  group_by(dep_hour) %>% 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE),
            avg_dep_delay = mean(dep_delay, na.rm = TRUE),
            n = n()) %>%
  arrange(avg_arr_delay)


## -----------------------------------------------------------------------
total_dest_delay_prop_delay <- flights %>% 
  select(dest, arr_delay) %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(dest) %>% 
  mutate(total_delay_dest = sum(arr_delay, na.rm = TRUE)) %>%
  mutate(prop_delay = arr_delay / total_delay_dest) %>% 
  arrange(dest, desc(prop_delay)) %>%
  ungroup()


## -----------------------------------------------------------------------
total_dest_delay <- flights %>%
  group_by(dest) %>%
  summarise(total_dest_delay = sum(arr_delay, na.rm = TRUE),
            avg_delay = mean(arr_delay, na.rm = TRUE),
            n_flights = n()) %>%
  arrange(desc(total_dest_delay))


## -----------------------------------------------------------------------
fastest_flights <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  mutate(min_air_time = min(air_time, na.rm = TRUE),
         air_time_ratio = air_time / min_air_time) %>%
  arrange(dest, air_time_ratio) %>%
  ungroup() %>%
  filter(air_time_ratio > 0.5) %>%
  select(
    carrier, tailnum, flight, origin, dest, air_time, min_air_time, air_time_ratio)

fastest_flights %>%
  arrange(air_time_ratio) %>%
  slice_head(n=5)



## ----warning=FALSE, message=FALSE, echo=FALSE---------------------------
most_delayed_airtime_Top5 <- flights %>%
  group_by(dest) %>%
  mutate(min_air_time = min(air_time, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(delay_in_air = air_time - min_air_time) %>%
  arrange(desc(delay_in_air)) %>%
  select(carrier, tailnum, flight, origin, dest, air_time, min_air_time,
         delay_in_air) %>%
  slice_head(n = 5)


## ----warning=FALSE, message=FALSE, echo=FALSE---------------------------
library(ggplot2)
s_fit <- read.csv("row_data.csv")

social_fit_plot <- ggplot(s_fit, aes(x = energy_level, y = frequency, label = name)) + 
  geom_point(colour = "skyblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", linewidth = 0.5) +
  geom_vline(xintercept = 0, colour = "darkgreen", linetype = "dashed", 
             linewidth = 0.5) +
   geom_hline(yintercept = 8, colour = "darkgreen", linetype = "dashed", 
             linewidth = 0.5) +
  geom_text(nudge_x = 0.2, size = 4, hjust = 0) +
  scale_x_continuous(limits = c(-5, 6)) +
  scale_y_continuous(limits = c(0, 16)) +
  labs(
  title = "Social Relationship Map: How energising or depleting some 
       relationships are",
    x = "Energy Level (Depleting -> Energising)",
    y = "Frequency (times seen/met per month)")+ 
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14))


social_fit_plot 
ggsave("Social_Relationship_Map.png", social_fit_plot, width = 9.5, height = 7, dpi = 300)


## -----------------------------------------------------------------------
s_fit <- s_fit[order(s_fit$energy_level), ]


## -----------------------------------------------------------------------
most_energising_top5 <- s_fit %>%
  arrange(desc(energy_level)) %>%
 head(5)
print(most_energising_top5)


## -----------------------------------------------------------------------
avg_freq_of_depleting <- s_fit %>%
  filter(energy_level < 0) %>%
  summarise(avg_frequency = mean(frequency, na.rm = TRUE))
print(avg_freq_of_depleting)


## -----------------------------------------------------------------------
avg_freq_of_energising <- s_fit %>%
  filter(energy_level > 0) %>%
  summarise(avg_frequency = mean(frequency, na.rm = TRUE))
print(avg_freq_of_energising)


## -----------------------------------------------------------------------
relationship_type <- s_fit %>%
  mutate(relationship_type = case_when(
    energy_level < 0 ~ "Depleting",
    energy_level == 0 ~ "Neutral",
    energy_level > 0 ~ "Energising"
  ))


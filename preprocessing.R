library(ggplot2)
library(dplyr)
library(rlang)
library(tidyr)
library(hms)
library(ggrepel)
#library(ggthemes)
library(pals)

data <- read.csv("./kolejkeR_data.csv", fileEncoding = "utf-8")

head(data)


dates <- data %>% select(date) %>% group_by(date) %>% summarise(n())

data[['date_time_posix']]<- as.POSIXct(strptime(paste(data[['time']], data[['date']]), format="%H:%M %Y-%m-%d", tz="Europe/Warsaw"))
data[['time_posix']] <- as.POSIXct(strptime(data[['time']], format="%H:%M", tz="Europe/Warsaw"))

parse_served_number <- function(queuer_token_col) {
  as.numeric(gsub("[[:alpha:]](\\d*)", "\\1", queuer_token_col, perl=TRUE))
}

data_with_queuers_count <- data %>%
  mutate(served_people=replace_na(parse_served_number(aktualnyNumer),0)) %>%
  mutate(week_day = factor(weekdays(date_time_posix), 
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
                           #levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek")
                          ))  

open_queues_preprocessed_data <- data_with_queuers_count %>% filter(liczbaCzynnychStan > 0 & status == 1 )


# Avg nr of served people per day
open_queues_preprocessed_data %>% 
  group_by(name, date) %>% 
  summarise(all_served_people = max(served_people)) %>% 
  group_by(name) %>% 
  summarise(`Mean daily nr of served people` = mean(all_served_people)) %>% 
  arrange(desc(`Mean daily nr of served people`)) %>%
  mutate(Office=reorder(name, `Mean daily nr of served people`)) %>% 
  ggplot(aes(x = Office, y = `Mean daily nr of served people`)) +
  geom_col() + coord_flip() + theme_bw()
  
# Avg nr of people in queue per day
open_queues_preprocessed_data %>% 
  group_by(name, date) %>% 
  summarise(avg_queue_len = mean(liczbaKlwKolejce)) %>% 
  group_by(name) %>% 
  summarise(`Mean daily queue length` = mean(avg_queue_len)) %>% 
  #summarise(avg_queue_len_per_day = mean(liczbaKlwKolejce)) %>% 
  mutate(Office=reorder(name, `Mean daily queue length`)) %>% 
  ggplot(aes(x = Office, y = `Mean daily queue length`)) +
  geom_col() + coord_flip() + theme_bw()


common_week_data <- data_with_queuers_count %>% 
  filter(date_time_posix > as.POSIXct(strptime("2020-01-07", format="%Y-%m-%d", tz="Europe/Warsaw")) &
          date_time_posix < as.POSIXct(strptime("2020-01-20", format="%Y-%m-%d", tz="Europe/Warsaw"))) 

common_week_data %>% 
  filter(liczbaCzynnychStan > 0) %>%  # filters out days that offices doesn't work
  mutate(`Day of weeek` = week_day) %>% 
  group_by(`Day of weeek`) %>% 
  summarise(`Mean queue length` = mean(liczbaKlwKolejce)) %>% 
  ggplot(aes(x = `Day of weeek`, y = `Mean queue length`)) +
  geom_col() +
  theme_bw() +
  theme( axis.text.x = element_text(angle=45, hjust = 1))


jitter_served_people <- data_with_queuers_count %>% 
  filter(liczbaCzynnychStan > 0 & status == 1 ) %>% # filters out days that offices doesn't work
  group_by(name, nazwaGrupy, date) %>% 
  summarise(all_served_people = max(served_people)) %>% 
  group_by(name, nazwaGrupy) %>% 
  summarise(`Mean daily nr of served people` = mean(all_served_people)) %>% 
  mutate(Office = name)

pos = position_jitter(width = 0.5, seed = 1)
ggplot(jitter_served_people, aes(x = Office, y = `Mean daily nr of served people`, color=name)) +
  geom_point(position = pos ) +
  geom_text_repel(
    aes(label=ifelse(`Mean daily nr of served people` > 250, as.character(nazwaGrupy), '')),
    box.padding   = 0.35, 
    point.padding = 0.5,
    segment.color = 'grey50',
    position = pos ) +
  theme_bw() +
  scale_colour_manual(values = cols25(n=21)) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        legend.position = "none")

# Avg nr of people in queue per day
jitter_queue_len <- data_with_queuers_count %>% 
  filter(liczbaCzynnychStan != 0) %>% # filters out days that offices doesn't work
  group_by(name, nazwaGrupy, date) %>% 
  summarise(all_queued_people = mean(liczbaKlwKolejce)) %>% 
  group_by(name, nazwaGrupy) %>% 
  summarise(`Mean queue length` = mean(all_queued_people))

pos = position_jitter(width = 0.5, seed = 2)
ggplot(jitter_queue_len, aes(x = name, y = `Mean queue length`, color=name)) +
  geom_point(position = pos ) +
  geom_text_repel(
    aes(label=ifelse(`Mean queue length` > 7, as.character(nazwaGrupy), '')),
    box.padding   = 0.35, 
    point.padding = 0.5,
    segment.color = 'grey50',
    position = pos ) +
  theme_bw() +
  scale_colour_manual(values = cols25(n=21)) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        legend.position = "none")

office_queues_ids %>% mutate(`Mean queue length` = mean_queuers, `Served people per day` = served, Office = name) %>% 
  ggplot(aes(x = `Served people per day`, y = `Mean queue length`, color = Office)) +
  geom_point() +
  geom_text_repel(
    aes(label=ifelse(mean_queuers > 50, as.character(nazwaGrupy), '')),
    box.padding   = 0.35, 
    point.padding = 0.5,
    segment.color = 'grey50',
    position = pos )+
  theme_bw() +
  scale_colour_manual(values = cols25(n=21))

wrap_plot_queues <- function(office_data, y_var, title_text, subtitle_text) {
  office_data %>% ggplot(aes(x=Time, y=!!enquo(y_var), color=date)) +
    geom_point() +
    scale_x_datetime() +
    scale_colour_ordinal() +
    theme_bw() +
    ggtitle(label = title_text, subtitle = subtitle_text)
}


office_queue_data_prepare <- function (data, office_name, queue_id) {
   data %>% 
    filter(idGrupy == queue_id & 
             name == office_name & 
             liczbaCzynnychStan != 0) %>% 
    mutate(Time = time_posix,
           `Served people` = served_people,
           `Queue length` = liczbaKlwKolejce)
}

mokotow_658_queue <- office_queue_data_prepare(common_week_data, "UD_Mokotow_1", "658")
#UD_Praga_Poludnie	Rejestracja pojazdów - składanie wniosków	165	2020-01-08	127	167	29.779528
praga_poludnie_165_queue <- office_queue_data_prepare(common_week_data, "UD_Praga_Poludnie", "165")
# Graph showing how nr of served people grows for different time slots and dates
wrap_plot_queues(mokotow_658_queue, 
                 `Served people`, 
                 mokotow_658_queue[1, "name"],
                 mokotow_658_queue[1, "nazwaGrupy"])

# Graph showing how nr of queued people changes for different time slots and dates
wrap_plot_queues(mokotow_658_queue, 
                 `Queue length`, 
                 mokotow_658_queue[1, "name"],
                 mokotow_658_queue[1, "nazwaGrupy"])

wrap_plot_queues(praga_poludnie_165_queue, 
                 `Served people`, 
                 praga_poludnie_165_queue[1, "name"],
                 praga_poludnie_165_queue[1, "nazwaGrupy"])

# Graph showing how nr of queued people changes for different time slots and dates
wrap_plot_queues(praga_poludnie_165_queue, 
                 `Queue length`, 
                 praga_poludnie_165_queue[1, "name"],
                 praga_poludnie_165_queue[1, "nazwaGrupy"])

mokotow_10_00 <- data_with_queuers_count %>%
  filter(name == "UD_Mokotow_1" & time == "10:00")

mokotow_10_00 

queues_for_12_00_grp <- data_with_queuers_count %>% filter(time == "12:00" & liczbaCzynnychStan > 0) %>%  
  pivot_longer(cols=c(liczbaCzynnychStan, liczbaKlwKolejce), names_to = "Type", values_to= "Count") %>% 
  mutate(Count = ifelse(Type == "liczbaCzynnychStan", -Count, Count)) %>% 
  group_by(name, nazwaGrupy)

queues_for_12_00_grp %>% filter(name == "UD_Mokotow_1") %>%  
  ggplot(aes(x = nazwaGrupy, y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = .6) +
  coord_flip()




plot_wait_time_per_timeslot <- function(queue_data, office_name, queue_id, date_str) {
  
  mokotow_queue_with_served_number <- queue_data %>% filter(name == office_name)
  
  data_with_slot_nr <- mokotow_queue_with_served_number %>%
    group_by(date, idGrupy ) %>%
    arrange(time) %>%
    mutate(slot_nr=row_number(), next_slot_nr=row_number()+1) %>%
    ungroup()
  
  mokotow_x_with_slot_nr <- data_with_slot_nr %>%  
    filter(idGrupy  == queue_id & date == date_str & liczbaCzynnychStan > 0)
  
  if (all(mokotow_x_with_slot_nr[["liczbaKlwKolejce"]] == 0)) 
    stop("Error no people in the queue!")
  
  if (all(mokotow_x_with_slot_nr$served_people == 0)) 
    stop("Error no people served in the queue!")
  
  mokotow_x_with_slot_time_limits <- mokotow_x_with_slot_nr %>%
    inner_join(data_with_slot_nr, 
               data_with_slot_nr, 
               by=c("next_slot_nr"="slot_nr", "date"="date", "literaGrupy"="literaGrupy")) %>% 
    mutate(slot_visitor_number_start = liczbaKlwKolejce.x + served_people.x,
           slot_visitor_number_end = liczbaKlwKolejce.y + served_people.y) %>% 
    select(time.x, time.y, slot_nr, next_slot_nr, slot_visitor_number_start,
           slot_visitor_number_end, served_people.x, served_people.y, liczbaKlwKolejce.x,
           liczbaKlwKolejce.y, time_posix.x, time_posix.y, liczbaCzynnychStan.x) %>% 
    rename(slot_start = time.x,
           slot_end = time.y,
           slot = slot_nr,
           next_slot = next_slot_nr,
           start_last_queuer_nr = slot_visitor_number_start,
           end_last_queuer_nr = slot_visitor_number_end,
           start_served_nr = served_people.x, 
           end_served_nr = served_people.y,
           start_queue_length = liczbaKlwKolejce.x, 
           end_queue_length = liczbaKlwKolejce.y,
           start_time = time_posix.x,
           end_time = time_posix.y, 
           active_lines_count = liczbaCzynnychStan.x
           )
  #browser()
  
  mokotow_x_self_cross_joined <- mokotow_x_with_slot_time_limits %>%
    mutate(dummy = TRUE) %>%
    inner_join(mokotow_x_with_slot_time_limits %>% mutate(dummy = TRUE),
               by = c("dummy" = "dummy")) %>% 
    select(-dummy) 
  
  
  # .x
  #        end_last_queuer_nr
  #came    |            |          |
  #     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  #served|    |                       |      
  #           end_served_nr            
  
  
  # .y
  #
  #came    |            |          |
  #     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  #served|    |                       |      
  
  # y - serverd
  # x - queued
  
  # any person that enqueued in given slot.x match with slot.y that served any of people enqueued in that slot
  # start_qued_person < serving_person_start & serving_person_end < end_qued_person |
  # serving_person_start < end_qued_person < serving_person_end | serving_person_start < start_qued_person < serving_person_end
  
  mokotow_x_paired_served_enqueued_slots <- mokotow_x_self_cross_joined %>% filter(
    start_last_queuer_nr.x < start_served_nr.y & end_served_nr.y < end_last_queuer_nr.x |
      start_served_nr.y <= end_last_queuer_nr.x & end_last_queuer_nr.x < end_served_nr.y |
      start_served_nr.y < start_last_queuer_nr.x & start_last_queuer_nr.x <= end_served_nr.y
    ) 
  
  mokotow_x_slots_wait_time <- mokotow_x_paired_served_enqueued_slots %>%
    filter(start_last_queuer_nr.x < end_last_queuer_nr.x) %>% 
    mutate(wait_time = as_hms(pmax(start_time.y + (end_time.y - start_time.y)/2 - (start_time.x + (end_time.x - start_time.x)/2), 0)),
           time_slot = as_hms(start_time.x + (end_time.x - start_time.x)/2),
           slot_start = as_hms(start_time.x),
           width = end_time.x - start_time.x)
    
  
  mokotow_x_avg_wait_time <- mokotow_x_slots_wait_time %>% 
    group_by(time_slot, width) %>%
    summarise(
      avg_wait_time=as_hms(mean(wait_time))
    )
  
  slot_even = as.logical(1:nrow(mokotow_x_avg_wait_time)%%2)
  
  # Plot showing how long on average people had to wait until getting served
  mokotow_x_avg_wait_time %>% 
    mutate(`Time` = time_slot,
           `Mean waiting time` = avg_wait_time,
           `Time slot` = as.numeric(width, "secs")) %>% 
    ggplot(aes(x = `Time`, 
               y = `Mean waiting time`,
               width =`Time slot`, #as.numeric(mokotow_x_avg_wait_time$width, "secs"),
               fill=`Time slot`)) + #as.numeric(mokotow_x_avg_wait_time$width, "secs") )) +
    geom_bar(stat = "identity" ) +
    #geom_point() +
    ggtitle(label = paste(mokotow_x_with_slot_nr[[1, "name"]], mokotow_x_with_slot_nr[[1, "date"]]),
            subtitle = mokotow_x_with_slot_nr[[1, "nazwaGrupy"]]) +
    theme_bw()
}



office_queues_ids <- open_queues_preprocessed_data %>%
  group_by(name, nazwaGrupy, idGrupy, date) %>%
  summarise(n = n(), served = max(served_people), mean_queuers = mean(liczbaKlwKolejce)) %>% 
  arrange(desc(mean_queuers))

common_office_queues_ids <- common_week_data %>%
  group_by(name, nazwaGrupy, idGrupy, date) %>%
  summarise(n = n(), served = max(served_people), mean_queuers = mean(liczbaKlwKolejce)) %>%
  arrange(desc(mean_queuers))

#View(common_office_queues_ids)
View(office_queues_ids %>% arrange(desc(mean_queuers)))

plot_wait_time_per_timeslot(data_with_queuers_count, "UD_Bemowo", "551", "2020-01-13")
plot_wait_time_per_timeslot(data_with_queuers_count, "UD_Bielany", "171", "2020-01-13")
#UD_Wola	Meldunki i dowody	182	2020-01-15	129	101	12.496124
plot_wait_time_per_timeslot(data_with_queuers_count, "UD_Wola", "182", "2020-01-15")

plot_wait_time_per_timeslot(data_with_queuers_count, "UD_Ochota", "599", "2020-12-30")
plot_wait_time_per_timeslot(data_with_queuers_count, "UD_Mokotow_1", "658", "2020-12-30")

common_week_data %>% filter(name == "UD_Bemowo" & literaGrupy == 'X')

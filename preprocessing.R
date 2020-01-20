library(ggplot2)
library(dplyr)
library(rlang)
library(tidyr)
library(hms)
library(ggrepel)

data <- read.csv("./kolejkeR_data.csv", fileEncoding = "utf-8")

head(data)

data %>% select(time) %>% unique()
dates <- data %>% select(date) %>% group_by(date) %>% summarise(n())
data %>% filter(date == "2020-12-30" | date == "2020-12-31") %>% select(name) %>% unique()
data %>% filter(date == "2019-12-30" | date == "2019-12-31") %>% select(name) %>% unique()
data %>% select(name) %>% unique()
data %>% filter(liczbaCzynnychStan > 0) %>%  select(nazwaGrupy) %>% unique() %>% count()

data[['date_time_posix']]<- as.POSIXct(strptime(paste(data[['time']], data[['date']]), format="%H:%M %Y-%m-%d", tz="Europe/Warsaw"))
data[['time_posix']] <- as.POSIXct(strptime(data[['time']], format="%H:%M", tz="Europe/Warsaw"))
parse_served_number <- function(queuer_token_col) {
  as.numeric(gsub("[[:alpha:]](\\d*)", "\\1", queuer_token_col, perl=TRUE))
}
data_with_queuers_count <- data %>%  mutate(served_people=replace_na(parse_served_number(aktualnyNumer),0))


data_with_queuers_count %>% filter(name == "UD_Srodmiescie_2")

# Avg nr of served people per day
data_with_queuers_count %>% 
  filter(liczbaCzynnychStan != 0) %>% # filters out days that offices doesn't work
  group_by(name, date) %>% 
  summarise(all_served_people = max(served_people)) %>% 
  group_by(name) %>% 
  summarise(avg_served_per_day = mean(all_served_people)) %>% 
  arrange(desc(avg_served_per_day)) %>%
  mutate(kolejka=reorder(name, avg_served_per_day)) %>% 
  ggplot(aes(x = kolejka, y = avg_served_per_day)) +
  geom_col() + coord_flip()
  
# Avg nr of people in queue per day
data_with_queuers_count %>% 
  filter(liczbaCzynnychStan != 0) %>% # filters out days that offices doesn't work
  group_by(name, date) %>% 
  summarise(avg_queuers = mean(liczbaKlwKolejce)) %>% 
  group_by(name) %>% 
  summarise(avg_queuers_per_day = mean(avg_queuers)) %>% 
  arrange(desc(avg_queuers_per_day)) %>% mutate(kolejka=reorder(name, avg_queuers_per_day)) %>% 
  ggplot(aes(x = kolejka, y = avg_queuers_per_day)) +
  geom_col() + coord_flip() + theme_bw()


# Avg nr of served people per day
data_with_queuers_count %>% 
  filter(liczbaCzynnychStan != 0) %>% # filters out days that offices doesn't work
  group_by(name, nazwaGrupy, date) %>% 
  summarise(all_served_people = max(served_people)) %>% 
  group_by(name, nazwaGrupy) %>% 
  summarise(avg_served_per_day = mean(all_served_people)) %>% 
  ggplot(aes(x = name, y = avg_served_per_day)) +
  geom_point()
    
# Avg nr of people in queue per day
jitter_report <- data_with_queuers_count %>% 
  filter(liczbaCzynnychStan != 0) %>% # filters out days that offices doesn't work
  group_by(name, nazwaGrupy, date) %>% 
  summarise(all_queued_people = mean(liczbaKlwKolejce)) %>% 
  group_by(name, nazwaGrupy) %>% 
  summarise(avg_queue_len = mean(all_queued_people))
  
pos = position_jitter(width = 0.5, seed = 1)
ggplot(jitter_report, aes(x = name, y = avg_queue_len, color=avg_queue_len>7)) +
  geom_point(position = pos ) +
  geom_label_repel(
    aes(name,avg_queue_len,label=ifelse(avg_queue_len > 7, as.character(nazwaGrupy), '')),
    box.padding   = 0.35, 
    point.padding = 0.5,
    segment.color = 'grey50',
    position = pos )


queueu_names <- data_with_queuers_count %>% 
  filter(liczbaCzynnychStan != 0) %>% # filters out days that offices doesn't work
  group_by(name, nazwaGrupy, date) %>% 
  summarise(all_queued_people = mean(liczbaKlwKolejce)) %>% group_by(nazwaGrupy) %>% count()


data_with_queuers_count %>% filter(name == "UD_Mokotow_1") %>% select(nazwaGrupy) %>% unique()

wrap_plot_queues <- function(office_data, y_var, title_text, subtitle_text) {
  office_data %>% ggplot(aes(x=time_posix, y=!!enquo(y_var), color=date)) +
    geom_point() +
    scale_x_datetime() +
    ggtitle(label = title_text, subtitle = subtitle_text)
}




mokotow_x_queue <- data_with_queuers_count %>% filter(literaGrupy == "X" & name == "UD_Mokotow_1")
# Graph showing how nr of served people grows for different time slots and dates
wrap_plot_queues(mokotow_x_queue, 
                 served_people, 
                 mokotow_x_queue[1, "name"],
                 mokotow_x_queue[1, "nazwaGrupy"])

# Graph showing how nr of queued people changes for different time slots and dates
wrap_plot_queues(mokotow_x_queue, 
                 liczbaKlwKolejce, 
                 mokotow_x_queue[1, "name"],
                 mokotow_x_queue[1, "nazwaGrupy"])
mokotow_queue_with_served_number <- data_with_queuers_count %>% filter(name == "UD_Mokotow_1")
data_with_slot_nr <- mokotow_queue_with_served_number %>%
  group_by(date, literaGrupy) %>%
  arrange(time) %>%
  mutate(slot_nr=row_number(), next_slot_nr=row_number()+1) %>%
  ungroup()

mokotow_x_with_slot_nr <- data_with_slot_nr %>%  
  filter(literaGrupy == "X" & date == "2020-12-30" & liczbaCzynnychStan != 0)


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

mokotow_x_slots_wait_time <- mokotow_x_paired_served_enqueued_slots %>% filter(start_last_queuer_nr.x < end_last_queuer_nr.x) %>% 
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
  ggplot(aes(x = time_slot, 
             y = avg_wait_time,
             width =as.numeric(mokotow_x_avg_wait_time$width, "secs"),
             fill=slot_even )) +
  geom_bar(stat = "identity" ) +
  ggtitle(label = mokotow_x_queue[1, "name"],
          subtitle = mokotow_x_queue[1, "nazwaGrupy"])
                                         



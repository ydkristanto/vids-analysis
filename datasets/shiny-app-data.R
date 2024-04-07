# Load packages
library(tidyverse)

# Load vid_analytics.RData from Github
load(url("https://raw.githubusercontent.com/ydkristanto/vids-analysis/main/datasets/vid_analytics.RData"))

# Make the data consistent
retention <- retention %>% 
  rename(vid_id = id, vid_title = title)
video_information <- video_information %>% 
  rename(vid_id = id, vid_position = position)
video_stat <- video_stat %>% 
  rename(vid_id = video)

# Create necessary dataframes
video_title <- retention %>% 
  select(vid_id, vid_title) %>% 
  distinct()
lesson_position <- video_information %>% 
  distinct(topic_id, lesson_id) %>% 
  group_by(topic_id) %>% 
  mutate(lesson_position = row_number()) %>% 
  ungroup() %>% 
  select(lesson_id, lesson_position)
video_duration <- retention %>% 
  select(vid_id, duration) %>% 
  distinct() %>% 
  mutate(duration = round(as.numeric(ms(duration)) / 60, 2))
video_info <- video_information %>% 
  left_join(lesson_position, by = join_by(lesson_id)) %>% 
  left_join(video_title, by = join_by(vid_id)) %>% 
  left_join(video_duration, by = join_by(vid_id)) %>% 
  select(
    vid_id, vid_title, duration, talking_head, vid_position,
    lesson_id, lesson_name, lesson_position,
    topic_id, topic_name
  )

# Create data for the Shiny app
basic_stats_data <- video_stat %>% 
  left_join(video_info, by = join_by(vid_id))
retention_data <- retention %>% 
  select(-vid_title, -duration) %>% 
  left_join(video_info, by = join_by(vid_id))

# Remove unnecessary objects
rm(
  list = c(
    "lesson_position", "retention", "video_duration",
    "video_information", "video_stat", "video_title"
  )
)

# Save the data
save.image(file = "datasets/vid_data.RData")

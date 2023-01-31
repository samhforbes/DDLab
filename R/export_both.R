#' read video data from filepath
#'
#' @param filepath a quoted filepath
#'
#' @examples
#'  \dontrun{
#' video_data <- read_video_data('data/annotate.txt')
#' }
#' @export
#' @return a dataframe with annotation
read_video_data <- function(filepath){
  data <- read_csv(filepath, skip = 1)
  return(data)
}

#' read eyetracking data from filepath
#'
#' @param filepath a quoted filepath
#' @param sep the delimeter, defaults to tab
#'
#' @examples
#' \dontrun{
#' et_data <- read_et_data('data/eyetracking.txt', sep = '\t')
#' }
#' @export
#' @return a rectangular dataframe from the eyetracking output
read_et_data <- function(filepath, sep = '\t'){
  et_data <- read.table(filepath, sep = sep, header = T, na.strings = c('NA', '', '.'))
  return(et_data)
}


#' export a combined video and et dataframe in long form
#'
#' Please do check that the output matches what you want to use
#'
#' @param et_data a rectangular data for eyetracking from read_et_data
#' @param video_data a rectangular data from read_video_data
#' @param hertz the capture rate of the video in hertz defaults to 100
#'
#' @examples
#'  \dontrun{
#' video_data <- read_video_data('data/annotate.txt')
#' et_data <- read_et_data('data/eyetrack.txt')
#' out <- export_two_modularities(et_data, video_data, 100)
#' }
#'


export_two_modularities <- function(et_data, video_data, hertz = 100){

  et_data2 <- et_data %>%
    janitor::clean_names()

  namey <- names(et_data2)
  namey <- namey[namey != 'timestamp']
  namey <- namey[namey != 'sample_message']

  if(!'DISPLAY_FLASH' %in% unique(et_data2$sample_message)){
    et_data2 <- et_data2 %>%
      mutate(sample_message = ifelse(sample_message == 'Video Component'|
                                       sample_message == 'Video Component;',
                                     'DISPLAY_FLASH', sample_message))
  }

  video_data2 <- video_data %>%
    janitor::clean_names() %>%
    mutate(diff = time - lag(time),
           timestamp2 = ifelse(is.na(diff), timestamp, diff),
           timestamp2 = cumsum(timestamp2)) %>%
    rename(timestamp_old = timestamp,
           timestamp = timestamp2)

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  samplerate <- getmode(et_data2$timestamp - lag(et_data2$timestamp))


  check <- full_join(video_data2, et_data2, by = 'timestamp') %>%
    arrange(timestamp)

  # check2 <- check %>%
  #   filter(!is.na(time)) %>%
  #   filter(!is.na(recording_session_label))
  #
  # check3 <- inner_join(video_data2, et_data2, by = 'timestamp')

  unique(et_data2$sample_message)

  combined <- check %>%
    mutate(trial_trial = stringr::word(trial_label, 2, sep = ' ')) %>%
    fill(all_of(namey), .direction = 'down') %>%
    fill(track_name) %>% # bad bad bad
    group_by(trial_trial) %>%
    mutate(in_trial = ifelse(sample_message == 'DISPLAY_FLASH', 1, NA)) %>%
    fill(in_trial) %>%
    filter(!is.na(in_trial)) %>%
    mutate(start_time = first(timestamp),
           timestamp_new = timestamp - start_time) %>%
    filter(timestamp_new >= 0) %>%
    ungroup()

  l <- table(combined$track_name, useNA = 'always')
  if(length(l == 1)){
    warning('Only one direction detected in video data, this indicates a sync error')
  }
  # k <- check %>%
  #   mutate(trial_trial = stringr::word(trial_label, 2, sep = ' ')) %>%
  #   fill(all_of(namey), .direction = 'down') %>%
  #   fill(track_name) %>%
  #   group_by(trial_label) %>%
  #   mutate(in_trial = ifelse(sample_message == 'DISPLAY_FLASH', 1, NA)) %>%
  #   fill(in_trial) %>%
  #   filter(!is.na(in_trial)) %>%
  #   mutate(start_time = first(timestamp),
  #          timestamp_new = timestamp - start_time) %>%
  #   filter(timestamp_new >= 0) %>%
  #   summarise(max = max(timestamp_new))


  t1 <- filter(check, trial == 1)
  a <- t1 %>%
    filter(!is.na(sample_message)) %>%
    select(sample_message, sample_index)

  s <- combined %>%  filter(trial_label == 'Trial: 1')

  f <- combined %>%
    filter(trial_trial != trial) %>%
    group_by (trial, trial_trial) %>%
    summarise(time = first(timestamp_new)) %>%
    ungroup()

  clean <- combined %>%
    mutate(et_look = case_when(average_interest_area_label == 'LEFT_BIA' | left_interest_area_label == 'LEFT_BIA' | right_interest_area_label == 'LEFT_BIA' ~ 'L',
                               average_interest_area_label == 'RIGHT_BIA' | left_interest_area_label == 'RIGHT_BIA' | right_interest_area_label == 'RIGHT_BIA' ~ 'R',
                               TRUE ~ NA_character_))

  clean2 <- clean %>%
    mutate(video_look = case_when(track_name == 'left' ~ 'L',
                                  track_name == 'right' ~ 'R',
                                  track_name == 'away' ~ NA_character_,
                                  TRUE ~ NA_character_))

  # downsample
  clean3 <- clean2

  if(hertz != samplerate){
    clean3 <- clean3 %>%
      group_by(recording_session_label, trial_trial) %>%
      mutate(sample_index2 = sample_index - first(sample_index)) %>%
      ungroup()

    clean3$timebin <- floor(clean3[["sample_index2"]] *samplerate / hertz)

    clean3 <- clean3 %>%
      group_by(recording_session_label, trial, trial_trial, condition, side,
               timebin) %>%
      summarise(et_look = getmode(et_look),
                video_look = getmode(video_look),
                timestamp_new = first(timestamp_new),
                timestamp = first(timestamp)) %>%
      ungroup() %>%
      mutate(timestamp3 = timebin * hertz)
  }else{
    clean3$timestamp3 <- clean3$timestamp_new
  }

  et_out <- clean3 %>%
    mutate(target = ifelse(et_look == side, 1, 0),
           distractor = ifelse(et_look != side, 1, 0),
           trackloss = ifelse(is.na(et_look), 1, 0)) %>%
    rename(utrial = trial_trial,
           ID = recording_session_label) %>%
    select(ID, utrial, timestamp, timestamp_new, condition, side, target, distractor, trackloss) %>%
    mutate(video_condition = 'eyetracked',
           trial = paste(utrial, video_condition, sep = '_'))

  video_out <- clean3 %>%
    mutate(target = ifelse(video_look != side, 1, 0), #switch side for video
           distractor = ifelse(video_look == side, 1, 0),
           trackloss = ifelse(is.na(video_look), 1, 0)) %>%
    rename(utrial = trial_trial,
           ID = recording_session_label) %>%
    select(ID, utrial, timestamp, timestamp_new, condition, side, target, distractor, trackloss) %>%
    mutate(video_condition = 'video',
           trial = paste(utrial, video_condition, sep = '_'))

  all <- bind_rows(et_out, video_out)
  return(all)
}

# all2 <- all %>%
#   select(-trial) %>%
#   pivot_wider(names_from = 'video_condition', values_from = c(target, distractor, trackloss))
#
# all3 <- all2
# all3[is.na(all3)] <- 0
#
# with(all3, chisq.test(target_video, target_eyetracked))
# with(all3, chisq.test(distractor_video, distractor_eyetracked))

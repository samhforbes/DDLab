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

  names <- names(data)
  if(!'Timestamp' %in% names){
    names(data)[[4]] <- 'Timestamp'
  }

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
#' @param IA the label for the interest area. Defaults to bIA
#' @param .return_full_data return unfiltered frame for debugging and recoding.
#'
#' @examples
#'  \dontrun{
#' video_data <- read_video_data('data/annotate.txt')
#' et_data <- read_et_data('data/eyetrack.txt')
#' out <- export_two_modularities(et_data, video_data, 100)
#' }
#'


export_two_modularities <- function(et_data, video_data, hertz = 100, IA = 'BIA', .return_full_data = F){

  et_data2 <- et_data %>%
    janitor::clean_names()

  namey <- names(et_data2)
  namey <- namey[namey != 'timestamp']
  namey <- namey[namey != 'sample_message']
  namey <- namey[namey != "average_interest_area_label"]
  namey <- namey[namey != "left_interest_area_label"]
  namey <- namey[namey != "right_interest_area_label"]

  if(!'DISPLAY_FLASH' %in% unique(et_data2$sample_message)){
    et_data2 <- et_data2 %>%
      mutate(sample_message = ifelse(sample_message == 'Video Component'|
                                       sample_message == 'Video Component;',
                                     'DISPLAY_FLASH', sample_message))
  }

  a <- first(which(video_data$Timestamp > 0))

  if(a != 1){
    video_data <- video_data[a:nrow(video_data),]
  }

  video_data2 <- video_data %>%
    janitor::clean_names() %>%
    filter(task == 'VWM') %>%
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


    combined_all <- check %>%
      mutate(trial_trial = stringr::word(trial_label, 2, sep = ' ')) %>%
      fill(trial_trial) %>%
      fill(all_of(namey), .direction = 'down') %>%
      fill(track_name) %>% # bad bad bad
      group_by(trial_trial) %>%
      mutate(in_trial = ifelse(sample_message == 'DISPLAY_FLASH', 1, NA),
             in_trial = ifelse(sample_message == "Trial ended by timeout", 0, in_trial)) %>%
      fill(in_trial) %>% #add end buffer
      mutate(intrial = ifelse(in_trial == 0, NA, in_trial),
             start_time = first(timestamp),
             timestamp_new = timestamp - start_time) %>% #fill blank from merge below
      mutate(left_interest_area_label = ifelse(!is.na(time) & !is.na(in_trial), lag(left_interest_area_label), left_interest_area_label),
             right_interest_area_label = ifelse(!is.na(time) & !is.na(in_trial), lag(right_interest_area_label), right_interest_area_label),
             average_interest_area_label = ifelse(!is.na(time) & !is.na(in_trial), lag(average_interest_area_label), average_interest_area_label)) %>%
      ungroup()

lIA <- paste('LEFT', IA, sep = '_')
rIA <- paste('RIGHT', IA, sep = '_')

    clean_all <- combined_all %>%
      mutate(et_look = case_when(average_interest_area_label == lIA | left_interest_area_label == lIA | right_interest_area_label == lIA ~ 'L',
                                 average_interest_area_label == rIA | left_interest_area_label == rIA | right_interest_area_label == rIA ~ 'R',
                                 TRUE ~ NA_character_))

    clean2_all <- clean_all %>%
      mutate(video_look = case_when(track_name == 'left' ~ 'R', #do the switch here instead of further down for this case.
                                    track_name == 'right' ~ 'L',
                                    track_name == 'away' ~ 'A',
                                    TRUE ~ NA_character_)) %>%
      mutate(video_in_frame = ifelse(!is.na(time), 1, NA)) #%>%
      # fill(et_look)

    #this is Jake export

    # all = clean2_all

    # message('raw left eye eyetracking directions are: \n',
    #     table(et_data2$left_interest_area_label, useNA = 'always'),
    #     'raw right eye eyetracking directions are: \n',
    #     table(et_data2$right_interest_area_label, useNA = 'always'),
    #     'output ET directions are: \n',
    #   table(all$et_look, useNA = 'always'))

    clean3 <- clean2_all %>%
      filter(in_trial == 1) %>%
      group_by(trial, trial_trial) %>%
      mutate(diff2 = timestamp - lag(timestamp),
      timebin = floor(timestamp_new / hertz)) %>%
      ungroup()

    clean4 <- clean3 %>%
      group_by(trial, trial_trial, condition, side,
               timebin) %>%
      summarise(recording_session_label = first(recording_session_label),
                et_look = getmode(et_look),
                video_look = getmode(video_look),
                timestamp_new = first(timestamp_new),
                timestamp = first(timestamp)) %>%
      ungroup() %>%
      mutate(timestamp3 = timebin * hertz) %>%
      select(recording_session_label, everything())

    et_out <- clean4 %>%
      mutate(target = ifelse(et_look == side, 1, 0),
             distractor = ifelse(et_look != side, 1, 0),
             trackloss = ifelse(is.na(et_look), 1, 0)) %>%
      rename(utrial = trial,
             ID = recording_session_label,
             direction = et_look) %>%
      select(ID, utrial, timestamp, timestamp_new, direction, condition, side, target, distractor, trackloss) %>%
      mutate(video_condition = 'eyetracked',
             trial = paste(utrial, video_condition, sep = '_'),
             direction = as.character(direction))

    video_out <- clean4 %>%
      mutate(video_look = ifelse(video_look == 'A', NA, video_look),
             target = ifelse(video_look == side, 1, 0), #don't switch side (above)
             distractor = ifelse(video_look != side, 1, 0),
             trackloss = ifelse(is.na(video_look), 1, 0)) %>%
      rename(utrial = trial,
             ID = recording_session_label,
             direction = video_look) %>%
      select(ID, utrial, timestamp, timestamp_new, direction, condition, side, target, distractor, trackloss) %>%
      mutate(video_condition = 'video',
             trial = paste(utrial, video_condition, sep = '_'),
             direction = as.character(direction))

    all <- bind_rows(et_out, video_out)
    #that is final for analysis

    #check ratios of left look
    inT <- clean2_all %>%
      filter(in_trial == 1) %>%
      mutate(video_look = ifelse(video_look == 'A', NA, video_look))

    tab_rat = table(inT$et_look, useNA = 'always')
    inT_ratio = tab_rat[[1]]/nrow(inT)

    clean_rat = table(et_out$direction, useNA = 'always')
    clean_ratio = clean_rat[[1]]/nrow(et_out)

    message('Eyetracking \n _________________________ \n Ratio of left looks in full data =', round(inT_ratio, 4), '\n Ratio of left looks in downsampled data =', round(clean_ratio, 4), '\n
            If this looks wrong contact me!')

    vid_rat = table(inT$video_look, useNA = 'always')
    vid_ratio = vid_rat[[1]]/nrow(inT)

    out_rat = table(video_out$direction, useNA = 'always')
    out_ratio = out_rat[[1]]/nrow(video_out)

    message('Video \n _________________________ \n Ratio of left looks in full data =', round(vid_ratio, 4), '\n Ratio of left looks in downsampled data =', round(out_ratio, 4), '\n
            If this looks wrong contact me!')

    if(.return_full_data == T){
    return(clean2_all)
  }else{
    return(all)

  # combined <- check %>%
  #   mutate(trial_trial = stringr::word(trial_label, 2, sep = ' ')) %>%
  #   fill(all_of(namey), .direction = 'down') %>%
  #   fill(track_name) %>% # bad bad bad
  #   group_by(trial_trial) %>%
  #   mutate(in_trial = ifelse(sample_message == 'DISPLAY_FLASH', 1, NA)) %>%
  #   fill(in_trial) %>%
  #   filter(!is.na(in_trial)) %>%
  #   mutate(start_time = first(timestamp),
  #          timestamp_new = timestamp - start_time) %>%
  #   filter(timestamp_new >= 0) %>%
  #   ungroup()
  #
  # l <- table(combined$track_name, useNA = 'always')
  #
  # if(length(l) == 1){
  #   warning('Only one direction detected in video data, this indicates a sync error')
  # }
  #
  # # k <- check %>%
  # #   mutate(trial_trial = stringr::word(trial_label, 2, sep = ' ')) %>%
  # #   fill(all_of(namey), .direction = 'down') %>%
  # #   fill(track_name) %>%
  # #   group_by(trial_label) %>%
  # #   mutate(in_trial = ifelse(sample_message == 'DISPLAY_FLASH', 1, NA)) %>%
  # #   fill(in_trial) %>%
  # #   filter(!is.na(in_trial)) %>%
  # #   mutate(start_time = first(timestamp),
  # #          timestamp_new = timestamp - start_time) %>%
  # #   filter(timestamp_new >= 0) %>%
  # #   summarise(max = max(timestamp_new))
  #
  #
  # t1 <- filter(check, trial == 1)
  # a <- t1 %>%
  #   filter(!is.na(sample_message)) %>%
  #   select(sample_message, sample_index)
  #
  # s <- combined %>%  filter(trial_label == 'Trial: 1')
  #
  # f <- combined %>%
  #   filter(trial_trial != trial) %>%
  #   group_by (trial, trial_trial) %>%
  #   summarise(time = first(timestamp_new)) %>%
  #   ungroup()
  #
  # cleana <- combined %>%
  #   mutate(et_look = case_when(average_interest_area_label == 'LEFT_BIA' | left_interest_area_label == 'LEFT_BIA' | right_interest_area_label == 'LEFT_BIA' ~ 'L',
  #                              average_interest_area_label == 'RIGHT_BIA' | left_interest_area_label == 'RIGHT_BIA' | right_interest_area_label == 'RIGHT_BIA' ~ 'R',
  #                              TRUE ~ NA_character_))
  #
  # clean2 <- cleana %>%
  #   mutate(video_look = case_when(track_name == 'left' ~ 'L',
  #                                 track_name == 'right' ~ 'R',
  #                                 track_name == 'away' ~ NA_character_,
  #                                 TRUE ~ NA_character_))
  #
  # # downsample
  # clean3 <- clean2
  #
  # if(hertz != samplerate){
  #   clean3 <- clean3 %>%
  #     group_by(recording_session_label, trial_trial) %>%
  #     mutate(sample_index2 = sample_index - first(sample_index)) %>%
  #     ungroup()
  #
  #   clean3$timebin <- floor(clean3[["sample_index2"]] *samplerate / hertz)
  #
  #   clean3 <- clean3 %>%
  #     group_by(recording_session_label, trial, trial_trial, condition, side,
  #              timebin) %>%
  #     summarise(et_look = getmode(et_look),
  #               video_look = getmode(video_look),
  #               timestamp_new = first(timestamp_new),
  #               timestamp = first(timestamp)) %>%
  #     ungroup() %>%
  #     mutate(timestamp3 = timebin * hertz)
  # }else{
  #   clean3$timestamp3 <- clean3$timestamp_new
  # }
  #
  # et_out <- clean3 %>%
  #   mutate(target = ifelse(et_look == side, 1, 0),
  #          distractor = ifelse(et_look != side, 1, 0),
  #          trackloss = ifelse(is.na(et_look), 1, 0)) %>%
  #   rename(utrial = trial_trial,
  #          ID = recording_session_label) %>%
  #   select(ID, utrial, timestamp, timestamp_new, condition, side, target, distractor, trackloss) %>%
  #   mutate(video_condition = 'eyetracked',
  #          trial = paste(utrial, video_condition, sep = '_'))
  #
  # video_out <- clean3 %>%
  #   mutate(target = ifelse(video_look != side, 1, 0), #switch side for video
  #          distractor = ifelse(video_look == side, 1, 0),
  #          trackloss = ifelse(is.na(video_look), 1, 0)) %>%
  #   rename(utrial = trial_trial,
  #          ID = recording_session_label) %>%
  #   select(ID, utrial, timestamp, timestamp_new, condition, side, target, distractor, trackloss) %>%
  #   mutate(video_condition = 'video',
  #          trial = paste(utrial, video_condition, sep = '_'))
  #
  # all <- bind_rows(et_out, video_out)
  }
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

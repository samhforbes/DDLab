#' create change side data
#'
#' @param window_start start time
#' @param window_stop stop time
#' @param side side column
#' @param trackloss trackloss prop, default .75
#'
#' @export
#'
#' @return a summary output

create_change_side_data <- function(data, window_start = 1750, window_stop = 6750, side = 'side', trackloss = .75,
                                    time_column = 'Timestamp',
                                    aoi_columns = c('Target', 'Distractor'),
                                    trackloss_column = 'Trackloss',
                                    participant_column = 'ID',
                                    trial_column = 'UTrial'){

  # to do: come back and generalise

  ETanalysis <- data %>% #JOHN should this come from full_filter instead?
    filter(!is.na(condition))

  ETanalysis4 <- ETanalysis %>%
    filter(timestamp_new >= 1000, timestamp_new <= 10000) #1000 is first change

  # split by first change at 1000
  ETanalysis4_t <- ETanalysis4 %>%
    group_by(ID, trial, utrial, condition, video_condition) %>%
    filter(!is.na(target)) %>% #ie remove trackloss
    summarise(FirstT = first(target),
              FirstTime = first(timestamp_new)) %>%
    mutate(FirstLook = ifelse(FirstT == TRUE, 'Change', 'No_Change')) %>%
    ungroup()

  #compare rough classification
  table(ETanalysis4_t$FirstLook, ETanalysis4_t$video_condition)

  a <- ETanalysis4_t %>%
    filter(video_condition == 'eyetracked')
  if(nrow(a) > 0){
  #new
  ET_test <- ETanalysis4_t %>%
    select(ID, utrial, condition, video_condition, FirstLook) %>%
    pivot_wider(names_from = video_condition, values_from = FirstLook) %>%
    mutate(Agree = ifelse(eyetracked == video, 'Y', 'N'))


  val <- table(ET_test$Agree)[['Y']]/nrow(ET_test)
  message('First look agreement is ' , val, ' \n')
  }else{
    message('No eyetracking data to report for comparison \n')
    val <- NA
  }

  # better to constrain to FirstLook within 2500ms using ET
  ETanalysis4_2b <- ETanalysis4 %>%
    filter(timestamp_new >= 1000, timestamp_new <= 10000) %>% #update using 1d format
    group_by(ID, trial, utrial, condition, video_condition) %>%
    filter(!is.na(target)) %>%
    summarise(FirstT = first(target),
              FirstTime = first(timestamp_new)) %>%
    filter(FirstTime < 2500)  %>%
    mutate(FirstLook = ifelse(FirstT == TRUE, 'Change', 'No_Change')) %>%
    ungroup() %>%
    filter(video_condition == 'video') %>% # change to video
    select(-video_condition, -trial)

  # join back and apply classification to both
  ETanalysis4_3b <- left_join(ETanalysis, ETanalysis4_2b) %>%
    filter(condition != 0)

  final_window <- subset_by_window(ETanalysis4_3b,
                                   window_start_time = 1750, # final restricted time for 5 sec analysis
                                   window_end_time = 6750,
                                   rezero = F,
                                   remove = T)

  #get prop 5
  windowCP <- make_time_window_data(final_window,
                                    aois = 'target',
                                    predictor_columns = c('video_condition', 'condition'),
                                    summarize_by = 'ID')

  # get window for NC C
  windowMis <- make_time_window_data(final_window,
                                     aois = 'target',
                                     predictor_columns = c(
                                       'FirstLook', 'video_condition', 'condition'),
                                     summarize_by = 'ID')

  windowFL <- windowMis

  windowMisWide <- windowFL %>%
    select(ID, video_condition, condition, FirstLook, Prop) %>%
    spread(FirstLook, Prop) %>%
    rename(Prop_C = Change) %>%
    rename(Prop_NC = No_Change) %>%
    #select(-No_Change) %>%
    mutate(Prop_NC = ifelse(is.nan(Prop_NC), NA, Prop_NC),
           Prop_C = ifelse(is.nan(Prop_C), NA, Prop_C))

  windowMisWideOutput <- windowMisWide %>%
    select(ID, video_condition, condition, Prop_C, Prop_NC)

  windowCPOutput <- windowCP %>%
    select(ID, video_condition, condition, Prop)

  windowOutputFull <- full_join(windowMisWideOutput, windowCPOutput) %>%
    mutate(participant_look_agreement = val)

  return(windowOutputFull)
}

#' Join a change side data with a visexp data
#'
#' @param change_data change side dataframe
#' @param visexp_data vis exp dataframe
#' @param .extra other columns to keep
#'
#' @export
#'
#' @return a clean dataframe
#'

join_CS_to_VE <- function(change_data, visexp_data, .other = NULL){

  VE_data <- visexp_data %>%
    rename(condition = Load) %>%
    group_by(ID, video_condition, condition) %>%
    summarise(TLT = mean(TLT, na.rm = T),
              MLD = mean(MLD, na.rm = T),
              SR = mean(SR, na.rm = T),
              CP10 = mean(CP, na.tm = T))

  all_data <- full_join(change_data, VE_data)

  return(all_data)
}

#' Master run
#' Do the lot!
#'
#' @param input_file a text file
#' @param output_location output location
#' @param save_raw if T then exports raw as well as summarised
#' @param task a task
#' @param .kappa_start pass to get_kappa
#' @param .kappa_stop pass to get_kappa
#'
#' @export
#' @return all the data

convert_and_compare <- function(input_file, output_location = '', save_raw = F, task = 'VWM', .kappa_start = 0, .kappa_stop = 10000){

  input_file <- read_csv(input_file,
                         col_names = F)
  for(i in 1:nrow(input_file)){
    ID <- input_file[[2]][[i]]
    Camera <- input_file[[3]][[i]]
    Path = input_file[[1]][[i]]

    pathML <- paste(Path, '/', ID, '/', Camera, sep = '')
    pathET <- paste(Path, '/', ID, sep = '')

    mylist <- list.dirs(pathET)

    mylist <- list.dirs(pathET)
    # myfiles <- mylist[2:31]


    etlist <- list.files(pathET, pattern = 'Output_', recursive = T, full.names = T)
    mllist <- list.files(pathML, pattern = 'livecapture_', recursive = T, full.names = T)

    mllist <- mllist[grepl("\\.txt$", mllist)]

    ETfiles <- lapply(etlist, read_et_data)
    MLfiles <- lapply(mllist, read_video_data)

    Note <- ETfiles[[1]][[1]][[1]]

    message('now we are looking at ', Note, '! \n')

    file_out <- list()
    for(i in 1:length(ETfiles)){
      #  file_out[[i]] <- export_two_modularities(ETfiles[[i]], MLfiles[[i]], IA='BIA')
      #  file_out[[i]] <- export_two_modularities(ETfiles[[i]], MLfiles[[i]], IA='SIA')
      file_out[[i]] <- export_two_modularities(ETfiles[[i]], MLfiles[[i]], IA='sIA', task = 'VWM', .return_full_data = F)
    }

    z <- lapply(file_out, nrow)[[1]]
    z

    full <- bind_rows(file_out)

    full <- full %>%
      filter(utrial != 0) %>%
      filter(condition !=0) #for VWM

    data <- make_eyetrackingr_data(full,
                                   participant_column = 'ID',
                                   trackloss_column = 'trackloss',
                                   time_column = 'timestamp_new',
                                   trial_column = 'trial',
                                   aoi_columns = c('target', 'distractor'),
                                   treat_non_aoi_looks_as_missing = F)

    if(save_raw == TRUE){
      savename <- paste(output_location, '/', ID, '_Video',Camera,'_raw.csv', sep = '')

      write_csv(full, file = savename)
    }

    cs_data <- create_change_side_data(data = data)
    ve_data <- create_manual_trial_data2(data = full)
    kappa <- get_kappa_value(data = full,
                             start_time = .kappa_start,
                             stop_time = .kappa_stop)

    all <- join_CS_to_VE(change_data = cs_data,
                         visexp_data = ve_data)

    all$participant_kappa <- kappa

    savename2 <- paste(output_location, '/', ID, '_Video',Camera,'.csv', sep = '')
    write_csv(all, file = savename2)

  }


}

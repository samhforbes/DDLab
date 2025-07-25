#' create a trial report from a fixation eyetracking data
#'
#' This was designed to work with eyelink fixation reports and the VWM trial.
#' I can't guarantee it will bring out what you want beyond that so please check
#' the output carefully.
#'
#' @param data a dataframe read in from a fixation report CSV or txt
#' @param task in quotes, a string regarding the task you are interested in from the task column
#' @param write if TRUE will save a csv in the current working directory
#' @param show_all_missing if T will assume 18 trials per participant and leave blank rows
#'
#' @examples
#' library(readr)
#' data <- read_csv("etdata.csv")
#' data_out <- create_ET_trial_data(data, task = 'VWM', write = F)
#'
#' @export
#'
#' @return A formatted dataframe with CP, SwitchRate, MLD and TLT, as well as coding info.

create_ET_trial_data <- function(data, task, write = F, show_all_missing = F){

  ETd <- data

  if('CURRENT_FIX_INTEREST_AREA_LABEL' %in% names(ETd)){
    ETd <- ETd
  }else{
    ETd <- ETd %>%
      mutate(CURRENT_FIX_INTEREST_AREA_LABEL = CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL)
    message('Note there is no column entitled CURRENT_FIX_INTEREST_AREA_LABEL, so using CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL instead.')
  }

  ETdata2 <- ETd %>%
    group_by(RECORDING_SESSION_LABEL, trial) %>%
    filter(task == task,
           CURRENT_FIX_INTEREST_AREA_LABEL != '.' & !is.nan(CURRENT_FIX_INTEREST_AREA_LABEL) & CURRENT_FIX_INTEREST_AREA_LABEL != 'NaN') %>%
    mutate(CURRENT_FIX_INTEREST_AREA_LABEL = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == 'LEFT_IA', 'LEFT_BIA', CURRENT_FIX_INTEREST_AREA_LABEL),
           CURRENT_FIX_INTEREST_AREA_LABEL = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == 'RIGHT_IA', 'RIGHT_BIA', CURRENT_FIX_INTEREST_AREA_LABEL)) %>%
    mutate(LEFT = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == 'LEFT_BIA', CURRENT_FIX_DURATION, 0),
           RIGHT = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == 'RIGHT_BIA', CURRENT_FIX_DURATION, 0)) %>%
    ungroup() %>%
    group_by(RECORDING_SESSION_LABEL, trial) %>%
    mutate(LOOKS = ifelse(CURRENT_FIX_RUN_INDEX ==1 | CURRENT_FIX_RUN_INDEX <= lag(CURRENT_FIX_RUN_INDEX), 1, 0),
           LOOKS = ifelse(is.na(LOOKS), 1, LOOKS)) %>%
    filter(CURRENT_FIX_INTEREST_AREA_LABEL == 'LEFT_BIA' | CURRENT_FIX_INTEREST_AREA_LABEL == 'RIGHT_BIA') %>%
    mutate(SWITCH = 0 ) %>%
    mutate(SWITCH = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == lag(CURRENT_FIX_INTEREST_AREA_LABEL), 0, 1)) %>%
    mutate(SWITCH = ifelse(is.na(SWITCH), 0, SWITCH)) %>%
    summarise(ChangeSide = first(side),
              Load = first(condition),
              Left = sum(LEFT),
              Right = sum(RIGHT),
              Switch = sum(SWITCH),
              Looks = sum(LOOKS)
    ) %>%
    mutate(TLT = Left + Right,
           PercLook = TLT/10000,
           MLD = TLT/Looks,
           SR = Switch/(TLT/1000),
           CP = ifelse(ChangeSide == 'L', Left/TLT, Right/TLT),
           Both = ifelse(Left > 0 & Right > 0, 'Y', 'N'),
          # ToCode = ifelse(Both == 'N', 'Y', 'N')) %>%
           ToCode = ifelse(TLT == 0, 'Y', 'N')) %>%
    ungroup()

  All <- ETd %>%
    filter(task == task) %>%
    group_by(RECORDING_SESSION_LABEL, trial) %>%
    summarise(ChangeSide = first(side),
              Load = first(condition)) %>%
    ungroup()

  data_out <- merge(All, ETdata2, by = c('RECORDING_SESSION_LABEL', 'trial', 'ChangeSide', 'Load'))

  data_out <- data_out %>%
    mutate(ToCode = ifelse(is.na(ToCode), 'Y', ToCode)) %>%
    rename(ID = RECORDING_SESSION_LABEL,
           Trial = trial) %>%
    mutate(Trial = as.numeric(Trial)) %>%
    mutate(Load = as.character(Load)) %>%
    arrange(ID, Trial)

  if(show_all_missing == T){
    ID <- rep(unique(data_out$ID), each = 18)
    Trial <- rep(c(1:18), times = length(unique(data_out$ID)))

    datum <- data.frame(ID, Trial)

    data_out2 <- merge(datum, data_out, by = c('ID', 'Trial'), all = T, sort = F)

    data_out2 <- data_out2 %>%
      arrange(ID, Trial)
  }else{data_out2 <- data_out}

  if(write == T){
    write_csv(data_out2, paste('Clean_Data_', task, '.csv', sep = ''))
  }

  return(data_out2)
}


#' create a trial report from a sample eyetracking data cleaned the way Sam likes it.
#'
#' This was designed to work with cleaned eyetracking data reports and the VWM trial.
#' I can't guarantee it will bring out what you want beyond that so please check
#' the output carefully.
#'
#' @param data a dataframe read in from a fixation report CSV or txt
#' @param write if TRUE will save a csv in the current working directory
#' @param show_all_missing if T will assume 18 trials per participant and leave blank rows
#'
#' @examples
#' library(readr)
#' data <- read_csv("etdata.csv")
#' data_out <- create_ET_trial_data(data, task = 'VWM', write = F)
#'
#' @export
#'
#' @return A formatted dataframe with CP, SwitchRate, MLD and TLT, as well as coding info.

create_ET_trial_data2 <- function(data, write = F, show_all_missing = F){

  ETd <- data

  ETdata2 <- ETd %>%
    group_by(Run, Age, Trial) %>%
    mutate(CURRENT_FIX_INTEREST_AREA_LABEL = case_when(Target == 1 ~ 'Target',
                                                       Distractor == 1 ~ 'Distractor',
                                                       Trackloss == 1 ~ 'Trackloss',
                                                       TRUE ~ 'Trackloss')) %>%

    mutate(CURRENT_FIX_START = ifelse(first(CURRENT_FIX_INTEREST_AREA_LABEL == 'Trackloss'), 0, 1),
           CURRENT_FIX_START = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL != lag(CURRENT_FIX_INTEREST_AREA_LABEL) & CURRENT_FIX_INTEREST_AREA_LABEL != 'Trackloss', CURRENT_FIX_START + 1, CURRENT_FIX_START),
           CURRENT_FIX_RUN_INDEX = cumsum(CURRENT_FIX_START)) %>%
    mutate(LOOKS = ifelse(CURRENT_FIX_RUN_INDEX ==1 | CURRENT_FIX_RUN_INDEX <= lag(CURRENT_FIX_RUN_INDEX), 1, 0),
           LOOKS = ifelse(is.na(LOOKS), 1, LOOKS),
           TOTAL_ON = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == 'Target' |
                            CURRENT_FIX_INTEREST_AREA_LABEL == 'Distractor', 1, 0)) %>%
    filter(CURRENT_FIX_INTEREST_AREA_LABEL == 'Target' | CURRENT_FIX_INTEREST_AREA_LABEL == 'Distractor') %>%
  mutate(SWITCH = 0 ) %>%
    mutate(SWITCH = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == lag(CURRENT_FIX_INTEREST_AREA_LABEL), 0, 1)) %>%
    mutate(SWITCH = ifelse(is.na(SWITCH), 0, SWITCH)) %>%
    summarise(ChangeSide = first(Side),
              Load = first(Load),
              Switch = sum(SWITCH),
              Looks = sum(CURRENT_FIX_START),
              Tar = sum(Target),
              Dis = sum(Distractor)
    ) %>%
    mutate(TLT = (Tar + Dis) * 10,
           PercLook = TLT/10000,
           MLD = TLT/Looks,
           SR = Switch/(TLT/1000),
           CP = 10 * Tar / TLT,
           Both = ifelse(Tar > 0 & Dis > 0, 'Y', 'N'),
           # ToCode = ifelse(Both == 'N', 'Y', 'N')) %>%
           ToCode = ifelse(TLT == 0, 'Y', 'N')) %>%
    ungroup()

  All <- ETd %>%
    group_by(Run, Age, Trial, Condition) %>%
    summarise(ChangeSide = first(Side),
              Load = first(Load)) %>%
    ungroup()

  data_out <- left_join(All, ETdata2, by = c('Run', 'Age', 'Trial', 'ChangeSide', 'Load'))

  data_out <- data_out %>%
    mutate(ToCode = ifelse(is.na(ToCode), 'Y', ToCode)) %>%
    # rename(ID = Run,
    #        Trial = trial) %>%
    mutate(Trial = as.numeric(Trial)) %>%
    mutate(Load = as.character(Load)) %>%
    arrange(Run, Trial)

  # if(show_all_missing == T){
  #   ID <- rep(unique(data_out$ID), each = 18)
  #   Trial <- rep(c(1:18), times = length(unique(data_out$ID)))
  #
  #   datum <- data.frame(ID, Trial)
  #
  #   data_out2 <- merge(datum, data_out, by = c('ID', 'Trial'), all = T, sort = F)
  #
  #   data_out2 <- data_out2 %>%
  #     arrange(ID, Trial)
  # }else{data_out2 <- data_out}

  data_out2 <- data_out

  if(write == T){
    write_csv(data_out2, paste('Clean_Data_', task, '.csv', sep = ''))
  }

  return(data_out2)
}

#' create a trial report from a sample eyetracking data
#'
#' This was designed to work with cleaned eyetracking data reports and the VWM trial.
#' I can't guarantee it will bring out what you want beyond that so please check
#' the output carefully.
#'
#' @param data a dataframe read in from a fixation report CSV or txt
#' @param write if TRUE will save a csv in the current working directory
#' @param show_all_missing if T will assume 18 trials per participant and leave blank rows
#'
#' @examples
#' library(readr)
#' data <- read_csv("etdata.csv")
#' data_out <- create_ET_trial_data(data, task = 'VWM', write = F)
#'
#' @export
#'
#' @return A formatted dataframe with CP, SwitchRate, MLD and TLT, as well as coding info.

create_ET_trial_data3 <- function(data, write = F, show_all_missing = F){

  ETd <- data

  samplerate = ETd$Timestamp[2] - ETd$Timestamp[1]

  ETdata2 <- ETd %>%
    group_by(Run, Age, Trial) %>%
    mutate(CURRENT_FIX_INTEREST_AREA_LABEL = case_when(Target == 1 ~ 'Target',
                                                       Distractor == 1 ~ 'Distractor',
                                                       Trackloss == 1 ~ 'Trackloss',
                                                       TRUE ~ 'Trackloss')) %>%

    mutate(CURRENT_FIX_START = ifelse(first(CURRENT_FIX_INTEREST_AREA_LABEL == 'Trackloss'), 0, 1),
           CURRENT_FIX_START = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL != lag(CURRENT_FIX_INTEREST_AREA_LABEL) & CURRENT_FIX_INTEREST_AREA_LABEL != 'Trackloss', CURRENT_FIX_START + 1, CURRENT_FIX_START),
           CURRENT_FIX_RUN_INDEX = cumsum(CURRENT_FIX_START)) %>%
    mutate(LOOKS = ifelse(CURRENT_FIX_RUN_INDEX ==1 | CURRENT_FIX_RUN_INDEX <= lag(CURRENT_FIX_RUN_INDEX), 1, 0),
           LOOKS = ifelse(is.na(LOOKS), 1, LOOKS),
           TOTAL_ON = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == 'Target' |
                               CURRENT_FIX_INTEREST_AREA_LABEL == 'Distractor', 1, 0)) %>%
    filter(CURRENT_FIX_INTEREST_AREA_LABEL == 'Target' | CURRENT_FIX_INTEREST_AREA_LABEL == 'Distractor') %>%
    mutate(SWITCH = 0 ) %>%
    mutate(SWITCH = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == lag(CURRENT_FIX_INTEREST_AREA_LABEL), 0, 1)) %>%
    mutate(SWITCH = ifelse(is.na(SWITCH), 0, SWITCH)) %>%
    summarise(ChangeSide = first(Side),
              Load = first(Load),
              Switch = sum(SWITCH),
              Looks = sum(CURRENT_FIX_START, na.rm = T),
              Tar = sum(Target) * samplerate,
              Dis = sum(Distractor) * samplerate
    ) %>%
    mutate(TLT = (Tar + Dis),
           PercLook = TLT/10000,
           MLD = TLT/Looks,
           SR = Switch/(TLT/1000),
           CP =  Tar / TLT,
           Both = ifelse(Tar > 0 & Dis > 0, 'Y', 'N'),
           # ToCode = ifelse(Both == 'N', 'Y', 'N')) %>%
           ToCode = ifelse(TLT == 0, 'Y', 'N')) %>%
    ungroup()

  All <- ETd %>%
    group_by(Run, Age, Trial, Condition) %>%
    summarise(ChangeSide = first(Side),
              Load = first(Load)) %>%
    ungroup()

  data_out <- left_join(All, ETdata2, by = c('Run', 'Age', 'Trial', 'ChangeSide', 'Load'))

  data_out <- data_out %>%
    mutate(ToCode = ifelse(is.na(ToCode), 'Y', ToCode)) %>%
    # rename(ID = Run,
    #        Trial = trial) %>%
    mutate(Trial = as.numeric(Trial)) %>%
    mutate(Load = as.character(Load)) %>%
    arrange(Run, Trial)

  # if(show_all_missing == T){
  #   ID <- rep(unique(data_out$ID), each = 18)
  #   Trial <- rep(c(1:18), times = length(unique(data_out$ID)))
  #
  #   datum <- data.frame(ID, Trial)
  #
  #   data_out2 <- merge(datum, data_out, by = c('ID', 'Trial'), all = T, sort = F)
  #
  #   data_out2 <- data_out2 %>%
  #     arrange(ID, Trial)
  # }else{data_out2 <- data_out}

  data_out2 <- data_out

  if(write == T){
    write_csv(data_out2, paste('Clean_Data_', task, '.csv', sep = ''))
  }

  return(data_out2)
}

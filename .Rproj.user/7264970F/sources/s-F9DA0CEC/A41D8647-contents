#' create a trial report from a fixation eyetracking data
#'
#' This was designed to work with eyelink fixation reports and the VWM trial.
#' I can't guarantee it will bring out what you want beyond that so please check
#' the output carefully.
#'
#' @param data a dataframe read in from a fixation report CSV or txt
#' @param task in quotes, a string regarding the task you are interested in from the task column
#' @param write if TRUE will save a csv in the current working directory
#'
#' @examples
#' library(readr)
#' data <- read_csv("etdata.csv")
#' data_out <- create_ET_trial_data(data, task = 'VWM', write = F)
#'
#' @export
#'
#' @return A formatted dataframe with CP, SwitchRate, MLD and TLT, as well as coding info.

create_ET_trial_data <- function(data, task, write = F){

  ETd <- data

  ETdata2 <- ETd %>%
    filter(task == task,
           CURRENT_FIX_INTEREST_AREA_LABEL != '.') %>%
    mutate(LEFT = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == 'LEFT_BIA', CURRENT_FIX_DURATION, 0),
           RIGHT = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == 'RIGHT_BIA', CURRENT_FIX_DURATION, 0)) %>%
    mutate(SWITCH = ifelse(CURRENT_FIX_INTEREST_AREA_LABEL == lag(CURRENT_FIX_INTEREST_AREA_LABEL), 0, 1)) %>%
    group_by(RECORDING_SESSION_LABEL, trial) %>%
    summarise(ChangeSide = first(side),
              Load = first(condition),
              Left = sum(LEFT),
              Right = sum(RIGHT),
              Switch = sum(SWITCH)
    ) %>%
    mutate(Looks = Switch + 1,
           TLT = Left + Right,
           PercLook = TLT/10000,
           MLD = TLT/Looks,
           SR = Switch/(TLT/1000),
           CP = ifelse(ChangeSide == 'Left', Left/TLT, Right/TLT),
           Both = ifelse(Left > 0 & Right > 0, 'Y', 'N'),
           ToCode = ifelse(Both == 'NAY' | PercLook < .4, 'Y', 'N')) %>%
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
           Trial = trial)

  if(write == T){
  write_csv(data_out, paste('Clean_Data_', task, '.csv', sep = ''))
  }

  return(data_out)
}



#' create a trial report from a datavyu output data
#'
#' This was designed to work with eyelink fixation reports and the VWM trial.
#' I can't guarantee it will bring out what you want beyond that so please check
#' the output carefully.
#'
#' @param data a dataframe read in from a datavyu report CSV or txt
#' @param write if TRUE will save a csv in the current working directory
#'
#' @examples
#' library(readr)
#' data <- read_csv("datavyudata.csv")
#' data_out <- create_datavyu_trial_data(data, write = F)
#'
#' @export
#'
#' @return A formatted dataframe with CP, SwitchRate, MLD and TLT, as well as coding info.

create_datavyu_trial_data <- function(data, write = F){

  if('TrialLook.looking_direction' %in% names(data)){
    dvdata <- data %>%
      mutate(direction = TrialLook.looking_direction)
  }else{
    dvdata <- data %>%
      mutate(direction = TrialLook.looking_code01)
  }

  dvsubj <- dvdata %>%
    select(SubID.SUBID, SubID.onset) %>%
    rename(TrialLook.offset = SubID.onset) %>%
    filter(!is.na(SubID.SUBID))

  dvdata2 <- dvdata %>%
    mutate(Trial = TrialLook.trials_trialnum,
           Load = TrialLook.trials_ss,
           ChangeSide = TrialLook.trials_changeside) %>%
    filter(!is.na(Trial)) %>%
    rename(Direction = direction) %>%
    mutate(Direction = stringr::str_to_upper(Direction)) %>%
    mutate(Duration = TrialLook.offset - TrialLook.onset,
           Left = ifelse(Direction == 'R', Duration, 0),
           Right = ifelse(Direction == 'L', Duration, 0))
  #switched perspective

  dvdata3 <- merge(dvsubj, dvdata2, by = 'TrialLook.offset', all = T)

  dvdata4 <- dvdata3 %>%
    tidyr::fill(SubID.SUBID.x) %>%
    filter(!is.na(Duration)) %>%
    rename(ID = SubID.SUBID.x) %>%
    select(ID, Trial, ChangeSide, Load, Direction, Duration, Left, Right) %>%
    group_by(ID, Trial, ChangeSide, Load) %>%
    summarise(Left = sum(Left),
              Right = sum(Right),
              Switch = length(Direction) - 1,
              Looks = length(Direction),
              TLT = sum(Left) + sum(Right)) %>%
    mutate(PercLook = TLT/10000,
           MLD = TLT/Looks,
           SR = Switch/(TLT/1000),
           CP = ifelse(ChangeSide == 'L', Left/TLT, Right/TLT),
           Both = ifelse(Left > 0 & Right > 0, 'Y', 'N'),
           ToCode = 'Coded') %>%
    ungroup() %>%
    mutate(ID = stringr::str_remove(ID, '_'))

  if(write == T){
    write_csv(dvdata4, paste('Coded_Data', '.csv', sep = ''))
  }

  return(dvdata4)
}

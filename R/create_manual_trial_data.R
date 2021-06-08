#' create a trial report from a datavyu output data
#'
#' This was designed to work with datavyu frame-by-frame reports and the VWM trial.
#' I can't guarantee it will bring out what you want beyond that so please check
#' the output carefully.
#'
#' @param data a dataframe read in from a datavyu report CSV or txt
#' @param write if TRUE will save a csv in the current working directory
#'
#' @examples
#' library(readr)
#' data <- read_csv("datavyudata.csv")
#' data_out <- create_manual_trial_data(data, write = F)
#'
#' @export
#'
#' @return A formatted dataframe with CP, SwitchRate, MLD and TLT, as well as coding info

create_manual_trial_data <- function(data, write = F){

  if('SUBID.SUBID' %in% names(data)){
    data <- data %>%
      rename(SubID.SUBID = SUBID.SUBID)
  }
  if('SubId.SUBID' %in% names(data)){
    data <- data %>%
      rename(SubID.SUBID = SubId.SUBID)
  }
  if('SubId.ID' %in% names(data)){
    data <- data %>%
      rename(SubID.SUBID = SubId.ID)
  }
  if('SubID.code01' %in% names(data)){
    data <- data %>%
      rename(SubID.SUBID = SubID.code01)
  }
  if('SubId.SubID' %in% names(data)){
    data <- data %>%
      rename(SubID.SUBID = SubId.SubID)
  }
  if('SubID.SubID' %in% names(data)){
    data <- data %>%
      rename(SubID.SUBID = SubID.SubID)
  }

  if('Trials.condition' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.trials_ss = substr(Trials.condition, 3, 3),
             TrialLook.trials_changeside = substr(Trials.condition, 4, 4))
  }

  if('Looking.code01' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.looking_direction = Looking.code01)
  }

  if('Trials.TRIALNUM' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.trials_trialnum = Trials.TRIALNUM)
  }
  if('Trials.TRIALNUMBER' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.trials_trialnum = Trials.TRIALNUMBER)
  }
  if('Trials.TRIAL' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.trials_trialnum = Trials.TRIAL)
  }
  if('Looking.DIRECTION' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.looking_direction = Looking.DIRECTION)
  }
  if('Looking.direction' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.looking_direction = Looking.direction)
  }
  if('Looking.CHANGESIDE' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.trials_changeside = Looking.CHANGESIDE)
  }
  if('Trials.CHANGESIDE' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.trials_changeside = Trials.CHANGESIDE)
  }
  if('Trials.SS' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.trials_ss = Trials.SS)
  }
  if('Looking.ordinal' %in% names(data)){
    data <- data %>%
      mutate(TrialLook.looking_ordinal = Looking.ordinal)
  }

  data2 <- data %>%
    mutate(ID = SubID.SUBID,
           Trial = TrialLook.trials_trialnum,
           Direction = TrialLook.looking_direction,
           ChangeSide = TrialLook.trials_changeside,
           Load = TrialLook.trials_ss)

  samplerate = data2$time[2] - data2$time[1]


  idx <- which(duplicated(names(data)))
  if(length(idx) > 0){
    warning('You have multiple columns with the same name. We shall proceed anyway, but you may wish to remove and try again!')
    data2 <- data2[,-idx]
  }

  dvdata <- data2  %>%
    tidyr::fill(ID) %>%
    mutate(Direction = stringr::str_to_upper(Direction),
           ChangeSide = stringr::str_to_upper(ChangeSide)) %>%
    mutate(.lag = time - lag(time),
           .lag = ifelse(is.na(.lag), samplerate, .lag)) %>% #approximate lag with sample rate if needs be
    mutate(Left = ifelse(Direction == 'R', .lag, 0), #direction switch
           Right = ifelse(Direction == 'L', .lag, 0),
           Duration = ifelse(Left != 0, Left,
                             ifelse(Right != 0, Right, 0))) %>%
    filter(!is.na(Trial)) %>%
    mutate(Look_Ord = TrialLook.looking_ordinal) %>%
    select(-.lag)

  dvdata2 <- dvdata %>%
    select(ID, Trial, ChangeSide, Load, Direction, Duration, Left, Right, Look_Ord) %>%
    # mutate(Looks = 0) %>%
    # mutate(Looks = ifelse(lag(Direction) != Direction, 1, 0)) %>%
    # mutate(Looks = ifelse(is.na(Looks), 0, Looks),
    #        Looks = ifelse(first)) %>%
    group_by(ID, Trial, Look_Ord) %>%
    mutate(Looks = ifelse(row_number()==1, 1, 0),
           Looks = ifelse(is.na(Looks), 0, Looks)) %>%
    ungroup() %>%
    select(-Look_Ord) %>%
    filter(Direction == 'L' | Direction == 'R') %>%
    group_by(ID, Trial, ChangeSide, Load) %>%
    mutate(Switch = 0) %>%
    mutate(Switch = ifelse(lag(Direction) != Direction, 1, 0)) %>%
    mutate(Switch = ifelse(is.na(Switch), 0, Switch)) %>%
    summarise(Left = sum(Left, na.rm = T),
              Right = sum(Right, na.rm = T),
              Switch = sum(Switch, na.rm = T),
              Looks = sum(Looks, na.rm = T),
              TLT = Left + Right) %>% #not sum
    mutate(PercLook = TLT/10000,
           MLD = TLT/Looks,
           SR = Switch/(TLT/1000),
           CP = ifelse(ChangeSide == 'L', Left/TLT, Right/TLT),
           Both = ifelse(Left > 0 & Right > 0, 'Y', 'N'),
           ToCode = 'Coded') %>%
    ungroup() #%>%
    #mutate(ID = stringr::str_remove(ID, '_')) #%>%
    #filter(!is.na(TLT))

  dvdata2 <- dvdata2  %>%
    mutate(Load = as.character(Load),
           Both = as.character(Both))



  return(dvdata2)
}

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
#' @return A formatted dataframe with CP, SwitchRate, MLD and TLT, as well as coding info

create_datavyu_trial_data <- function(data, write = F){

  idx <- which(duplicated(names(data)))
  if(length(idx) > 0){
    warning('You have multiple columns with the same name. We shall proceed anyway, but you may wish to remove and try again!')
    data <- data[,-idx]
  }

  if('TrialLook.trials_trialnum' %in% names(data)){
    data <- data %>%
      rename('TrialLook.trials_trial' = 'TrialLook.trials_trialnum')
  }
  if('TrialLook.looking_direction' %in% names(data)){
    dvdata <- data %>%
      mutate(direction = TrialLook.looking_direction)
  }else{if('TrialLook.looking_code01' %in% names(data)){
    dvdata <- data %>%
      mutate(direction = TrialLook.looking_code01)
  }else{
    dvdata <- data %>%
      mutate(direction = Looking.direction)
  }
  }

  if('ID.run' %in% names(data)){
    dvdata <- dvdata %>%
      rename(SubID.SUBID = ID.run,
             SubID.onset = ID.onset,
             #TrialLook.onset = Looking.onset,
             #TrialLook.offset = Looking.offset,
             TrialLook.trials_trialnum = TrialLook.trials_trial)
  }

  dvsubj <- dvdata %>%
    select(SubID.SUBID, ID.subnumber, SubID.onset, ID.offset) %>%
    rename(TrialLook.onset = SubID.onset) %>%
    filter(!is.na(SubID.SUBID))

  dvdata2 <- dvdata %>%
    fill(ID.subnumber) %>%
    mutate(Trial = TrialLook.trials_trialnum,
           Load = TrialLook.trials_ss,
           ChangeSide = TrialLook.trials_changeside) %>%
    filter(!is.na(Trial)) %>%
    rename(Direction = direction) %>%
    mutate(Direction = stringr::str_to_upper(Direction),
           ChangeSide = stringr::str_to_upper(ChangeSide)) %>%
    mutate(Duration = TrialLook.offset - TrialLook.onset,
           Left = ifelse(Direction == 'R', Duration, 0),
           Right = ifelse(Direction == 'L', Duration, 0))

  adjacent <- dvdata2 %>%
    group_by(ID.subnumber) %>%
    mutate(adj = ifelse(lag(SubID.SUBID) != SubID.SUBID, 'adj', NA)) %>%
    fill(adj, .direction = 'down') %>%
    fill(adj, .direction = 'up') %>%
    ungroup() %>%
    filter(adj == 'adj')

  nonadj <- anti_join(dvdata2, adjacent) %>%
    fill(SubID.SUBID, .direction = 'down')



  #switched perspective
  for(i in 1:nrow(dvdata2)){

    if(length(dvdata2$ChangeSide[i]) > 1){
      warning('Please ensure change side is coded with one letter - you currently have other things in there. Please fix')
    }
  }

  dvdataadj3 <- right_join(dvsubj, adjacent, by = c('ID.subnumber' , 'TrialLook.onset'))

  dvdata33 <- dvdataadj3 %>%
    arrange(ID.subnumber, TrialLook.onset) %>%
    rename(SubID.SUBID = SubID.SUBID.x) %>%
    fill(SubID.SUBID, .direction = 'down')

  #non adjacent type (this is a pain) needs to merge by subID

  dvdatanonadj3 <- right_join(dvsubj, nonadj, by = c('ID.subnumber', 'SubID.SUBID', 'TrialLook.onset'))
  dvdata34 <- dvdatanonadj3 %>%
    arrange(ID.subnumber, SubID.SUBID, TrialLook.onset)

  #join them
  dvdata35 <- bind_rows(dvdata33, dvdata34)

  dvdata4 <- dvdata35 %>%
    filter(!is.na(Duration)) %>%
    rename(ID = SubID.SUBID) %>%
    select(ID, Trial, ChangeSide, Load, Direction, Duration, Left, Right) %>%
    filter(Direction == 'L' | Direction == 'R') %>%
    group_by(ID, Trial, ChangeSide, Load) %>%
    mutate(Switch = 0) %>%
    mutate(Switch = ifelse(lag(Direction) != Direction, 1, 0)) %>%
    mutate(Switch = ifelse(is.na(Switch), 0, Switch)) %>%
    summarise(Left = sum(Left),
              Right = sum(Right),
              Switch = sum(Switch),
              Looks = length(Direction),
              TLT = sum(Left) + sum(Right)) %>%
    mutate(PercLook = TLT/10000,
           MLD = TLT/Looks,
           SR = Switch/(TLT/1000),
           CP = ifelse(ChangeSide == 'L', Left/TLT, Right/TLT),
           Both = ifelse(Left > 0 & Right > 0, 'Y', 'N'),
           ToCode = 'Coded') %>%
    ungroup() %>%
    mutate(ID = stringr::str_remove(ID, '_')) %>%
    filter(!is.na(TLT))

  # allow for no switch data
  dvdata5 <- dvdata35 %>%
    tidyr::fill(SubID.SUBID) %>%
    filter(!is.na(Duration)) %>%
    rename(ID = SubID.SUBID) %>%
    select(ID, Trial, ChangeSide, Load, Direction, Duration, Left, Right) %>%
    group_by(ID, Trial, ChangeSide, Load) %>%
    summarise(Left = sum(Left),
              Right = sum(Right),
              Looks = length(Direction),
              TLT = sum(Left) + sum(Right)) %>%
    mutate(PercLook = TLT/10000,
           MLD = TLT/Looks,
           CP = ifelse(ChangeSide == 'L', Left/TLT, Right/TLT),
           Both = ifelse(Left > 0 & Right > 0, 'Y', 'N'),
           ToCode = 'Coded') %>%
    ungroup() %>%
    mutate(ID = stringr::str_remove(ID, '_'))

  #take only what's missing
  dvdata6 <- anti_join(dvdata5, dvdata4, by = c('ID', 'Trial', 'ChangeSide', 'Load')) %>%
    arrange(ID, Trial) %>%
    filter(TLT == 0)

  dvdata7 <- full_join(dvdata4, dvdata6) %>%
    arrange(ID, Trial)


  if(write == T){
    write_csv(dvdata7, paste('Coded_Data', '.csv', sep = ''))
  }

  return(dvdata7)
}

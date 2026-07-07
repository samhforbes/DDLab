#' run mallet data
#' runs a mallet processing script on a path. If write is true it will write the output
#' @param path a file path from mallet
#' @param write write output files? default is true
#'
#' @export
#' @return a list of files: summary, vwm, pingu

run_mallet_data <- function(path, write = T){

  data_path <- path

  # mllist <- list.files(data_path, pattern = 'MALLET', recursive = F, full.names = T)
  mllist2 <- list.files(data_path, pattern = '\\.txt', recursive = F, full.names = T)
  mllist <- mllist2[grepl("MALLET", mllist2)]

  word(mllist[[1]], -1, sep = '_')
  word(mllist[[1]], 2, sep = '//')
  str_count(word(mllist[[1]], 2, sep = '//'), '_')


  MLfiles <- lapply(mllist, read_video_data)

  for(i in 1:length(MLfiles)){

    MLfiles[[i]]$framerate <- word(mllist[[i]], -1, sep = '_')
    MLfiles[[i]]$trial <- word(mllist[[i]], -2, sep = '_')
    MLfiles[[i]]$ID <- word(mllist[[i]], -4, sep = '_')

    # if(str_count(word(mllist[[i]], 2, sep = '//'), '_') ==7){
    if(grepl("_VWM_", mllist[[i]])){
      # VWM
      MLfiles[[i]]$side <- word(mllist[[i]], -6, sep = '_')
      MLfiles[[i]]$load <- word(mllist[[i]], -8, sep = '_')
      MLfiles[[i]]$task <- 'VWM'
    }else{
      MLfiles[[i]]$task <- 'Pingu'
    }
  }

  time <- bind_rows(MLfiles)
  vwm <- time %>%
    filter(task == 'VWM') %>%
    mutate(framerate = as.numeric(substr(framerate, 1, 2)),
           direction = TrackName,
           timestamp_new = Time * (1000/framerate),
           side = tolower(side),
           side = case_when(side == 'l' ~ 'left',
                            side == 'r' ~ 'right',
                            .default = NA_character_),
           condition = load,
           video_condition = 'video') %>%
    rename(frame = Time)

  vedata <- create_manual_trial_data2(vwm)
  csdata <- create_change_side_data2(vwm)

  csdata2 <- create_change_side_data2(vwm, summary = F)


  number_check <- csdata2 %>%
    group_by(ID, condition) %>%
    summarise(CPC = sum(!is.na(Prop_C)),
              CPNC = sum(!is.na(Prop_NC)),
              CP = sum(!is.na(Prop))) %>%
    ungroup()


  all_dat <- join_CS_to_VE(csdata, vedata)


  pingu <- time %>%
    filter(task == 'Pingu') %>%
    mutate(framerate = as.numeric(substr(framerate, 1, 2)),
           direction = TrackName,
           timestamp_new = Time * (1000/framerate),
           video_condition = 'video') %>%
    rename(frame = Time)

  pin_dat <- create_pingu_data(pingu)

  if(write == T){
    write_csv(number_check, paste(path,'summary_file.csv', sep = '/'))
    write_csv(all_dat, paste(path,'vwm_file.csv', sep = '/'))
    write_csv(pin_dat, paste(path,'pingu_file.csv', sep = '/'))
  }
  fils <- list(number_check, all_dat, pin_dat)
  return(fils)
  message('processing complete')
}

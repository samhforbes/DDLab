#' Downsample a frame-by-frame eyetracking output - ie a sample report
#'
#' This downsamples the values, taking means across time bins
#'
#' @param data an eyetracking sample report
#' @param time_bin_size the size of the desired timebin, in ms
#' @param sample_rate the current sample rate of the original output
#'
#' @examples
#' library(readr)
#' data <- read_csv('eyetracking_sample.csv')
#' data_out <- downsample(data, 100, 2)
#'
#' @export
#'
#' @return A downsampled eyetracking sample report

downsample <- function(data, time_bin_size, sample_rate = 2){
  library(dplyr, verbose = F)


  if(is.null(time_bin_size)){
    stop('Please supply a time bin size!')
  }
  #data <- read.table(data, sep = '\t', header = T)

  samples_per_bin <- (sample_rate/1000) * time_bin_size

  srate <- data[['TIMESTAMP']][2] - data[['TIMESTAMP']][1]

  if (time_bin_size %% srate != 0){
    warning('sample frequency is not a multiple of the target frequency. I will do it anyway, but you may wish to rethink your choices!')
  }

  data$DS <- data$TIMESTAMP %/% time_bin_size
  data$DS <- data$DS * time_bin_size

  data2 <- data %>%
    group_by(DS) %>%
    distinct(RECORDING_SESSION_LABEL, trial, DS, .keep_all = T) %>%
    ungroup()

  if(TRUE %in% (duplicated(data2$TIMESTAMP)) == TRUE){
    warning('There may be duplicate values in the time series.')
  }


  return(data2)
}

#' Merge eyetracking and datavyu data
#'
#' Both files should first have been processed with the relevant commands
#'
#' @param ETdata a file of processed eyetracking data
#' @param datavyudata processed output from datavyu file
#' @param write if TRUE will write a file directly to the current wd
#'
#' @examples
#' library(readr)
#' data <- read_csv("etdata.csv")
#' data_out <- create_ET_trial_data(data, task = 'VWM', write = F)
#' data2 <- read_csv("datavyudata.csv")
#' data2_out <- create_datavyu_trial_data(data2, write = F)
#' data3 <- merge(data_out, data2_out, write = F)
#'
#' @export
#'
#' @return a merged dataframe where eyetracking data is replaced with look coded data where necessary


merge_ET_datavyu <- function(ETdata, datavyudata, write = FALSE){

  ET <- ETdata
  DV <- datavyudata

  all <- full_join(DV, ET)

  all2 <- all %>%
    arrange(ID, Trial) %>%
    group_by(ID, Trial) %>%
    mutate(remove = ifelse(ToCode == 'Coded' & lead(ToCode) == 'N',
                            'Y', 'N')) %>%
    filter(remove != 'Y') %>%
    mutate(remove = ifelse(ToCode == 'Y' & lag(ToCode) == 'Coded',
                           'Y', 'N')) %>%
    mutate(remove = ifelse(is.na(remove), 'N', remove)) %>%
    filter(remove != 'Y')

  all2$remove <- NULL

  if(write == T){
    write_csv(all2, paste('Merged_Data', '.csv', sep = ''))
  }

  return(all2)

}

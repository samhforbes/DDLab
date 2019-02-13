read_in_caps <- function(digipath, pattern, capsize, type = c('NIHVWM', 'Gates', 'HWB')){
  capsize <- read.csv(capsize)
  names(capsize)[c(1,2,3)] <- c('ID', 'cap', 'head')
  if(type == 'NIHVWM'){
  capsize$age <- ifelse(grepl('30NIHVWM', capsize$ID), '30', '06')
  }
  if(type == 'HWB'){
    capsize$age <- ifelse(grepl('32HWB', capsize$ID), '32', '54')
  }
 
  file.list <- list.files(path = digipath, pattern = pattern, recursive = TRUE)
  #make into dataframe for merging
  dataframe <- data.frame(file.list)
  if(type =='NIHVWM'){
    dataframe$ID <- substr(dataframe$file.list, 1, 12)
  }
  if(type =='Gates'){
    dataframe$ID <- substr(dataframe$file.list, 1, 9)
  }
  if(type =='HWB'){
    dataframe$ID <- substr(dataframe$file.list, 1, 11)
  }
  fulldata <- merge(dataframe, capsize, by = 'ID', all = T)
  
  no_nirs <- subset(fulldata, is.na(fulldata$file.list))
  no_caps <- subset(fulldata, is.na(fulldata$cap))
  
  test_data <- fulldata %>% 
    filter(!is.na(file.list)) %>%
    filter(!is.na(cap))
  #file.list <- subset(file.list.30, !grepl('anatomical', file.list.30))
  
  test_data$file.list <- as.character(test_data$file.list)
  
  full_list <- list()
  for (j in 1:length(unique(test_data$cap))){
    # read in function
    datalist <- subset(test_data, test_data$cap == unique(test_data$cap)[j])
    
    sub_list <- list()
    for (i in 1:nrow(datalist)){
      sub_list[[i]] <- read.table(paste(digipath, '/', datalist$file.list[i], sep = ''))
      names(sub_list)[[i]] <- as.character(datalist$file.list[i])
    }
    full_list[[j]] <- sub_list
    names(full_list)[[j]] <- paste('capsize', unique(test_data$cap)[j], sep = '')
  }
  return(full_list)
}
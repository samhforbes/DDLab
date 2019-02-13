calc_3d_dist <- function(point0, point1){
  dist = c()
  for(i in 1:nrow(point1)){
    x0 <- point0[i,1]
    y0 <- point0[i,2]
    z0 <- point0[i,3]
    
    x1 <- point1[i,1]
    y1 <- point1[i,2]
    z1 <- point1[i,3]
    
    dist[i] <- sqrt(((x1-x0)^2) + ((y1 - y0)^2) + ((z1-z0)^2))
  }
  return(dist)
}

calc_dist_and_replace <- function(template_index, data, max){
  #calculate distances from a template index
  template <- list()
  for(i in 1:length(template_index)){
    ind <- as.numeric(template_index[[i]])
    
    template[[i]] <- data[[i]][[ind]]
  }
  
  fulldist <- data
  for(i in 1:length(fulldist)){
    for(j in 1:length(fulldist[[i]])){
      fulldist[[i]][[j]] <- calc_3d_dist(template[[i]], fulldist[[i]][[j]])
    }
  }
  
  # and remove bad points
  corrected <- data
  for(i in 1:length(fulldist)){
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:length(fulldist[[i]][[j]]))
        if(fulldist[[i]][[j]][k] > max){
          corrected[[i]][[j]][k,] <- template[[i]][k,]
        }
    }
  }
  return(corrected)
}

calc_dist_and_replace_template <- function(template, data, max){
  #calculate distances from a template prespecified
  #template <- list()
  #for(i in 1:length(template_index)){
  #  ind <- as.numeric(template_index[[i]])
  #  
  #  template[[i]] <- data[[i]][[ind]]
  #}
  
  fulldist <- data
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      fulldist[[i]][[j]] <- calc_3d_dist(template[[i]], fulldist[[i]][[j]])
    }
  }
  
  # and remove bad points
  a <- 0
  corrected <- data
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:length(fulldist[[i]][[j]]))
        if(fulldist[[i]][[j]][k] > max){
          a <- a +1
          corrected[[i]][[j]][k,] <- template[[i]][k,]
        }
    }
  }
  cat(a)
  return(corrected)
}

rezero_template <- function(template){
  data <- template
  if(length(data) == 0) next
  for(i in 1:length(data)){
    data[[i]]$z <- data[[i]]$z - data[[i]]$z[4]
    
    #inbuilt sanity check
    for(j in 1:nrow(data[[i]])){
      if(data[[i]]$y[j] > (data[[i]]$y[2] + 1.5)){
        data[[i]]$y[j] <- (data[[i]]$y[2] + 1.5)
      }
      if(data[[i]]$y[j] < (data[[i]]$y[3] - 1.5)){
        data[[i]]$y[j] <- (data[[i]]$y[3] - 1.5)
      }
      if(data[[i]]$x[j] < (data[[i]]$x[5] - 1)){
        data[[i]]$x[j] <- (data[[i]]$x[5] - 1)
      }
    }
  }
  return(data)
}

calc_dist_and_replace_na <- function(template, aligned_data, original_data, max){
  #calculate distances from a template prespecified
  #template <- list()
  #for(i in 1:length(template_index)){
  #  ind <- as.numeric(template_index[[i]])
  #  
  #  template[[i]] <- data[[i]][[ind]]
  #}
  
  fulldist <- aligned_data
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      fulldist[[i]][[j]] <- calc_3d_dist(template[[i]], fulldist[[i]][[j]])
    }
  }
  
  # and remove bad points
  corrected <- original_data
  a <- 0
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:length(fulldist[[i]][[j]]))
        if(fulldist[[i]][[j]][k] > max){
          corrected[[i]][[j]][k,] <- NA
          a <- a + 1
        }
    }
  }
  cat(a)
  return(corrected)
}

replace_nas_with_template <- function(template, data){
  
  
  corrected <- data
  for(i in 1:length(data)){
    if(length(data[[i]]) == 0) next
    for(j in 1:length(data[[i]])){
      for(k in 1:nrow(data[[i]][[j]]))
        if(is.na(data[[i]][[j]][k, 1])){
          corrected[[i]][[j]][k,1:3] <- template[[i]][k,1:3]
        }
    }
  }
  return(corrected)
}

calc_participant_numbers <- function(data){
  a <- 0
  for(i in 1:length(data)){
      a <- a + length(data[[i]])
  }
  return(a)
}

rezero_template_ind <- function(template){
  data <- template
  if(length(data) == 0) next
  for(i in 1:length(data)){
    
    #inbuilt sanity check
    for(j in 1:nrow(data[[i]])){
      if(data[[i]]$y[j] > (data[[i]]$y[2] + 1.5)){
        data[[i]]$y[j] <- (data[[i]]$y[2] + 1.5)
      }
      if(data[[i]]$y[j] < (data[[i]]$y[3] - 1.5)){
        data[[i]]$y[j] <- (data[[i]]$y[3] - 1.5)
      }
      if(data[[i]]$x[j] < (data[[i]]$x[5] - 1)){
        data[[i]]$x[j] <- (data[[i]]$x[5] - 1)
      }
    }
  }
  return(data)
}
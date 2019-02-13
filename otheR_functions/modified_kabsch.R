modified_kabsch <- function(pm, qm, om) {
  pm_dims <- dim(pm)
  if (!all(dim(qm) == pm_dims)) {
    stop(call. = TRUE, "Point sets must have the same dimensions")
  }
  # The rotation matrix will have (ncol - 1) leading ones in the diagonal
  diag_ones <- rep(1, pm_dims[2] - 1)
  
  # center the points
  pm <- scale(pm, center = TRUE, scale = FALSE)
  qm <- scale(qm, center = TRUE, scale = FALSE)
  
  am <- crossprod(pm, qm)
  
  svd_res <- svd(am)
  # use the sign of the determinant to ensure a right-hand coordinate system
  d <- determinant(tcrossprod(svd_res$v, svd_res$u))$sign
  dm <- diag(c(diag_ones, d))
  
  # rotation matrix
  um <- svd_res$v %*% tcrossprod(dm, svd_res$u)
  
  #om needs to be a matrix
  om <- as.matrix(om)
  # Rotate and then translate to the original centroid location of pm
  sweep(t(tcrossprod(um, om)), 2, -attr(pm, "scaled:center"))
}

align_caps_to_best_guess <- function(template_index, data, num_aligned){
  #cut the garbage
  cut_list <- lapply(data, function(x) 
    lapply(x, function(y) select(y, one_of(c('V2', 'V3', 'V4')))))
  
  #get the templates
  new_data <- list()
  for(i in 1:length(template_index)){
    a <- as.numeric(template_index[[i]])
    new_data[[i]] <- cut_list[[i]][[a]]
  }
  
  #align it
  land_norm <- cut_list
  for(i in 1:length(land_norm)){
    
    for(j in 1:length(land_norm[[i]])){
      
      land_norm[[i]][[j]][,1:3] <- modified_kabsch(new_data[[i]][1:num_aligned,1:3], land_norm[[i]][[j]][1:num_aligned,1:3], land_norm[[i]][[j]][,1:3])
      
    }
  }
  return(land_norm)
}


align_all_caps_nested <- function(data, num_aligned){
  cut_list <- lapply(data, function(x) 
    lapply(x, function(y) select(y, one_of(c('V2', 'V3', 'V4')))))
  
  #align it
  land_norm <- cut_list
  land_norm2 <- land_norm
  for(i in 1:length(land_norm)){
    for(j in 1:length(land_norm[[i]])){
      sub_list <- list()
      land_norm2[[i]][[j]] <- land_norm[[i]]
      land_norm2[[i]][[j]][[j]] <- land_norm[[i]][[j]]
      for(k in 1:length(land_norm2[[i]][[j]])){
        
        land_norm2[[i]][[j]][[k]] <- data.frame(modified_kabsch(cut_list[[i]][[j]][1:num_aligned, 1:3], land_norm2[[i]][[j]][[k]][1:num_aligned, 1:3], land_norm2[[i]][[j]][[k]][,1:3]))

      }
    }
  }
  return(land_norm2)
}

align_to_template <- function(template, data, num_aligned){
  if(ncol(data[[1]][[1]]) > 3){
  normalised <- lapply(data, function(x) 
    lapply(x, function(y) y[,2:4]))
  } else{normalised <- data}
  
  for(i in 1:length(normalised)){
    if(length(normalised[[i]]) == 0) next
    for(j in 1:length(normalised[[i]])){
      if(any(is.na(normalised[[i]][[j]][1:num_aligned,])) == TRUE){
        normalised[[i]][[j]][1:num_aligned,] <- template[[i]][1:num_aligned,]
      } 
        normalised[[i]][[j]] <- data.frame(modified_kabsch(template[[i]][1:num_aligned,], normalised[[i]][[j]][1:num_aligned,], normalised[[i]][[j]]))
      
    }
  }
  
  n <- c('x', 'y', 'z')
  
  normalised <- lapply(normalised, function(x)
    lapply(x, setNames, n))
}
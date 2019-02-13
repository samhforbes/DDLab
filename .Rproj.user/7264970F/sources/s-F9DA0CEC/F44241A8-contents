#' transform polhemus data into digpoints for AtlasViewerGUI
#'
#' This works with NIH, Gates, and HWB data. If you have different data then contact Sam
#'
#' @param filename A string denoting name of the file
#' @param wd A string denoting working directory
#' @param option Experiment setup cap configuration, from NIH, Gates, or HWB
#' @param out_folder A string denoting out folder name, by default within wd
#'
#' @examples
#' transform_polhemus('06NIHVWM111B', 'Desktop/Digitization/'m option = 'NIH', out_folder = 'DigTest')
#'
#' @export
#'
#' @return A saved digpts file in the out_folder


transform_polhemus <- function(filename, wd, option = c('NIH', 'Gates', 'HWB'), out_folder = 'DigTest'){

  points <- read.table(paste(wd, filename, sep = ''), header = F)

  if(option == 'Gates'){
    points <- points[c(2:4)]
    points2 <- apply(points, 2, function(x) x*10)

    landmarks <- c('nz:', 'ar:', 'al:', 'cz:', 'iz:')
    sources <- c(paste('s', seq(1,12), ':', sep = ''))
    detectors <- c(paste('d', seq(1,20), ':', sep = ''))

    setup <- c(landmarks, sources, detectors)

    if(nrow(points2) != 37){
      stop(paste('Incorrect number of rows for digitization. Did you mean to select one of the other options? Gates option requires 37 points, you have', nrow(points2)))
    }
    out <- cbind(setup, points2)

  }
    if(option == 'HWB'){
      points <- points[c(2:4)]
      points2 <- apply(points, 2, function(x) x*10)

      setup<- c('nz:', 'ar:', 'al:', 'cz:', 'iz:', paste('s', 1:8, ':', sep =''),
            paste('d', 1:16, ':', sep = ''))

      setup <- as.character(setup)
      if(nrow(points2) != 29){
        stop(paste('Incorrect number of rows for digitization. Did you mean to select one of the other options? HWB option requires 29 points, you have', nrow(points2)))
      }
      out <- cbind(setup, points2)

    }
    if(option == 'NIH'){
      null_option <- c(0,0,0)

      points <- points[c(2:4)]
      points2 <- data.frame(apply(points, 2, function(x) x*10))

      new_points <- rbind(points2[1:23,], null_option,
                          points2[24:30,], null_option,
                          points2[31:37,], null_option,
                          points2[38:44,], null_option,
                          points2[45:49,])

      landmarks <- c('nz:', 'ar:', 'al:', 'cz:', 'iz:')
      sources <- c(paste('s', seq(1,16), ':', sep = ''))
      detectors <- c(paste('d', seq(1,32), ':', sep = ''))

      setup <- c(landmarks, sources, detectors)

      nummy <- nrow(new_points)
      if(nummy != 53){
        stop(paste('Incorrect number of rows for digitization. Did you mean to select one of the other options? NIH option requires 53 points (49 digitized), you have', nrow(new_points)))
      }else{
      out <- cbind(setup, new_points)
      }

  }
  cat(paste('File successfully transformed and saved as digpts.txt in', out_folder, 'subfolder'))

  write.table(out, paste(wd, out_folder,'/digpts.txt', sep = ''), row.names = F,
              col.names = F, quote = F, sep = '\t')
}

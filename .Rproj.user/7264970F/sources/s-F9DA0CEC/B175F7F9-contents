#' Run a kappa test on preferential looking data to test for reliability in video coding from datavyu
#' @param data a data frame, or preferentially a tibble
#' @param Subject1 a column of directions coding by Subject 1
#' @param Subject2 a column of directions coding by Subject 2
#' @param Trials a column of
#' @examples
#' data <- read_csv(mydata.csv)
#' reliability_test(data, 'LOOKING.DIRECTION', 'Looking_JG2.DIRECTION', 'TRIALS.SS')
#'
#' @export
#'
#' @return Kappa test output of coding reliability


reliability_test <- function(data, Subject1, Subject2, Trials){


#data <- read_csv('088G.csv')

# Subject1 <- 'LOOKING.DIRECTION'
# Subject2 <- 'Looking_JG2.DIRECTION'
# Trials <- 'TRIALS.SS'

data <- data %>% select(framenum, time, !!Trials, !!Subject1, !!Subject2) %>%
  filter(!is.na(Trials))

#Look for change of direction
data <- data %>% mutate(lag_S1 = ifelse(data[[Subject1]] == lag(data[[Subject1]]), 1, 0))
data <- data %>% mutate(lag_S2 = ifelse(data[[Subject2]] == lag(data[[Subject2]]), 1, 0))

#Take 5 frames before and 5 after
changes_S1 <- data$framenum[data$lag_S1 == 0]
changes_S1 <- c(changes_S1, changes_S1 + 1 , changes_S1 + 2, changes_S1 + 3 , changes_S1 + 4 , changes_S1 + 5,
                changes_S1 - 1, changes_S1 - 2, changes_S1 - 3, changes_S1 - 4, changes_S1 - 5)

changes_S2 <- data$framenum[data$lag_S2 == 0]
changes_S2 <- c(changes_S2, changes_S2 + 1 , changes_S2 + 2, changes_S2 + 3 , changes_S2 + 4 , changes_S2 + 5,
                changes_S2 - 1, changes_S2 - 2, changes_S2 - 3, changes_S2 - 4, changes_S2 - 5)


extra <- changes_S2[changes_S2 %in% changes_S1 == F]

changes <- c(changes_S1, extra)

S1 <- data %>% filter(framenum %in% changes) %>%
  filter(!is.na(framenum))

# alternative calculation
#S2 <- data %>% filter(framenum %in% changes_S2) %>%
#  filter(!is.na(framenum))

#data2 <- merge(S1, S2, by = cbind('framenum', 'time', Trials, Subject1, Subject2), all.x = T, all.y = T)

data2 <- S1 %>% select('framenum', 'time', !!Trials, !!Subject1, !!Subject2)

data2 <- data2 %>% filter(!is.na(time))

#fix case
data3 <- data.frame(lapply(data2, function(x) {
  if(is.character(x)) return(toupper(x))
  else(return(x))
}))

data3 <- data3 %>% arrange(framenum)

output <- data3 %>% select(!!Subject1, !!Subject2) %>% irr::kappa2(weight = 'unweighted')
#alternative output with fmsb
#output <- fmsb::Kappa.test(x = data3[[Subject1]], y = data3[[Subject2]])

if(output$value < .4) Agreement <- 'Little agreement'
if(output$value >=.4 & output$value < .6) Agreement <- 'Moderate agreement'
if(output$value >=.6 & output$value < .8) Agreement <- 'Substantial agreement'
if(output$value > .8) Agreement <- 'Almost perfect agreement'

cat(' **********************************', '\n', Agreement, '\n', '**********************************', '\n')

return(output)
}
#Kappa .780 p = .000 ASE 0.021
#2676
#.4 - .6 moderate, .6 - .8 substantial, >.8 almost perfect



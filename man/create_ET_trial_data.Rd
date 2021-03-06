% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_ET_trial_data.R
\name{create_ET_trial_data}
\alias{create_ET_trial_data}
\title{create a trial report from a fixation eyetracking data}
\usage{
create_ET_trial_data(data, task, write = F, show_all_missing = F)
}
\arguments{
\item{data}{a dataframe read in from a fixation report CSV or txt}

\item{task}{in quotes, a string regarding the task you are interested in from the task column}

\item{write}{if TRUE will save a csv in the current working directory}

\item{show_all_missing}{if T will assume 18 trials per participant and leave blank rows}
}
\value{
A formatted dataframe with CP, SwitchRate, MLD and TLT, as well as coding info.
}
\description{
This was designed to work with eyelink fixation reports and the VWM trial.
I can't guarantee it will bring out what you want beyond that so please check
the output carefully.
}
\examples{
library(readr)
data <- read_csv("etdata.csv")
data_out <- create_ET_trial_data(data, task = 'VWM', write = F)

}

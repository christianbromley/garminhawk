#' @title garminhawk
#'
#' @description A series of functions to process, analyse and plot Garmin Connect data.
#'
#' @param data downloaded from Garmin Connect in .csv format and read into R using read.csv or read.table for example.
#'
#' @return dataframe
#'
#' @examples
#' #let's process my data so that it's good for R, and tidy for plotting
#' my_runs <- processGarminRunning(data=garmin)
#' summary(my_runs)
#'
#' @export

data <- garmin
processGarminRunning <- function(data){

  #check if the required packages are installed
  packages = c("tidyverse", "lubridate")

  ## Now load or install
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )

  #check if it's a data frame
  if(is.matrix(data)){
    data <- as.data.frame()
  }

  if(is.data.frame(data)){

    #get running data
    garmin <- data %>% filter(Activity.Type == "Running")

    #now drop variables we aren't interested in
    garmin <- garmin %>%
      dplyr::select(-Max.Temp, -Decompression, -Surface.Interval, -Min.Temp, -Bottom.Time, -Flow, -Training.Stress.Score., -Avg.Vertical.Oscillation, -Avg.Vertical.Ratio)

    # format Date column to POSIXct
    garmin$Date <- as.POSIXct(strptime(garmin$Date, format = "%Y-%m-%d %H:%M:%S"))

    # format Avg.Pace to POSIXct
    garmin$Avg.Pace <- as.POSIXct(strptime(garmin$Avg.Pace, format = "%M:%S"))

    # format Time to POSIXct
    garmin$Time <- as.POSIXct(strptime(garmin$Time, format = "%H:%M:%S"))
    garmin$Best.Lap.Time <- as.POSIXct(strptime(garmin$Best.Lap.Time, format = "%H:%M.%S"))

    #convert calories to numeric
    garmin$Calories <- gsub(",","",garmin$Calories)
    garmin$Calories <- gsub("--", NA, garmin$Calories)
    garmin$Calories <- as.numeric(garmin$Calories)

    #convert cadence to a numeric
    garmin$Avg.Run.Cadence <- gsub("--", NA, garmin$Avg.Run.Cadence) %>% as.numeric()
    garmin$Max.Run.Cadence <- gsub("--", NA, garmin$Max.Run.Cadence) %>% as.numeric()

    #make Best Pace variable a POSIXCT
    garmin$Best.Pace <- as.POSIXct(strptime(garmin$Best.Pace, format = "%M:%S"))

    #turn elevation gains into as numeric
    garmin$Elev.Gain <- gsub("--", NA, garmin$Elev.Gain) %>% as.numeric()
    garmin$Elev.Loss <- gsub("--", NA, garmin$Elev.Loss) %>% as.numeric()

    #make climb time variable a POSIXCT
    garmin$Climb.Time <- as.POSIXct(strptime(garmin$Climb.Time, format = "%M:%S"))

    # factorise the distances travelled
    garmin <- garmin %>%
      mutate(distance_grouping = factor(case_when(Distance <= 1 ~ "0-1 mile",
                                                  Distance > 1 & Distance <= 2 ~ "1-2 miles",
                                                  Distance > 2 & Distance <= 3 ~ "2-3 miles",
                                                  Distance > 3 & Distance <= 4 ~ "3-4 miles",
                                                  Distance > 4 & Distance <= 5 ~ "4-5 miles",
                                                  Distance > 5 & Distance <= 10 ~ "5-10 miles",

                                                  Distance > 10 & Distance <= 15 ~ "10-15 miles",
                                                  Distance > 15 & Distance <= 20 ~ "15-20 miles",
                                                  Distance > 20 ~ ">20 miles"), levels=c("0-1 mile", "1-2 miles",
                                                                                         "2-3 miles", "3-4 miles",
                                                                                         "4-5 miles", "5-10 miles",
                                                                                         "10-15 miles", "15-20 miles",
                                                                                         ">20 miles")))
    #deal with number of laps
    garmin$Number.of.Laps <- as.numeric(garmin$Number.of.Laps)

    #if there are any bugs we will be able to tell by the Calories - remove rows in which calories are NA
    #or if the distance is very small
    if(sum(is.na(garmin$Calories)) > 0){
      garmin <- garmin[-which(is.na(garmin$Calories)),]
    }

    if(min(garmin$Distance) < 0.1){
      garmin <- garmin[-which(garmin$Distance < 0.1),]
    }

    #reaffirm factor variables
    garmin$Title <- as.factor(as.character(garmin$Title))

    #change location
    garmin$location <- word(garmin$Title, 1) %>% as.character() %>% as.factor()

    garmin$location <- factor(ifelse(garmin$location == "Running", "Unknown", as.character(garmin$location)))

    #edit heart rate
    garmin$Avg.HR <- gsub("--", NA, garmin$Avg.HR) %>% as.numeric()
    garmin$Max.HR <- gsub("--", NA, garmin$Max.HR) %>% as.numeric()




    garmin <- as.data.frame(garmin)

  }else{
    stop("is neither a data frame nor a matrix. Try reading in the Garmin Connect output .csv file again")
  }

  return(garmin)

}







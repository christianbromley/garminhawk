#' @title plotGarminRunning
#'
#' @description A function to generate stylish plots that rapidly summarise your running performance.
#'
#' @param output from the processGarminRunning function
#' @param date_from a cut-off for runs that you wish to look at in the format e.g as.Date("2020-03-18")
#' @param plot can be one of "distance", "pace", "dist_pace", "dist_time", "dist_cals", "cumulative_dist",
#' "pace_per_dist.gp","heart.rate_dist.gp","cadence_dist.gp"
#' @param target_pace your half marathon time for example or a target pace that you want to run at in the format "8:00" "minutes:seconds".
#' @return plot
#'
#' @examples
#' #let's process my data so that it's good for R, and tidy for plotting
#' my_runs <- processGarminRunning(data=garmin)
#'
#' # I'm firstly interested in how far I have ran since lockdown (COVID19 induced)
#' plotGarminRunning(my_runs,
#'                   plot = "cumulative_dist",
#'                 date_from = as.Date("2020-03-18"))
#'
#' # I can now clearly visualise how far I have run but what are the exact distances of those individual runs?
#' plotGarminRunning(my_runs,
#'                  plot = "dist_time",
#'                  date_from = as.Date("2020-03-18"),
#'                  target_time = "7:18")
#'
#' #there is a minor trend for runs to increase in distance particularly since moving to Manchester.
#' #early on during lockdown I ran a lot of the same run you'll notice - around 2.5m miles
#'
#' #we can separate by location to look at the distance of the runs
#'
#' plotGarminRunning(my_runs,
#'                 plot = "distance",
#'                  date_from = as.Date("2020-03-18"))
#'
#' #there is a great degree of variation amongst the manchester runs - for some reason salford and manchester are separated here
#'
#' #what about pace? How fast have I been running? Compare this to my per mile half marathon time
#' plotGarminRunning(my_runs,
#'                  plot = "pace",
#'                 date_from = as.Date("2020-03-18"),
#'                  target_time = "7:18")
#'
#' #You'll notice that on my longer runs I am way behind my half marathon pace - oh dear!
#' #There are a bunch of short distance runs in which I'm considerably under.
#'
#' plotGarminRunning(my_runs,
#'                  plot = "dist_pace",
#'                  date_from = as.Date("2020-03-18"),
#'                  target_time = "7:18")
#'
#' #we can also separate according to distance grouping to monitor our face. Seemingly there are a bunch of short distance, but slow runs between 1 and 2 miles.
#' #lots of variation in the 3-4 mile range
#' plotGarminRunning(my_runs,
#'                  plot = "pace_per_dist.gp",
#'                  date_from = as.Date("2020-03-18"),
#'                  target_time = "7:18")
#'
#' #what about calories? how many calories am I burning in these runs?
#' plotGarminRunning(my_runs,
#'                  plot = "dist_cals",
#'                  date_from = as.Date("2020-03-18"))
#'
#'
#' #very strong relationship between calories and distance run - strange wobble at the bottom  may be related to pace!
#' #what about heart rate?
#' plotGarminRunning(my_runs,
#'                  plot = "heart.rate_dist.gp",
#'                  date_from = as.Date("2020-03-18"))
#'
#' # you'll notice in the longer runs that the average heart rate for each run is above the median value of average heart rates across all runs.
#'
#' #What about cadence?
#' plotGarminRunning(my_runs,
#'                 plot = "cadence_dist.gp",
#'                 date_from = as.Date("2020-03-18"))
#'
#' @export



plotGarminRunning <- function(data,
                              date_from = NA,
                              plot = "distance",
                              target_time = median(data$Avg.Pace)){

  if(!plot %in% c("distance", "pace", "dist_pace", "dist_time", "dist_cals", "cumulative_dist","pace_per_dist.gp","heart.rate_dist.gp","cadence_dist.gp","dist_time.bar")){
    stop("The plot you request is not part of the repertoire")
  }

  #check if the required packages are installed
  packages = c("tidyverse", "lubridate", "RColorBrewer", "ggridges", "dplyr","ggpubr")

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

  #get some colours
  pal = c(brewer.pal(8, "Dark2"), brewer.pal(8,"Paired"))
  pal2 = c(brewer.pal(11, "Spectral"))

  #get the runs SINCE the date specified
  if(!is.na(date_from)){

    data <- data[which(data$Date > date_from),]
  }

  data$location <- as.factor(as.character(data$location))

  if(plot == "distance"){
    # distance
    plot = ggplot(data, aes(x=Distance, y=location, fill=location))+
      geom_density_ridges(scale=1, jittered_points=TRUE, point_size=2, point_alpha=0.8,point_colour="white")+
      scale_fill_manual(values=pal)+
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        plot.title = element_text(colour="white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title.x = element_text(colour="white"),
        axis.title.y=element_blank(),
        axis.line = element_line(colour="white"),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white"),
        legend.position = c(0.8, 0.9)
      )+
      labs(x="Distance (miles)", title="Run distance density")


  }

  if(plot == "pace"){
    # average pace
    plot = ggplot(data, aes(x = Date,y = Avg.Pace, colour = distance_grouping)) +
      geom_point(size=5, alpha=0.8) +
      scale_colour_manual(values=pal2)+
      scale_y_datetime(date_labels = "%M:%S") +
      #scale_x_date(date_labels = "%Y-%M-%d")+
      geom_smooth(color = "yellow", se=FALSE) +
      labs(x = "Date", y = "Average Pace (min/km)", title = "Average pace") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        #legend.position = c(0.3, 0.8),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      labs(x="", y="Average Pace (min/mile)")+
      geom_hline(yintercept = as.POSIXct(strptime(target_time, format = "%M:%S")), linetype="dashed", colour="white")
      #annotate("text", label = "Target", xmin= min(data$Data), ymin=as.POSIXct(strptime(target_time, format = "%M:%S")), colour = "white")


  }

  if(plot == "dist_pace"){
    plot = ggplot(data, aes(Distance, Avg.Pace))+
      geom_point(aes(colour=location), size=5, alpha=0.8) +
      scale_colour_manual(values=pal)+
      scale_y_datetime(date_labels = "%M:%S")+
      geom_smooth(colour="yellow",se=FALSE)+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title.x = element_text(colour="white"),
        axis.title.y=element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white"),
        #legend.position = c(0.8, 0.6)
      )+
      labs(x="Distance (miles)", y="Average Pace (min/mile)",title="Distance - pace relationship")+
      geom_hline(yintercept = as.POSIXct(strptime(target_time, format = "%M:%S")), linetype="dashed", colour="white")
  }

  if(plot =="dist_time"){
    plot = ggplot(data, aes(x = Date,y = Distance, colour = distance_grouping)) +
      geom_point(size=5, alpha=0.8, aes(shape = location)) +
      #geom_segment(x=Date, y=0, yend=Distance, colour="white")+
      scale_colour_manual(values=pal2)+
      geom_smooth(color = "yellow", se=FALSE) +
      labs(x = "Date", y = "Average Pace (min/km)") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        #legend.position = c(0.3, 0.8),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(fill = NA, colour = "#333333"),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      labs(x="", y="Distance (miles)", title = "Run distance scatter plot")

  }

  if(plot =="dist_time.bar"){
    plot = ggplot(data, aes(x = Date,y = Distance)) +
      geom_bar(aes(fill=distance_grouping), stat="identity") +
      scale_fill_manual(values=pal2)+
      geom_smooth(color = "yellow", se=FALSE) +
      labs(x = "Date", y = "Average Pace (min/km)") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        #legend.position = c(0.3, 0.8),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      labs(x="", y="Distance (miles)",title="Run distances")

  }

  if(plot=="dist_cals"){

    plot = ggplot(data, aes(Distance, Calories))+
      geom_point(aes(colour=location), size=5, alpha=0.8) +
      scale_colour_manual(values = pal)+
      #scale_y_datetime(date_labels = "%M:%S")+
      geom_smooth(colour="yellow",se=FALSE)+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title.x = element_text(colour="white"),
        axis.title.y=element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white"),
        legend.position = c(0.8, 0.2)
      )+
      labs(x="Distance (miles)", y="Calories", title="Calories by distance")

  }

  if(plot == "cumulative_dist"){

    data <- data %>%
      arrange(Date) %>%
      mutate(cumulative_dist = cumsum(Distance))


    plot = ggplot(data, aes(Date, cumulative_dist))+
      geom_point(aes(colour=location), size=5, alpha=0.8) +
      scale_colour_manual(values=pal)+
      geom_line(colour="yellow")+
      geom_hline(yintercept = 100, linetype="dashed", colour="yellow")+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        plot.title = element_text(colour="white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title.x = element_text(colour="white"),
        axis.title.y=element_text(colour="white"),
        axis.line = element_line(colour="white"),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white"),
        legend.position = c(0.8, 0.2)
      )+
      labs(x="Date", y="Cumulative distance (miles)", title="Cumulative running distance")



  }

  if(plot == "pace_per_dist.gp"){


    plot = ggplot(data, aes(x = Date,y = Avg.Pace, colour = location)) +
      geom_point(size=5, alpha=0.8) +
      scale_colour_manual(values=pal)+
      scale_y_datetime(date_labels = "%M:%S") +
      #geom_smooth(color = "yellow", se=FALSE) +
      labs(x = "Date", y = "Average Pace (min/km)", title = "Average pace per distance grouping") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        panel.border = element_rect(colour="white", size=1),
        #legend.position = c(0.3, 0.8),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      labs(x="Distance (miles)", y="Average Pace (min/mile)")+
      geom_hline(yintercept = as.POSIXct(strptime(target_time, format = "%M:%S")), linetype="dashed", colour="white")+
      facet_wrap(.~distance_grouping)
  }

  if(plot == "heart.rate_dist.gp"){

    dlong <- data %>%
      dplyr::select(Date, Avg.HR, Max.HR, distance_grouping) %>%
      pivot_longer(cols=c(Avg.HR, Max.HR),
                   values_to = "hr",
                   names_to = "avg.max")
    dlong$hr <- as.numeric(dlong$hr)
    dlong$Date <- as.POSIXct(strptime(dlong$Date, format = "%Y-%m-%d %H:%M:%S"))


    plot = ggplot(dlong, aes(x = Date,y = hr, colour = avg.max)) +

      geom_line(aes(group = Date),colour="yellow",size=1)+
      geom_point(size=6, alpha=0.8) +
      scale_colour_manual(values=pal)+

      labs(x = "Date", y = "Heart rate",title="Heart rate by grouping") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        plot.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        panel.border = element_rect(colour="white", size=1),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      geom_hline(yintercept = mean(dlong$hr[dlong$avg.max == "Avg.HR"]),size=1, linetype="dashed", colour = pal[1])+
      geom_hline(yintercept = mean(dlong$hr[dlong$avg.max == "Max.HR"]),size=1, linetype="dashed", colour = pal[2])+
      facet_wrap(.~distance_grouping)
  }

  if(plot == "cadence_dist.gp"){

    dlong <- data %>%
      dplyr::select(Date, Avg.Run.Cadence, Max.Run.Cadence, distance_grouping) %>%
      pivot_longer(cols=c(Avg.Run.Cadence, Max.Run.Cadence),
                   values_to = "cadence",
                   names_to = "avg.max")
    dlong$cadence <- as.numeric(dlong$cadence)
    dlong$Date <- as.POSIXct(strptime(dlong$Date, format = "%Y-%m-%d %H:%M:%S"))


    plot = ggplot(dlong, aes(x = Date,y = cadence, colour = avg.max)) +

      geom_line(aes(group = Date),colour="yellow",size=1)+
      geom_point(size=6, alpha=0.8) +
      scale_colour_manual(values=pal)+

      labs(x = "Date", y = "Cadence",title = "Cadence by distance grouping") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        panel.border = element_rect(colour="white", size=1),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      geom_hline(yintercept = mean(dlong$cadence[dlong$avg.max == "Avg.Run.Cadence"]),size=1, linetype="dashed", colour = pal[1])+
      geom_hline(yintercept = mean(dlong$cadence[dlong$avg.max == "Max.Run.Cadence"]),size=1, linetype="dashed", colour = pal[2])+
      facet_wrap(.~distance_grouping)




  }
  return(plot)
}



#' @title overviewGarminRunning
#'
#' @description A function to generate stylish plots that rapidly give an overview of your running.
#'
#' @param output from the processGarminRunning function
#' @param date_from a date processed by as.Date e.g as.Date("2020-03-18"). This is a cut-off data for your analysis. Everything after this will be included in the analysis.
#' @param plot can be one of "runs_per_loc_per_dist", "distance_per_location", "mean_pace", "pca_plot", and "total_runs"
#' @param target_time a pace in the form "7:30" (character variable) with "minutes:seconds" that is one previously achieved that you wish to mark on your plots, OR it could be a target pace you have set yourself
#' @return plot
#'
#' @examples
#'
#' #let's process my data so that it's good for R, and tidy for plotting
#' my_runs <- processGarminRunning(data=garmin)
#'
#'
#' #What about my pace - calculate my mean avg.pace amongst all my runs per location
#'
#' overviewGarminRunning(my_runs,
#'                      date_from = as.Date("2020-03-18"),
#'                      plot = "mean_pace",
#'                      target_time = "7:18")
#'
#' #Note the 5-10 mile runs the pace is slightly faster in manchester than in Bromsgrove probably because of the hills!
#'
#' # we can also utilise numeric data to do PCA analysis - what are the major sources of variation in our running data?
#' overviewGarminRunning(my_runs,
#'                      date_from = as.Date("2020-03-18"),
#'                      plot = "pca_plot",
#'                      target_time = "7:18")
#'
#' @export


overviewGarminRunning <- function(data,
                                  date_from = NA,
                                  plot,
                                  target_time = median(data$Avg.Pace)){

  if(!plot %in% c("runs_per_loc_per_dist", "mean_pace", "distance_per_location", "total_runs", "pca_plot")){
    stop("The plot you request is not part of the repertoire")
  }

  #check if the required packages are installed
  packages = c("tidyverse", "lubridate", "RColorBrewer", "ggridges", "dplyr","ggpubr", "ggfortify", "caret")

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


  loc_dist <-  data %>%
    group_by(location, distance_grouping) %>%
    summarise(n = n(), mean.pace = mean(Avg.Pace))

  if(plot == "runs_per_loc_per_dist"){

    plot = ggplot(loc_dist, aes(location, n))+
      geom_bar(aes(fill = distance_grouping), stat = "identity")+
      scale_fill_manual(values=pal2)+

      labs(x = "", y = "Number of runs",title = "Runs per location per distance") +
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
      )
  }

  if(plot == "mean_pace"){

    plot = ggplot(loc_dist, aes(distance_grouping, mean.pace))+
      geom_point(aes(colour = location),size=4,alpha=0.8)+
      scale_colour_manual(values=pal)+
      scale_y_datetime(date_labels = "%M:%S") +
      labs(x = "", y = "Within distance mean pace",title = "Mean pace per distance per location") +
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
      geom_hline(yintercept = as.POSIXct(strptime(target_time, format = "%M:%S")), linetype="dashed", colour="white")
  }

  tot_dist <-  data %>%
    group_by(location) %>%
    summarise(total_dist = sum(Distance))

  if(plot == "distance_per_location"){

    plot = ggplot(tot_dist, aes(location, total_dist))+

      geom_segment(aes(x=location, xend=location, y=0, yend=total_dist), colour="yellow")+
      geom_point(aes(colour = location), stat = "identity", size=8, alpha=0.8)+
      scale_colour_manual(values=pal)+

      labs(x = "", y = "Total distance run",title = "Cumulative distance per location") +
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
        legend.position = "none"
      )
  }

  runs <-  data %>%
    group_by(Activity.Type, distance_grouping) %>%
    summarise(n = n())

  if(plot == "total_runs"){

    plot = ggplot(runs, aes(Activity.Type, n))+
      geom_bar(aes(fill = distance_grouping), stat = "identity")+
      scale_fill_manual(values=pal2)+

      labs(x = "", y = "Total runs",title = "Total runs per distance group") +
      theme_minimal()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.y = element_blank(),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        #panel.border = element_rect(colour="white", size=1),

        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      coord_flip()
  }

  if(plot == "pca_plot"){

    ndata <- data %>%
      dplyr::select_if(is.numeric)

    #next get rid of runs with too many NA
    if(length(which(apply(ndata, 1, function(x) sum(is.na(x))) > ncol(ndata)/2)) > 0){
      ndata <- ndata[-which(apply(ndata, 1, function(x) sum(is.na(x))) > ncol(ndata)/2),]
    }

    #drop columns with zero variance
    nzv <- caret::nearZeroVar(ndata)
    if(length(nzv) > 0){
      ndata <- ndata[,-nzv]
    }

    #if there are NAs try imputing these
    if(sum(is.na(ndata)) > 0){
      pred_obj <- caret::preProcess(ndata, method="knnImpute")
      ndata <- predict(pred_obj, ndata)
    }


    plot = autoplot(prcomp(ndata), colour = "Distance", point.size=4, loadings=TRUE, loadings.label=TRUE, loadings.label.colour = "white")+
      theme_bw()+
      labs(title = "Runs: principal components analysis") +
      scale_colour_gradient(low = "yellow", high = "red")+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour="white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        #panel.border = element_rect(colour="white", size=1),

        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_text(color="white"),
        legend.text = element_text(color = "white")
      )
  }

return(plot)
}




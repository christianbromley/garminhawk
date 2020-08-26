# garminhawk

## Intro

__garminhawk__ is my first attempt at creating an R package. The Garmin Connect app, whilst comprehensive, allows somewhat limited visualisations of running and cycling data with little space for customisation, and little in the way of depth. 

This package is currently under development, and I am seeking new ways to analyse the data, and new ways to visualise the data all of the time. I will continue to add new functionality to the package as I continue analysing and continue running. As I begin to train for a marathon, this more advanced analysis will become crucial.

At the minute the package consists of quite __basic data visualisation__. My aim to develop __new metrics__ from the data to give improved insights.

I am also yet to robustly test any of the functions. To do this I will need external datasets, and for others to give it a try and feedback.

Please contact me if you have any comments about the code. I am a beginner.

## Installing garminhawk

```{r}
#install.packages("devtools")
devtools::install_github("christianbromley/garminhawk")

```

## How to use garminhawk?

- Log into Garmin Connect
- In the left hand panel click on __Activities__
- Scroll down to the bottom
- Click Export CSV in the top right
- Load up R

### Read your data into R
```{r,warning=FALSE,message=FALSE,error=FALSE}
#setwd("~/Downloads")
#garmin <- read.csv("Activities-2.csv", header=T)

```

### Process your data in R
```{r,warning=FALSE,message=FALSE,error=FALSE}
library(garminhawk)
my_runs <- processGarminRunning(data=garmin)

```

### Generate a couple of overview plots

For all the available plots, enter ?overviewGarminRunning in the console.
```{r,warning=FALSE,message=FALSE,error=FALSE}
#exactly where have most of these runs been?
overviewGarminRunning(my_runs,
                      date_from = as.Date("2020-03-18"),
                      plot = "distance_per_location",
                      target_time = "7:18")

overviewGarminRunning(my_runs,
                      date_from = as.Date("2020-03-18"),
                      plot = "pca_plot",
                      target_time = "7:18")


```

### Broadly analyse individual runs

For all the available plots, enter ?plotGarminRunning in the console.
```{r,warning=FALSE,message=FALSE,error=FALSE}

plotGarminRunning(my_runs,
                  plot = "dist_pace",
                  date_from = as.Date("2020-03-18"),
                  target_time = "7:18")

plotGarminRunning(my_runs,
                  plot = "cumulative_dist",
                  date_from = as.Date("2020-03-18"))

```

## What's next?

- clean up the code, make it more understandable
- design new plots tailored to specific questions frequently asked by runners. Some ideas:
  - Am I getting faster and fitter?
  - How do I deal with elevation? Does this affect my speed?
  - How do I run best? Am I better running at a steady, homogeneous pace __or__ am I better going for a really quick start?
- build cycling-specific functions
- build walking-specific functions
- build functionality to merge activity data with other Garmin Connect outputs
- build in customisable plots e.g. different palettes etc
- explore more advanced analytics methods
- create interactive plots e.g plotly

Please, if you have any suggestions, feel free to contact me.




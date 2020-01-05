#The functions below are all helpers required by the overarching function
#rawDataTransformer, that transforms the raw data from the tracker into a 
#more readable format.

#The function below is to calculate the difference between time-points in a consistent way, 
#subtracting the night-time(s) from 6 p.m. to 8 a.m. when an activity or queue last over more than 
#one day. This function ignores possible suspended periods that occur within the interval.
time.diff.calculator <- function(begin.time,end.time){
  beg.date <- as.Date(begin.time) #Conversion of the POSIXct format (with time) to day format (without).
  end.date <- as.Date(end.time)
  f <- as.double(end.date-beg.date) #f is >0 if the activity/queue spans over more than one day
  return (as.double(difftime(end.time,begin.time,units = "secs")-(f * 48600))) #48600 s is the period
  #between 6:30 p.m. and 8 a.m. I am using "as.double" hoping that the data would appear continuous
  #and not discrete - ggplot2 cannot do a histogram with concrete data unfortunately.
}

#Isolating time of the day (in double format that might be better suitable for a histogram later) from a 
#date in POSIXct format:
timeOfDayCalculator <- function(time){
  date.only <- as.POSIXct(strptime(time,"%Y-%m-%d", tz = "UTC"))
  return (as.double(time - date.only))
}

#Calculation of the duration of all suspended periods within a time-period:
susp.dur.calculator <- function(begin.time,end.time,df){
  #Creating a subset of the prescr.data dataframe isolating only those records containing
  #"suspended" under Activity that start and end within the time period at hand. 
  df.be <- filter(df, ((Start >= begin.time)&(End <= end.time)))
  susp.duration <- 0
  if ("Suspended" %in% df.be$Activity){
    df.be.susp <- filter(df.be, Activity == "Suspended")
    susp.duration <- as.double(sum(df.be.susp$timeDiff))
  }
  return (susp.duration)
}
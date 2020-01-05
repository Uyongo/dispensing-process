library(simmer)
library(EnvStats)

#Helper functions for the "app.R" file:

#This function converts seconds to a string with the
#format "xx h xx min XX sec". It only works for single
#values, i.e. it does not work for vectors.
secondsToHoursAndMinText <- function(sec){
  hours <- sec %/% 3600
  minutes <- (sec %% 3600) %/% 60
  seconds <- (sec %% 3600) %% 60
  if (hours == 0){
    hours.ch <- ""
  }else{
    hours.ch <- paste0(hours %>% as.character(), " h ")
  }
  if ((hours == 0) & (minutes == 0)){
    minutes.ch <- ""
  }else{
    minutes.ch <- paste0(minutes %>% as.character(), " min ")
  }
  seconds.ch <- paste0(seconds %>% 
                         round(digits = 0) %>% 
                         as.character(),
                       " sec")
  return(
    paste0(
      hours.ch,
      minutes.ch,
      seconds.ch
    )
  )
}

#This function helps visualise the average length
#of queues in the queue tab of "interface.R". It 
#returns a data-frame that is easier for ggplot to 
#display. It transforms seconds into minutes and prepares the data
#for the crossbar chart of ggplot:
queueBinner <- function(dataframeForPlot){
  #binHelper is a vector that goes from 8 to 18.5 in steps
  #of 0.5
  binHelper <- (1:22 * 0.5) + 7.5
  result.df <- NULL #this is the dataframe that is going to be built
  #by the for loop below and then returned by the function
  for (count in 1:21){
    #all queues available in in the half-hour period at hand:
    dataVector <- dataframeForPlot %>% 
      filter((arrivals > binHelper[count])&(arrivals <= binHelper[count+1])) %>%
      pull(queue)
    if (length(dataVector) == 0){
      lower <- 0
      median <- 0
      upper <- 0
    }else{
      lower <- dataVector %>% 
        quantile(probs = 0.25) %>% 
        as.numeric() %>%
        '/'(60) #division by 60 to get minutes instead of seconds
      median <- dataVector %>% quantile(probs = 0.50) %>% 
        as.numeric() %>% '/'(60)
      upper <- dataVector %>% quantile(probs = 0.75) %>% 
        as.numeric() %>% '/'(60)
    }
    arrival <- binHelper[count]
    result.df <- rbind(
      result.df,
      data.frame(
        "arrival" = arrival,
        "lower" = lower,
        "median" = median,
        "upper" = upper
        )
      )
  }
  return(result.df)
}

####################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # 
####################################################
#Code for functions making up or used by the simulation model:

#The simulation model below is a function that requires a shift pattern for
#four roles, the proportion of the different dispensing requirements, 
#a vector outlining arrivals at the hatch and a vector outlining arrivals
#from the ward, both over 1 working day, i.e. the simulator
#runs over 1 working day. It returns a data-frame of queue and activity
#durations of every single work-item that finished the simulation and 
#the number of work-items still remaining in the simulation process:
simulationResults <- function(shiftMatrix, arrivalsAtHatchVect, 
                              arrivalsFromWardVect, outPatDurs, inPatDurs, 
                              regArrivalBool, repeatNumber){
  #outPatDurs is a data-frame containing the activity durations for outpatients
  #for a chosen date
  #inPatDurs is a the equivalent data-frame for inpatient wards
  #repeatNumber indicates how often the simulation core is to be repeated
  
  #browser()
  
  if (regArrivalBool){
    #creating new arrival vector indicating arrival times at fixed intervals
    #from each other - the total number of arrivals remains the same as on the day chosen
    arrivalsAtHatchV <- seq(
      from = 60, # i.e. at 9 a.m.
      by = 480/(length(arrivalsAtHatchVect)), #480 min. are 8 hours (9 a.m.-5 p.m.)
      length.out = length(arrivalsAtHatchVect)
    )
    arrivalsFromWardV <- seq(
      from = 60,
      by = 480/(length(arrivalsFromWardVect)), 
      length.out = length(arrivalsFromWardVect)
    )
  }else{
    arrivalsAtHatchV <- arrivalsAtHatchVect
    arrivalsFromWardV <- arrivalsFromWardVect
  }
  
  outpatientDurations <- outPatDurs
  inpatientDurations <- inPatDurs
  
  #Data input of average durations of main activities per arrival
  #and duration of run.I will assume that time-units are minutes.
  runDuration <- 630 #630 min. would be 10.5 hours, e.g. from 8:00 a.m. to 6:30 p.m.
  arrivalFromHatch <- "hatch" #The name used for arrivals at the hatch in the model.
  arrivalFromWard <- "wards" #The name used for arrivals from the wards in the model.
  
  #Schedules (i.e. shifts for resources):
  shiftTimes <- c(0, 60, 180, 300, 420, 540) # this corresponds to 8 a.m., 
  #9 a.m., 11 a.m., 1 p.m., 3 p.m., and 5 p.m. - this is when number of resources
  #change
  disp.pharmacist.sched <- schedule(shiftTimes,
                                    shiftMatrix["disp. pharmacist",], period = runDuration)
  ward.pharmacist.sched <- schedule(shiftTimes,
                                    shiftMatrix["ward pharmacist",], period = runDuration)
  pharm.tech.sched <- schedule(shiftTimes,
                               shiftMatrix["pharm. tech.",], period = runDuration)
  fin.check.sched <- schedule(shiftTimes,
                              shiftMatrix["checking tech.",], period = runDuration)
  
  resultDataFrame <- NULL
  
  for (counter in (1:repeatNumber)){
    
    #since I want to keep the activity durations of a particular prescription
    #together, I will just create a vector of randomly selected RxIDs from 
    #the outpatient or inpatient prescription data for the chosen day - 
    #these prescription numbers will be used to access the activity data
    #later prescription by prescription
    outpatientRxIDs <- sample(x = outpatientDurations %>% pull(RxID), 
                              size = outpatientDurations %>% nrow(), 
                              replace = F)
    inpatientRxIDs <- sample(x = inpatientDurations %>% pull(RxID), 
                             size = inpatientDurations %>% nrow(), 
                             replace = F)
    #browser()
    ##############################################################
    #Defining Simmer environment:
    pharmacy <- simmer("Dispensing Process")
    
    #Defining trajectory with 2 activities, the distribution of their durations
    #and their required resources:
    dispProcess <- trajectory("dispensing & final checking") %>% 
      set_attribute(keys = "progress", values = function(){5}) %>% # 5 ... waiting for dispensing
      seize("dispenser", 1) %>%
      set_attribute(keys = "progress", values = function(){6}) %>% # 6 ... start of dispensing
      timeout(function() {durationCalculator(get_name(pharmacy),
                                             get_attribute(pharmacy,"progress"),
                                             outpatientRxIDs,
                                             inpatientRxIDs,
                                             outpatientDurations,
                                             inpatientDurations)}) %>% #********
      release("dispenser", 1) %>%
      set_attribute(keys = "progress", values = function(){7}) %>% # 7 ... waiting for final checking
      simmer::select(resources = c("final checker","disp_pharmacist"), policy = 'shortest-queue') %>%
      seize_selected(amount = 1) %>%
      set_attribute(keys = "progress", values = function(){8}) %>% # 8 ... start of final checking
      timeout(function() {durationCalculator(get_name(pharmacy),
                                             get_attribute(pharmacy,"progress"),
                                             outpatientRxIDs,
                                             inpatientRxIDs,
                                             outpatientDurations,
                                             inpatientDurations)}) %>% #********
      release_selected(amount = 1) %>%
      set_attribute(keys = "progress", values = function(){9}) # 9 ... finish of final checking and process 
    
    #Part of the trajectory that covers the verifying of prescriptions from the ward:
    verifyingOnWards <- trajectory("verifying on wards") %>%
      #Attribute keeping track of progress of Rx in process:
      set_attribute(keys = "progress", values = function(){1}) %>% # 1 ... waiting for verifying 
      seize("ward pharmacist", 1) %>%   
      set_attribute(keys = "progress", values = function(){2}) %>% # 2 ... start of verifying
      timeout(function() {durationCalculator(get_name(pharmacy),
                                             get_attribute(pharmacy,"progress"),
                                             outpatientRxIDs,
                                             inpatientRxIDs,
                                             outpatientDurations,
                                             inpatientDurations)}) %>%
      release("ward pharmacist", 1)
    
    #Part of the trajectory that covers the verifying of prescriptions from the hatch (mainly
    #outpatient Rxs):
    verifyingOutpatients <- trajectory("verifying in dispensary") %>%
      set_attribute(keys = "progress", values = function(){1}) %>% # 1 ... waiting for verifying 
      seize("disp_pharmacist", 1) %>%   
      set_attribute(keys = "progress", values = function(){2}) %>% # 2 ... start of verifying
      timeout(function() {durationCalculator(get_name(pharmacy),
                                             get_attribute(pharmacy,"progress"),
                                             outpatientRxIDs,
                                             inpatientRxIDs,
                                             outpatientDurations,
                                             inpatientDurations)}) %>%
      release("disp_pharmacist", 1)
    
    prescriptFromWard <- join(verifyingOnWards, dispProcess)
    prescriptFromHatch <- join(verifyingOutpatients, dispProcess)
    
    #Defining number of resources (i.e. staff) available:
    pharmacy %>% 
      add_resource("disp_pharmacist", disp.pharmacist.sched) %>% 
      add_resource("ward pharmacist", ward.pharmacist.sched) %>% 
      add_resource("dispenser", pharm.tech.sched) %>% 
      add_resource("final checker", fin.check.sched) %>% 
      add_generator(arrivalFromHatch, prescriptFromHatch, at(arrivalsAtHatchV), mon = 2) %>%
      add_generator(arrivalFromWard, prescriptFromWard, at(arrivalsFromWardV), mon = 2)
    
    #Defining length of simulation run:
    pharmacy %>% run(until = runDuration)
    
    #Output of data to data-frame:
    arrivals.df <- pharmacy %>% get_mon_arrivals() %>% .[order(.$start_time),] 
    attributes.df <- pharmacy %>% get_mon_attributes() %>% .[order(.$time),]
    
    #browser()
    
    resultDataFrame <- resultDataFrame %>%
      rbind(
        outputRearranger(arrivals.df,attributes.df) %>%
          cbind(trial = counter)
      )
  }
  
  return(resultDataFrame)
}
####################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # 
####################################################


#This function returns a duration dependend on attributes in the trajectory below; 
durationCalculator <- function(arrivName, activity, outpatRxIDs, 
                               inpatRxIDs, outpatDurs, inpatDurs){
  #arrivName is the name of the arrival, e.g. "wards11", or "hatch5" - 
  #the last digits are to count the arrivals
  #activity is an integer from 1 to 9
  #depending on these two parameters an activity duration is picked from a
  #data-frame, i.e. outpatientDurations or inpatientDurations
  
  #browser()
  counter <- substr(arrivName, start =  6, stop = nchar(arrivName)) %>% #both arrival names have 5 letters
    as.integer() %>% "+" (1) #this is to extract the number of the arrival 
  currActivity <- switch(activity %>% as.character(), "2" ="verifActivity", 
        "6" = "dispActivity", "8" = "finCheckActivity")
  if (grepl("hatch",arrivName)){ #this expression would be true for an inpatient Rx
    r <- outpatDurs[
      outpatDurs$RxID == outpatRxIDs[counter],
      currActivity]
  }else{
    r <- inpatDurs[
      inpatDurs$RxID == inpatRxIDs[counter],
      currActivity]
  }
  
  return(r)
}

#Function to rearrange output available by get_mon_arrivals() and
#get_mon_attributes() functions by Simmer - it returns a dataframe
#that provides data in a more readable format:
outputRearranger <- function(arrivals.df,attributes.df){
  #Defining of dataframe that is to hold an temporary form of the
  #output data (i.e. the exact times when queues and activities start/end):
  absActivityTimes.df <- data.frame("arrival" = NULL, 
                                    "startOfVerifQueu" = NULL,
                                    "startOfVerif" = NULL,
                                    "startOfDispQueu" = NULL,
                                    "startOfDisp" = NULL,
                                    "startOfFinCheckQueu" = NULL,
                                    "startOfFinCheck" = NULL,
                                    "endOfFinCheck" = NULL)
  for (prescr in distinct(arrivals.df,name)$name){ #Going in
    #loop through every distinct name of every arrival in arrivals.df, i.e.
    #only those that have completed the process, in order
    #to transform the attributes.df dataframe into a more readable format.
    helper.df <- attributes.df %>% dplyr::filter(name == prescr) #isolating of
    #attributes for prescription with name stored in prescr
    #Defining a row that would need to be added to absActivityTimes.df:
    addedLine.df <- data.frame("arrival" = prescr, 
                               "startOfVerifQueu" = dplyr::filter(helper.df, key == "progress", value == 1)$time,
                               "startOfVerif" = dplyr::filter(helper.df, key == "progress", value == 2)$time,
                               "startOfDispQueu" = dplyr::filter(helper.df, key == "progress", value == 5)$time,
                               "startOfDisp" = dplyr::filter(helper.df, key == "progress", value == 6)$time,
                               "startOfFinCheckQueu" = dplyr::filter(helper.df, key == "progress", value == 7)$time,
                               "startOfFinCheck" = dplyr::filter(helper.df, key == "progress", value == 8)$time,
                               "endOfFinCheck" = dplyr::filter(helper.df, key == "progress", value == 9)$time)
    absActivityTimes.df <- rbind(absActivityTimes.df,addedLine.df)
  }
  #Transforming absActivityTimes.df to only contain the durations of 
  #activities and/or queues, together with other relevant data:
  queueAndactivityDurations.df <- absActivityTimes.df %>% 
    cbind(verifQueue = .$startOfVerif - .$startOfVerifQueu,
          verifying = .$startOfDispQueu - .$startOfVerif,
          dispQueue = .$startOfDisp - .$startOfDispQueu,
          dispensing = .$startOfFinCheckQueu - .$startOfDisp,
          finCheckQueue = .$startOfFinCheck - .$startOfFinCheckQueu,
          finChecking = .$endOfFinCheck - .$startOfFinCheck,
          totThroughput = .$endOfFinCheck - .$startOfVerifQueu) %>%
    subset(select = c(arrival,startOfVerifQueu,
                      verifQueue,verifying,dispQueue,dispensing,finCheckQueue,
                      finChecking,totThroughput)) 
  return (queueAndactivityDurations.df %>% .[order(.$startOfVerifQueu),])
}


#########################################################################
# # # # # # #  # # # # # # # # # # # # # # #  # # # # # # # # # # # # # #
#########################################################################

#This function returns the latest date in the quarter of a given date:
latestDateInQuarter <- function(givenDate){
  quarterInGivenYear <- quarterOfDate(givenDate)
  possibleMonths <- c("03","06", "09", "12")
  possibleLastDays <- c("31", "30", "30", "31")
  return(
    paste0(
      yearOfDate(givenDate) %>% as.character(),
      "-",
      possibleMonths[quarterInGivenYear],
      "-",
      possibleLastDays[quarterInGivenYear]
    ) %>%
      as.Date()
  ) 
}

#This function returns the earliest date in the quarter of a given date:
earliestDateInQuarter <- function(givenDate){
  quarterInGivenYear <- quarterOfDate(givenDate)
  possibleMonths <- c("01","04", "07", "10")
  return(
    paste0(
      yearOfDate(givenDate) %>% as.character(),
      "-",
      possibleMonths[quarterInGivenYear],
      "-",
      "01"
    ) %>%
      as.Date()
  )
}

quarterOfDate <- function(givenDate){
  return(
    format(givenDate, "%m") %>% as.numeric() %>% 
      "-" (1) %>%
      "/" (3) %>% "+" (1) %>% trunc()
  )
}

yearOfDate <- function(givenDate){
  return(
    format(givenDate, "%Y") %>% as.numeric()
  )
}



# #This function updates the data-frames that contain the underlying data the
# #app is currently working with. 
# datasetUpdater <- function(){
#   #the whole code below is completed on initiation of the program also
#   availableDataSetsV <- list.files(path = "data") %>% 
#     .[((substr(.,start = 1,stop = 2) == "rd")&(substr(.,start = 26,stop = 28) == "rds"))]
#   
#   #Uploading of underlying data - transformedRawData.df is a list made up of three
#   #data-frames (absolute times; calculated durations ignoring and considering suspended
#   #periods) and two vectors (out of range records, rejected records due to missing data).
#   #The first dataset in the estabilshed with the vector availableDataSets is initially
#   #used as underlying data:
#   transformedRawData.df <- readRDS(
#     file = paste0(
#       "data/",
#       availableDataSetsV[1]
#     )
#   )
#   
#   #Uploading relevant data for analysis:
#   absTimes.df <- transformedRawData.df[[1]]
#   #adding a column designating the weekday of the verifying activity starting:
#   absTimes.df <- absTimes.df %>% cbind(dayOfWeek = weekdays(.$startOfVerifQueu))
#   durations.df <- transformedRawData.df[[2]]
#   durationsS.df <- transformedRawData.df[[3]]
#   #adding another column to the durations data-frames about the durations
#   #of the steps in the dispensary only (i.e. dispensing and final checking)
#   durations.df <- durations.df %>% 
#     cbind(totDispThroughput = .$dispQueue+.$dispActivity+.$finCheckQueue+.$finCheckActivity)
#   durationsS.df <- durationsS.df %>%
#     cbind(totDispThroughput = .$dispQueue+.$dispActivity+.$finCheckQueue+.$finCheckActivity)
#   outOfRange.df <- transformedRawData.df[[4]]
#   rejected.df <- transformedRawData.df[[5]]
#   
#   #Finding the first and last date in absTimes.df:
#   earliestTime <- pull(absTimes.df,startOfVerifQueu) %>% min()
#   latestTime <- pull(absTimes.df,startOfVerifQueu) %>% max()
#   earliestDate.chr <- earliestTime %>% strftime(format = "%d/%m/%Y")
#   latestDate.chr <- latestTime %>% strftime(format = "%d/%m/%Y")
#   
#   #Establishing of unique wards in whole dataset and adding "all":
#   uniqueWards.vect <- absTimes.df %>% distinct(unit) %>% pull(unit) %>%
#     as.character() %>% sort(decreasing = F) %>% append("all", after = 0)
#   uniquePriorAll.vect <- absTimes.df %>% distinct(priority) %>%
#     pull(priority) %>% as.character() %>% sort(decreasing = F) %>% append("all", after = 0)
# }
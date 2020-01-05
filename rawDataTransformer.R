#Loading of helper functions (time.diff.calculator, timeOfDayCalculator, and
#susp.dur.calculator) required further below. 
source("rawDataTransformer helpers.R")

rawDataTransformer <- function(raw.data){
  
  #Adding a new column to the original, raw data frame that calculates the differences in time as per time.diff.calculator above:
  raw.data <- cbind(raw.data,timeDiff = time.diff.calculator(raw.data$Start,raw.data$End))
  
  #Adding another, new column to raw data frame that shows what time of the day the prescription arrived:
  raw.data <- cbind(raw.data,timeOfDay = timeOfDayCalculator(raw.data$Start))
  
  #Establishing of unique prescription numbers as a data frame. I have to use "dplyr::select" as there
  #is a command with the same name in the MASS libarary that would otherwise be used.
  unique.prescr.No <- distinct(dplyr::select(raw.data,PrescriptionID))
  
  #Defining vectors for data-gathering and output:
  output.vect <- NULL #This vector stores the data created by the for loop below. Suspension periods
  #are ignored, i.e. added to activity and queue times according to algorithm below. 
  output.vect.s <- NULL #This vector stores data calculated in an alternative way. Periods
  #of suspension are subtracted from queue and activity durations.
  no.activity.vect <- NULL #This vector is to store the prescription IDs for Rxs where 
  #not all relevant activities could not be found in the dataset, or where recorded 
  #activities were not recorded in a sensible order.
  out.of.range.vect <- NULL #Stores the prescription IDs of those Rxs where relevant 
  #activities were recorded to start/end at either before 8 a.m. or after 6:30 p.m.
  
  #Defining one of the data-frames to be returned by this function:
  absTimes.df <- data.frame()
  
  #This for loop goes through each unique prescription number present in
  #the data frame.
  for (ID in unique.prescr.No$PrescriptionID){
    #Isolating of subset for each prescription number:
    prescr.data <- filter(raw.data,PrescriptionID == ID)
    
    activity.checker <- T #Stays T to the end of the loop if all activities could be found
    #in the raw data and if they were in a sensible order.
    in.range.checker <- T #Stays T to the end of the loop of all activities in the dataset
    #start/end in the interval between 8 a.m. and 6 p.m.
    
    verif.queu.start <- NULL
    verif.act.start <- NULL
    disp.queu.start <- NULL
    disp.act.start <- NULL
    fin.check.queu.start <- NULL
    fin.check.act.start <- NULL
    collect.queu.start <- NULL
    collect.queu.end <- NULL
    
    if ("Booking in" %in% prescr.data$Activity){
      verif.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Booking in")$Start[1], tz = "UTC")
      #I added [1] to the $Start vector above as some of the activities might get mentioned more than once per prescription.
    }else{activity.checker <- F}
    
    if ("Clinical Checking" %in% prescr.data$Activity){
      verif.act.start <- as.POSIXct(subset(prescr.data,Activity == "Clinical Checking")$Start[1], tz = "UTC")
    }else{activity.checker <- F}
    
    if ("Waiting for Co-Ordinator" %in% prescr.data$Activity){
      disp.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Waiting for Co-Ordinator")$Start[1], tz = "UTC")
    }else if ("Co-Ordinator" %in% prescr.data$Activity){
      disp.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Co-Ordinator")$Start[1], tz = "UTC")
    }else if ("Waiting for Label & Dispense" %in% prescr.data$Activity){
      disp.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Waiting for Label & Dispense")$Start[1], tz = "UTC")
    }else if("Label & Dispense" %in% prescr.data$Activity){
      disp.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Label & Dispense")$Start[1], tz = "UTC")
    }else{activity.checker <- F}
    
    if ("Label & Dispense" %in% prescr.data$Activity){
      disp.act.start <- as.POSIXct(subset(prescr.data,Activity == "Label & Dispense")$Start[1], tz = "UTC")
    }else{activity.checker <- F}
    
    if ("Waiting for Final Check" %in% prescr.data$Activity){
      fin.check.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Waiting for Final Check")$Start[1], tz = "UTC")
    }else if ("Final Check" %in% prescr.data$Activity){
      fin.check.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Final Check")$Start[1], tz = "UTC")
    }else{activity.checker <- F}
    
    if ("Final Check" %in% prescr.data$Activity){
      fin.check.act.start <- as.POSIXct(subset(prescr.data,Activity == "Final Check")$Start[1], tz = "UTC")
    }else{activity.checker <- F}                   
    
    if ("Waiting for Ready for Collection" %in% prescr.data$Activity){
      collect.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Waiting for Ready for Collection")$Start[1], tz = "UTC")
    }else if ("Ready for Collection" %in% prescr.data$Activity){
      collect.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Ready for Collection")$Start[1], tz = "UTC")
    }else if ("Waiting for Collection" %in% prescr.data$Activity){
      collect.queu.start <- as.POSIXct(subset(prescr.data,Activity == "Waiting for Collection")$Start[1], tz = "UTC")
    }else{activity.checker <- F}
    
    if ("Collected" %in% prescr.data$Activity){
      collect.queu.end <- as.POSIXct(subset(prescr.data,Activity == "Collected")$Start[1], tz = "UTC")
    }else{activity.checker <- F}
    
    #Exclusion of data where activities are not recorded in the expected sequence or if no starting/end points of
    #activities/queues could be allocated:
    if (is.null(collect.queu.end) | is.null(collect.queu.start) | is.null(fin.check.act.start)
        | is.null(fin.check.queu.start) | is.null(disp.act.start) | is.null(disp.queu.start) | 
        is.null(verif.act.start) | is.null(verif.queu.start)){
      activity.checker <- F
    } 
    
    if (activity.checker){
      if (collect.queu.end < collect.queu.start | collect.queu.start < fin.check.act.start | fin.check.act.start < fin.check.queu.start
          | fin.check.queu.start < disp.act.start | disp.act.start < disp.queu.start | disp.queu.start < verif.act.start | 
          verif.act.start < verif.queu.start){
        activity.checker <- F
      }
    }  
    
    #Identifying those activities/queues that were recorded before 8 a.m. or after 
    #6:30 p.m., respectively, and excluding them from further analysis. Tried to solve this with a for 
    #loop before which R could not process (even though every single step worked 
    #bautifully in the console).
    if (activity.checker){
      act.queu.time.points <- c(verif.queu.start,verif.act.start,disp.queu.start,
                                disp.act.start,fin.check.queu.start,fin.check.act.start,
                                collect.queu.start,collect.queu.end)
      time.of.day <- ((timeOfDayCalculator(act.queu.time.points) < 8) | (timeOfDayCalculator(act.queu.time.points) > 18.5))
    }
    
    if (T %in% time.of.day){
      in.range.checker <- F
      out.of.range.vect <- append(out.of.range.vect,ID)
    }
    
    if (activity.checker & in.range.checker){
      rowToAddToAbsTimes.df <- data.frame("RxID" = ID, 
                                          "unit" = prescr.data$Ward[1], 
                                          "priority" = prescr.data$PrescriptionType[1],
                                          "startOfVerifQueu" = verif.queu.start,
                                          "startOfVerifAct" = verif.act.start,
                                          "startOfDispQueu" = disp.queu.start,
                                          "startOfDispAct" = disp.act.start,
                                          "startOfFinCheckQueu" = fin.check.queu.start,
                                          "startOfFinCheckAct" = fin.check.act.start,
                                          "endOfFinCheckAct" = collect.queu.start)
      absTimes.df <- rbind(absTimes.df,rowToAddToAbsTimes.df)
    }
    
    #Calculating of queue and activity durations ignoring possible suspended periods that occur
    #before activities/queues start or the collection queue ends.
    if (activity.checker & in.range.checker) {
      #Calculation of queue and activity durations:
      verif.queu.dur <- time.diff.calculator(verif.queu.start,verif.act.start)
      verif.act.dur <- time.diff.calculator(verif.act.start,disp.queu.start)
      disp.queu.dur <- time.diff.calculator(disp.queu.start,disp.act.start)
      disp.act.dur <- time.diff.calculator(disp.act.start,fin.check.queu.start)
      fin.check.queu.dur <- time.diff.calculator(fin.check.queu.start,fin.check.act.start)
      fin.check.act.dur <- time.diff.calculator(fin.check.act.start,collect.queu.start)
      coll.queu.dur <- time.diff.calculator(collect.queu.start,collect.queu.end)
      #Creation of new row for new data frame and adding it to output vector:
      new.record.row <- c(ID,verif.queu.dur,verif.act.dur,disp.queu.dur,disp.act.dur,fin.check.queu.dur,
                          fin.check.act.dur,coll.queu.dur,timeOfDayCalculator(verif.queu.start),
                          timeOfDayCalculator(disp.queu.start),timeOfDayCalculator(fin.check.queu.start))
      output.vect <- append(output.vect,new.record.row)
      #Calculating activity/queue durations taking into account possible suspended periods
      #and subtracting these from activity/queue durations calculated above:
      if ("Suspended" %in% prescr.data$Activity){
        verif.queu.dur.s <- verif.queu.dur - susp.dur.calculator(verif.queu.start,verif.act.start,prescr.data)
        verif.act.dur.s <- verif.act.dur - susp.dur.calculator(verif.act.start,disp.queu.start,prescr.data)
        disp.queu.dur.s <- disp.queu.dur - susp.dur.calculator(disp.queu.start,disp.act.start,prescr.data)
        disp.act.dur.s <- disp.act.dur - susp.dur.calculator(disp.act.start,fin.check.queu.start,prescr.data)
        fin.check.queu.dur.s <- fin.check.queu.dur - susp.dur.calculator(fin.check.queu.start,fin.check.act.start,prescr.data)
        fin.check.act.dur.s <- fin.check.act.dur - susp.dur.calculator(fin.check.act.start,collect.queu.start,prescr.data)
        coll.queu.dur.s <- coll.queu.dur - susp.dur.calculator(collect.queu.start,collect.queu.end,prescr.data)
        new.record.row.s <- c(ID,verif.queu.dur.s,verif.act.dur.s,disp.queu.dur.s,disp.act.dur.s,fin.check.queu.dur.s,
                              fin.check.act.dur.s,coll.queu.dur.s,new.record.row[9],new.record.row[10],
                              new.record.row[11])
        output.vect.s <- append(output.vect.s,new.record.row.s)
      }else{
        output.vect.s <- append(output.vect.s, new.record.row)
      }
    }
    
    
    if (!activity.checker){
      no.activity.vect <- append(no.activity.vect,ID)
    }
    
  }# end of for loop

    
  #The code below creates an output file in Excel including a comments tab, summarising 
  #how much data was excluded from analysis and why.
  output.mat <- matrix(output.vect,byrow = T, ncol = 11)
  colnames(output.mat) <- c("RxID","verifQueue","verifActivity","dispQueue","dispActivity","finCheckQueue",
                            "finCheckActivity","collQueue","arrAtVerifQueue","arrAtDispQueue",
                            "arrAtFinCheckQueue")
  output.df <- as.data.frame(output.mat)
  #Adding a column that adds all relevant queue and activity times for each prescription (without 
  #collection queue):
  output.df <- cbind(output.df,totThroughput = rowSums(output.df) - output.df$RxID - output.df$collQueue - output.df$arrAtVerifQueue - output.df$arrAtDispQueue - output.df$arrAtFinCheckQueue)
  
  #Output of durations/queues calculated considering suspension times:
  output.mat.s <- matrix(output.vect.s,byrow = T, ncol = 11)
  colnames(output.mat.s) <- c("RxID","verifQueue","verifActivity","dispQueue","dispActivity","finCheckQueue",
                              "finCheckActivity","collQueue","arrAtVerifQueue","arrAtDispQueue",
                              "arrAtFinCheckQueue")
  output.df.s <- as.data.frame(output.mat.s)
  #Adding a column that adds all relevant queue and activity times for each prescription (without 
  #collection queue):
  output.df.s <- cbind(output.df.s,totThroughput = rowSums(output.df.s) - output.df.s$RxID - output.df.s$collQueue - output.df.s$arrAtVerifQueue - output.df.s$arrAtDispQueue - output.df.s$arrAtFinCheckQueue)
  
  return (list(absTimes.df,output.df,output.df.s,out.of.range.vect,no.activity.vect))
}


library(shiny)
library(shinyWidgets)
library(shinyMatrix)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(xlsx)
library(readxl)
library(tools)
#library(scales)

source("app helpers.R")
source("rawDataTransformer.R")

# This JavaScript code resets the app:
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" 


unitGroups <- c("all", "OUT PATIENTS", "medical and surgical wards", "acute wards",
                "discharge lounge", "mental health wards", "clinics",
                "Women and Children Unit", "off-site wards")

# unitGroups <- c("all", "OUT PATIENTS", "medical and surgical wards", "acute wards",
#                 "FVRH D/L", "mental health wards", "clinics at FVRH",
#                 "Women and Children Unit", "CCH, FCH and SCH wards")

#defining default shiftpattern for resources (for simulator tab):
#(see "simulation model.R" for details about shifts)
defaultShiftMatrix <- matrix(
  c(
    c(0,1,1,1,1,1), #disp. pharmacist
    c(0,3,3,3,3,0), #ward pharmacist
    c(2,4,5,4,4,2), #pharm. tech.
    c(0,1,1,1,1,1)  #checking tech.
  ), 
  nrow = 4,
  byrow = T
)

colnames(defaultShiftMatrix) <- c("8-9 a.m.", "9-11 a.m.",
                                  "11 a.m.-1 p.m.", "1-3 p.m.",
                                  "3-5 p.m.", "5-6:30 p.m.")
rownames(defaultShiftMatrix) <- c(
  "disp. pharmacist", 
  "ward pharmacist", 
  "pharm. tech.", 
  "checking tech."
)

#which.max() can identify the position of the max. number in a vector or a data frame with 
#only one line

############################################################################################
############################################################################################
#Defining the user interface:
ui <- fluidPage(
  fluidRow(
    # titlePanel(title=div(id="dummy", img(height = 100,
    #                                      width = 100,
    #                                      src = "NHSFVlogo.png"))),
    column(
      width = 12, offset = 0,
      #Cannot, for the life of me, get the image below to show. Tried all the 
      #suggestions on Stackoverflow, can't get it to show. 
      # shiny::img(src='NHSFVlogo.png', align = "left", width = "50px", height = "50px"),
      tags$h2("Dispensing process monitor"),
      tags$h6("Please note the data displayed here is fictional. Any similarities to existing hospital dispensaries is coincidental."),
      verticalTabsetPanel(
        ##################
        ### KPI tab ######
        ##################
        verticalTabPanel(
          title = "KPIs", icon = icon("bullseye", "fa-1x", lib = "font-awesome"),
          box_height = "120px", 
          sidebarLayout(
            sidebarPanel(
              uiOutput(outputId = "dateRangeUIK"),
              shiny::p(
                textOutput(outputId = "dateRangeKText", 
                           inline = T), 
                style = "font-size:10px"
              ),
              selectInput(inputId = "wardsK", label = "units", unitGroups),
              uiOutput(outputId = "priorityKOutput"),
              sliderInput(inputId = "cutOffThroughputK", 
                          label = "cut-off time for overall completion",
                          min = 5, max = 150, value = 60,
                          step = 5, round = F, post = " min"),
              sliderInput(inputId = "cutOffDispensaryK", 
                          label = "cut-off time for dispensary activities only",
                          min = 5, max = 150, value = 60,
                          step = 5, round = F, post = " min"),
              checkboxInput(inputId = "suspendedK", 
                            label = "consider suspended periods",
                            value = F),
              shiny::p(
                "Please tick the box if you want suspended periods to be subtracted from respective activity or queue durations. Otherwise, occurring suspended periods will be ignored, i.e. they will be added to respective durations.", 
                style = "font-size:10px"
              ),
              br(),
              downloadButton(outputId = "exportToExcel", label = "download Excel file"),
              #actionButton(inputId = "exportToExcel", "export data"),
              shiny::p(
                "Please click button if you want to export tables and graphs to Excel.", 
                style = "font-size:10px"
              )
            ),
            mainPanel(
              fluidPage(
                fluidRow(
                  splitLayout(
                    cellWidths = c("50%","50%"),
                    plotOutput(outputId = "prescrWithinCutoffPlot", width = "200px", height = "250px"),
                    plotOutput(outputId = "prescrWithinDispCutoffPlot", width = "200px", height = "250px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    cellWidths = c("50%","50%"),
                    plotOutput(outputId = "overallWaiting", width = "200px", height = "250px"),
                    plotOutput(outputId = "dispensaryWaiting", width = "200px", height = "250px")
                  )
                ), 
                fluidRow(
                  shiny::p(textOutput(outputId = "numberOfAllPrescriptions", 
                                      inline = T),
                           textOutput(outputId = "numberOfWardPrescriptions",
                                      inline = T),
                           textOutput(outputId = "numberOfPriorPrescriptions",
                                      inline = T),
                           textOutput(outputId = "withinTotCutoff", 
                                      inline = T),
                           textOutput(outputId = "withinDispCutoff", 
                                      inline = T),
                           textOutput(outputId = "meanTotThroughput", 
                                      inline = T),
                           textOutput(outputId = "meanDispThroughput", 
                                      inline = T),
                           textOutput(outputId = "totAverageWaiting", 
                                      inline = T), 
                           textOutput(outputId = "totDispensaryWaiting", 
                                      inline = T),
                           style = "font-size:10px"
                  )
                ),
                br(),
                fluidRow(
                  shiny::div(
                    tableOutput(outputId = "processMetrics"),
                    style = "font-size: 9px; width: 100%"
                  ), 
                  shiny::div(
                    style = "font-size: 9px",
                    "* referring to those prescriptions that were not filtered out - see table about data quality below"
                  )
                ),
                br(),
                fluidRow(
                  shiny::div(
                    tableOutput(outputId = "targets"),
                    style = "font-size: 9px; width: 100%"
                  )
                ),
                br(),
                fluidRow(
                  shiny::div(
                    tableOutput(outputId = "durationsKPI"),
                    style = "font-size: 9px; width: 100%"
                  )
                ), 
                br(),
                fluidRow(
                  shiny::div(
                    tableOutput(outputId = "dataQuality"),
                    style = "font-size: 9px; width: 100%"
                  ),
                  shiny::div(
                    style = "font-size: 9px",
                    "** Prescription Tracker System",
                    tags$br(),
                    "*** activities below 15 seconds are unlikely to be accurate reflections of pharmacy performance and more likely due to recording errors "
                  )
                )
              )
            )
          )
        ),
        verticalTabPanel(
          title = "artificial variation", icon = icon("dice", "fa-1x", lib = "font-awesome"),
          box_height = "120px",
          #the date input widget only allows selection of days from Mon.-Fri. at least
          #a week before the last day in the dataset:
          sidebarLayout(
            sidebarPanel(
              uiOutput(outputId = "dateAOutput"),
              uiOutput(outputId = "weeksInput"),
              selectInput(inputId = "wardsA", label = "wards", unitGroups),
              selectInput(inputId = "stepA", label = "arrival at", 
                          c("verifying", "dispensing", "final checking"))
            ),
            mainPanel(
              plotOutput(outputId = "arrivalPlot")
            )
          )
        ),
        verticalTabPanel(
          title = "queues", icon = icon("hourglass-half", "fa-1x", lib = "font-awesome"),
          box_height = "120px", 
          #the date input widget only allows selection of days from Mon.-Fri. at least
          #a week before the last day in the dataset:
          sidebarLayout(
            sidebarPanel(
              uiOutput(outputId = "dateQOutput"),
              uiOutput(outputId = "weeksInputQ"),
              selectInput(inputId = "wardsQ", label = "wards", c("all", "OUT PATIENTS", "inpatient")),
              selectInput(inputId = "stepQ", label = "queue before", 
                          c("verifying", "dispensing", "final checking")),
              checkboxInput(inputId = "suspendedQ", 
                            label = "consider suspended periods",
                            value = F),
              h6("Please tick the box if you want suspended periods to be subtracted from respective activity or queue durations. Otherwise, occurring suspended periods will be ignored, i.e. they will be added to respective durations.")
            ),
            mainPanel(
              plotOutput(outputId = "queuePlotA", height = "250px"),
              br(),
              plotOutput(outputId = "queuePlotM", height = "250px")
            )
          )
        ),
        ####  #  #   #
        #  #  #  ## ##
        #     #  # # #
         ##   #  #   #
           #  #  #   #
        #  #  #  #   #
        ####  #  #   #
        verticalTabPanel(
          title = "simulation", icon = icon("robot", "fa-1x", lib = "font-awesome"),
          box_height = "120px", 
          sidebarLayout(
            sidebarPanel(
              width = 4,
              selectInput(inputId = "role", label = "choose role",
                          choices = c("disp. pharmacist", "ward pharmacist", 
                                      "pharm. tech.", "checking tech."), 
                          selected = "disp. pharmacist", width = "140px"),
              shiny::div(
                style = "display:inline-block;font-size: 75%; height: 75%; width: 50%",
                uiOutput(outputId = "arrivalDateSimOutput")
              ),
              bootstrapPage(
                checkboxInput(inputId = "suspendedS", 
                              label = "consider suspended periods",
                              value = F),
                checkboxInput(inputId = "arrivalS", 
                              label = "all prescriptions arrive at constant intervals",
                              value = F),
                shiny::div(
                  style = "white-space: nowrap;",
                  shiny::div(
                    style = "display:inline-block;font-weight: normal; height: 25%; width: 30%",
                    numericInput(inputId = "repeatS", label = "repeat simulation", 
                                 value = 1, min = 1, max = 100, step = 1, width = "75px")
                  ),
                  shiny::div(
                    style = "display:inline-block; font-size: 90%",
                    "times to increase accuracy"
                  )
                )
              ),
              uiOutput(outputId = "startSimulationButton")
            ),
            mainPanel(
              uiOutput(outputId = "shiftInput"),
              plotOutput(outputId = "simulatedDurations"), 
              shiny::p(
                style = "font-size:10px",
                textOutput(outputId = "textForExplanation", 
                           inline = T)
              ),
              shiny::div(
                tableOutput(outputId = "resultsTableS"),
                style = "font-size: 9px; width: 100%"
              ),
              shiny::div(
                tableOutput("shiftPatternTable"),
                style = "font-size: 7px; width: 30%"
              )
            )
          )
        ),
        verticalTabPanel(
          title = "uploading data", icon = icon("database", "fa-1x", lib = "font-awesome"),
          box_height = "120px",
          useShinyjs(),                                           # Include shinyjs in the UI
          extendShinyjs(text = jsResetCode, functions = c("reset")),
          shiny::div(
            tags$b("Currently uploaded datasets:")
          ),
          br(),
          #displaying currently uploaded datasets in a data-frame:
          shiny::div(
            tableOutput(outputId = "availableDataSets"),
            style = "font-size: 12px; width: 100%"
          ),
          br(),
          uiOutput(outputId = "selectDataSet"), 
          br(),
          #actionButton(inputId = "uploadFromExcel", "upload data"),
          fileInput(inputId = "uploadFromExcel", label = "choose an Excel file", 
                    accept = c(".xlsx", ".xls")),
          shiny::p(
            "Click button if you want to upload raw data from the Prescription Tracking System in Excel format.", 
            style = "font-size:11px"
          )
        ),
        verticalTabPanel(
          title = "user guide", icon = icon("user-cog", "fa-1x", lib = "font-awesome"),
          box_height = "120px",
          #tags$iframe(style="height:600px; width:100%", src="https://rstudio.com/wp-content/uploads/2016/05/base-r.pdf")),
          shiny::div(
            HTML(
              "<br>
              <p><b>User guide</b></p>

<p>This web application helps you explore data gathered by PTS (Prescription Tracker System) during the (prescribing and) dispensing process. The first three tabs named 'KPIs', 'artificial variation' and 'queues' present relevant aspects of the process that might help identify strategies to improve the efficiency of the process and utilisation of its resources. The 'simulation' tab, on the other hand, lets you explore a virtual day of the dispensing process and the impact of staffing on relevant performance indicators. Finally, the 'uploading data' tab facilitates processing of new data sets by the app. Please note that at most, only three months' data can be explored at a time by the app - this is to avoid long computation times. </p>

<p>In each of the tabs above you will find parameters that allow you to filter out certain aspects of the underlying data set. Please see the descriptions for the individual tabs below for details about their functioning. Please send questions about this app not answered by this guide to josef.elias@nhs.net.</p>

<p>You might find that the easiest way to familiarise yourself with this application is changing parameters in the tabs 'KPIs', 'artificial variation', 'queues' and 'simulation' and observe resulting graphs, texts and tables. </p>

<br>
<p><b>KPIs</b></p>

<p>The KPIs tab lets you explore several metrics of the process based on data recorded by PTS. The focus of the display lies on percentages of prescriptions completed within chosen cut-off points and proportions of throughput times (i.e the duration for a prescriptions to complete the process from verifying to final checking or dispensing to final checking, respectively) spent in queues. The dispensing process here is viewed to consist of three steps, i.e. verifying, dispensing and final checking, with each of these steps consisting of an activity, where value is added to the prescription, and a queue, where the prescription is waiting to be processed. </p>

<p>Please note that this application can only display data about prescriptions registered by PTS and of these only those where activities were recorded in a more-or-less logical order, able to be categorised into into the steps above. It is possible that not all prescriptions arriving in the dispensary are recorded on PTS. In a brief study of 326 paper prescriptions processed in the dispensary from May 20-22, 2019, 21 (6.4 %) were not registered on PTS. On the other hand, of 349 prescriptions registered in this period on PTS, only 305 (87.4 %) could be linked to the paper prescriptions above. This raises questions about how completely PTS data represent pharmacy and dispensary performance. Future audits might confirm whether this observation points to ongoing issues that need to be considered when interpreting data. </p>

<p>You can choose a date-range, the wards, the urgency and cut-off points for the prescription data to be displayed. </p>

<p>While the utility of the 'suspended' function on PTS might be debateable, it appears that its use is not consistent across the pharmacy department. The tick-box 'consider suspended periods' lets you interpret suspended periods in the tracker data as queues or activities, respectively (box not ticked), or subtract these periods from respective queue or activity durations (box ticked). You might find that for some metrics this is relevant while for others it is not. </p>

<p>When exploring the data, you might find that most prescriptions spend more than two thirds of their throughput time waiting to be processed by pharmacy staff. Focussing on reducing these queues appears to be a particularly promising avenue for possible future efforts of improving the efficiency of the process and utilisation of its resources. </p>

<p>A range of other performance indicators are displayed below the graphs in text or in tables. Another table on data quality displays the number of prescriptions eliminated before processing (mostly due to missing activities in the underlying data set) and indicates the reliability of the recording of activities by staff. High proportions of activity durations below 15 seconds might point to consistency issues regarding how times are registered. </p>

<br>
<p><b>Artificial Variation</b></p>

<p>The Artificial Variation tab allows you to visualise the distribution of prescriptions over a typical workday (Mon. to Fri. only) originating from chosen wards and at chosen, different steps of the process (refer to details about steps under the heading 'KPIs'). It is helpful to choose larger numbers of weeks and wards producing many prescriptions to display a more generalised picture. You will find that prescriptions do not arrive uniformly over the course of the day with peaks of demand often occurring in the late mornings and afternoons. The implication of this is highlighted in the next tab, called 'queues'. </p>

<br>
<p><b>Queues</b></p>

<p>The Queues tab aims at displaying average and median queue lengths over the course of a typical working day (Mon.-Fri.). Giving a meaningful display of this data is difficult due to the high variation of waiting times prescriptions are subjected to. Nevertheless, some general themes emerge when choosing larger timeframes and inpatient wards. As outlined under the heading 'KPIs' above, waiting times are a particular challenge of the dispensing process, and these seem to escalate for inpatients around lunchtime (in the dispensing step) and the early afternoon (in the final checking step), coinciding with higher than usual demand on these activities. It is also possible that staff breaks around this time impacts on waiting times. </p>

<br>
<p><b>Simulation</b></p>

<p>The Simulation tab lets you explore the relationship between staffing levels during a typical weekday (Mon.-Fri.) and queue as well as throughput times. In this simulation of a working day, the dispensary pharmacist verifies all outpatient scripts and also helps with final checking. Ward pharmacists are not represented in their numbers comparable to their availability on wards at the hospital and should be seen as an indication for the resource available to verify inpatient scripts only. It would be very difficult to exactly quantify this resource in a simulator as data on how much of their time ward pharmacists spend on verifying prescriptions is not easily quantifiable. Pharmacy technicians only dispense medication and final checkers only final check prescriptions in this simple simulation model - their numbers might better represent actual observations in the dispensary. </p>

<p>On a virtual working day, prescriptions arrive at exactly those times as on a past day of your choice. You can also choose to consider (tick) or ignore (untick) suspended periods - see heading KPI above for discussion about suspended periods - and you can further simulate prescriptions arriving uniformly during the day. </p>

<p>The underlying simulation model chooses activity durations randomly for a prescription at hand, however some restrictions apply. For instance, only durations that have occurred in reality during the chosen day are attributed to activities. Representing activity times with simpler mathematical models, such as exponential distributions, would fit the underlying data set very poorly and would make it difficult to compare simulation results with observations on a real day of the dispensary. </p>

<p>Earlier work on the simulation model included attempts at considering additional prescription characteristics, such as the presence of controlled drugs, blister packs, or part-packs, respectively, however very large variation in the data mentioned above, potentially due to inconsistencies when registering activities on PTS, as mentioned above, meant that a compelling relationship of these characteristics with activity durations of respective prescriptions could not yet be demonstrated. </p>

<p>You might find that results for some simulated days conform better with observations on the chosen day than others. It is worth mentioning that queues seem to build up ahead of the final checking activity in reality and in spite of short recorded activity times in this respect. The simulation does not usually show these queues and it is likely that activity durations for final checking recorded on PTS are not consistently reflecting realistic durations. For more reliable simulation results it is worth repeating the simulation various times (e.g. 10 times or more). Please note, however that the simulator is the most computationally intensive part of this application and responses can be delayed substantially, in particular for many repetitions of the simulation runs. </p>

<br>
<p><b>Uploading data</b></p>

<p>As outlined above, the underlying data is arbitrarily organised into blocks not larger than three months, i.e. at most data for a quarter of a given year can be investigated. This is to maintain responsiveness in the app and to avoid longer delays possible with the processing of large data sets. </p>

<p>This app allows you to upload PTS raw data in Excel format. Please note that it will only allow one data set to be held for each quarter, e.g. Jan. to March, of a given year, i.e. it will try to combine a possibly existing data set with a uploaded data, as long as they fit into a given quarter and one period immediately follows another. To achieve this, you need to make sure that the first day of the uploaded data set immediately follows the last day of the existing data set or vice versa, i.e. the last day of the uploaded data set immediately precedes the first day of an existing data set. You also need to ensure that each day of an uploaded data set firmly falls within a quarter of a year. On successfully uploading a new data set, the app will automatically re-start, displaying the first, i.e. 'KPIs', tab. </p>

<p>You can choose which underlying data set the app is to display or analyse. </p>
<br>

<p>This application was created using R (including its packages Shiny and Simmer). Please contact Josef Elias (josef.elias@nhs.net) for feed-back and questions. </p>
"
            )
          )
        ),
        contentWidth = 10
      )
    )
  )
)

############################################################################################
############################################################################################
#Processing of inputs and preparing them for output on
#the user interface:
server <- function(input, output) {
  
  #the reactives below (and above the line of ####) are used by several tabs, 
  #not just one
  availableDataSetsV <- reactive({ #this reactive returns a vector of the names of available
                                   #data sets
    list.files(path = "data") %>% 
      .[((substr(.,start = 1,stop = 2) == "fa")&(substr(.,start = 26,stop = 28) == "rds"))]
  })
  
  #Uploading of underlying data - currentRawData() is a list made up of three
  #data-frames (absolute times; calculated durations ignoring and considering suspended
  #periods) and two vectors (out of range records, rejected records due to missing data).
  #The first dataset in the estabilshed with the vector availableDataSets is initially
  #used as underlying data:
  #transformedRawData.df <- 
  currentRawData <- reactive({
    req(availableDataSetsV())
    if (is.null(input$selectedDataSet)){
      return(
        readRDS(file = paste0(
          "data/",
          availableDataSetsV()[1]
          )
        )
      )
    }else{
      return(
        readRDS(file = paste0(
          "data/",
          input$selectedDataSet
          )
        )
      )
    }
  })
  
  #Uploading relevant data for analysis:
  absTimes.df <- reactive({
    req(currentRawData())
    return(
      currentRawData()[[1]] %>% 
        cbind(dayOfWeek = weekdays(.$startOfVerifQueu))
    )
  })
  
  #vector with all unique wards in current dataset:
  allWards <- reactive({
    req(absTimes.df())
    return(
      absTimes.df() %>% pull(unit) %>% 
        unique() %>% as.character()
    )
  })
  
  observeEvent(eventExpr = allWards(), {
    req(medAndSurgWards())
    req(acuteWards())
    req(mentalHealthWards())
    req(clinics())
    req(childrenWards())
    req(outsideFVRHWards())
    notIncludedInGroups <- allWards()[
        !(
          allWards() %in% 
            c(medAndSurgWards(),
              acuteWards(),
              mentalHealthWards(), 
              clinics(),
              childrenWards(), 
              outsideFVRHWards(), 
              unitGroups, 
              "FVRH D/L")
        )
      ]
    #browser()
    if (length(notIncludedInGroups) > 0){
      return(
        showNotification(paste0(
          "Please note the following units in the current data set are not included in the groups available for selection: ", 
          notIncludedInGroups
        ), duration = 45, closeButton = T, type = "error")
      )
    }else{
      return(
        showNotification("All the units in the current data set are included in the groups available for selection above.", 
                         duration = 15, closeButton = T)
      )
    }
  })
  
  #vector with all medical and surgical wards in current dataset:
  medAndSurgWards <- reactive({
    req(absTimes.df())
    #searchVector contains strings that the algorithm below tries
    #to find in the ward names of the current data set:
    searchVector <- c("A11", "A12", "A21", "A22", "A31", 
                                    "A32", "B11", "B12", "B21", 
                                    "B23", "B31", "B32", "cardiology", 
                                    "ICU")
    return(
      #The construct below returns a vector with all actual ward names
      #in the current data set that contain or are equal to elements 
      #of searchVector (upper and lower case are ignored)
      allWards()[lapply(lapply(searchVector, FUN = grepl, 
                               x = allWards(), ignore.case = T), 
                        FUN = which) %>% 
                   unlist()] %>% 
        unique() 
    )
  })
  
  #vector with all acute wards in current dataset:
  acuteWards <- reactive({
    req(absTimes.df())
    #I'm not entirely sure what SSMU is - I think it is an 
    #acute wards ...
    searchVector <- c("AAU", "CAU", "SAU", "ED Bedded", "SSMU", "Short Stay")
    return(
      allWards()[lapply(lapply(searchVector, FUN = grepl, 
                               x = allWards(), ignore.case = T), 
                        FUN = which) %>% 
                   unlist()] %>% 
        unique()
    )
  })
  
  #vector containing all mental health wards in dataset:
  mentalHealthWards <- reactive({
    req(absTimes.df())
    #I'm not entirely sure what SSMU is - I think it is an 
    #acute wards ...
    searchVector <- c("LOCHVIEW", "MH", "RUSSELL PARK", "TRYSTPARK", 
                      "TRYSTVIEW", "BO'NESS")
    return(
      allWards()[lapply(lapply(searchVector, FUN = grepl, 
                               x = allWards(), ignore.case = T), 
                        FUN = which) %>% 
                   unlist()] %>% 
        unique()
    )
  })
  
  #vector containing all clinic names in dataset:
  clinics <- reactive({
    req(absTimes.df())
    searchVector <- c("Day", "ENDOSCOPY", "renal", "rehab")
    return(
      allWards()[lapply(lapply(searchVector, FUN = grepl, 
                               x = allWards(), ignore.case = T), 
                        FUN = which) %>% 
                   unlist()] %>% 
        unique()
    )
  })
  
  #vector containing all children wards in dataset:
  childrenWards <- reactive({
    req(absTimes.df())
    searchVector <- c("children", "NNU", "Ward 6", "Ward 7", 
                      "Ward 8", "ANTE")
    return(
      allWards()[lapply(lapply(searchVector, FUN = grepl, 
                               x = allWards(), ignore.case = T), 
                        FUN = which) %>% 
                   unlist()] %>% 
        unique()
    )
  })
  
  #vector containing hospital wards outside FVRH:
  outsideFVRHWards <- reactive({
    req(absTimes.df())
    searchVector <- c("CCH", "FCH", "SCH", "SCV")
    return(
      allWards()[lapply(lapply(searchVector, FUN = grepl, 
                               x = allWards(), ignore.case = T), 
                        FUN = which) %>% 
                   unlist()] %>% 
        unique()
    )
  })
  
  
  #adding another column to the durations data-frames about the durations
  #of the steps in the dispensary only (i.e. dispensing and final checking)
  durations.df <- reactive({
    currentRawData()[[2]] %>%
      cbind(totDispThroughput = .$dispQueue+.$dispActivity+.$finCheckQueue+.$finCheckActivity)
  })   
  
  durationsS.df <- reactive({
    currentRawData()[[3]] %>%
      cbind(totDispThroughput = .$dispQueue+.$dispActivity+.$finCheckQueue+.$finCheckActivity)
  })
  
  outOfRange.df <- reactive({ #why isn't this used anywhere?
    currentRawData()[[4]]
  })
  
  rejected.df <- reactive({ #why isn't this used anywhere?
    currentRawData()[[5]]
  })
  
  #Finding the first and last date in absTimes.df:
  earliestTime <- reactive({
    pull(absTimes.df(),startOfVerifQueu) %>% 
      min()
  })
    
  latestTime <- reactive({
    pull(absTimes.df(),startOfVerifQueu) %>% 
      max()
  })
    
  earliestDate.chr <- reactive({
    req(earliestTime())
    return(
      earliestTime() %>% 
        strftime(format = "%d/%m/%Y")
    )
  })
    
  latestDate.chr <- reactive({
    req(latestTime())
    return(
      latestTime() %>% 
        strftime(format = "%d/%m/%Y")
    )
  })
    
  #Establishing of unique wards in whole dataset and adding "all":
  uniqueWards.vect <- reactive({
    req(absTimes.df())
    return(
      absTimes.df() %>% distinct(unit) %>% pull(unit) %>%
        as.character() %>% sort(decreasing = F) %>% append("all", after = 0)
    )
  })
    
  uniquePriorAll.vect <- reactive({
    req(absTimes.df())
    return(
      absTimes.df() %>% distinct(priority) %>%
        pull(priority) %>% as.character() %>% 
        sort(decreasing = F) %>% append("all", after = 0)
    )
  })
    
    
  ##########################################################################################
  #for KPI tab:
  output$dateRangeUIK <- renderUI({
    req(earliestTime())
    req(latestTime())
    return(
      dateRangeInput(inputId = "dateRangeK", label = "date range", 
                     start = as.Date(earliestTime()), end = as.Date(latestTime()),
                     min = as.Date(earliestTime()), max = as.Date(latestTime()))
    )
  })
  
  output$dateRangeKText <- renderText({
    req(earliestDate.chr())
    req(latestDate.chr())
    return(
      paste0("Choose any period between ",
             earliestDate.chr(), 
             " and ",
             latestDate.chr(),".")
    )
  })
  
  #for KPIs tab (see explanation under outpatient's tab):
  #decided to separate the input$dateRange for each tab, as not doing so 
  #doesn't produce an error, rather sluggish behaviour and sometimes wrong
  #output
  beginK <- reactive({
    req(input$dateRangeK)
    return(as.POSIXct(input$dateRangeK[1], tz = "UTC"))
  })
  
  endK <- reactive({
    req(input$dateRangeK)
    return(as.POSIXct(input$dateRangeK[2], tz = "UTC")+66600)
    #the above is to ensure that 18.5 hours (66600 sec) are added to the 
    #chosen date and thereby all prescriptions of that workday are being 
    #considered below
  })
  
  #for KPIs tab:
  #updating data-frame according to chosen date-range:
  absTimesRangeUpdate <- reactive({
    req(beginK())
    req(endK())
    req(absTimes.df())
    return(
      absTimes.df() %>% 
        filter((startOfVerifQueu >= beginK())&(startOfVerifQueu <= endK()))
    )
  })
  
  #for KPIs tab:
  output$numberOfAllPrescriptions <- renderText({
    req(absTimesRangeUpdate())
    req(input$dateRangeK)
    paste0(
      absTimesRangeUpdate() %>% pull(RxID) %>%
        length() %>% as.character(),
      " prescriptions were processed in total between ",
      input$dateRangeK[1] %>% as.character(),
      " and ",
      input$dateRangeK[2] %>% as.character(),
      ". "
    )
  })
  
  #for KPIs tab:
  #updating data-frame according to ward chosen:
  absTimesWardsUpdate <- reactive({
    req(input$wardsK)
    if (input$wardsK == "all"){
      absTimesRangeUpdate()
    }else if (input$wardsK == "medical and surgical wards"){
      absTimesRangeUpdate() %>% filter(unit %in% medAndSurgWards())
    }else if (input$wardsK == "acute wards"){
      absTimesRangeUpdate() %>% filter(unit %in% acuteWards())
    }else if (input$wardsK == "mental health wards"){
      absTimesRangeUpdate() %>% filter(unit %in% mentalHealthWards())
    }else if (input$wardsK == "clinics"){
      absTimesRangeUpdate() %>% filter(unit %in% clinics())
    }else if (input$wardsK == "Women and Children Unit"){
      absTimesRangeUpdate() %>% filter(unit %in% childrenWards())
    }else if (input$wardsK == "off-site wards"){
      absTimesRangeUpdate() %>% filter(unit %in% outsideFVRHWards())
    }else if (input$wardsK == "discharge lounge"){
      absTimesRangeUpdate() %>% filter(unit == "FVRH D/L")
    }else{
      absTimesRangeUpdate() %>% filter(unit == input$wardsK)
    }
  })
  
  output$priorityKOutput <- renderUI({
    req(absTimes.df())
    req(uniquePriorAll.vect())
    return(
      selectInput(inputId = "priorityK", label = "urgency", 
                  choices = uniquePriorAll.vect(), 
                  selected = "all")
    )
  })
  
  #for KPIs tab:
  #updating data-frame according to chosen priority:
  absTimesPiorUpdate <- reactive({
    req(input$priorityK)
    req(absTimesWardsUpdate())
    if (input$priorityK == "all"){
      absTimesWardsUpdate()
    }else{
      absTimesWardsUpdate() %>% filter(priority == input$priorityK)
    }
  })
  
  #for KPIs tab:
  #output of number of prescriptions updated according to chosen ward:
  output$numberOfWardPrescriptions <- renderText({
    req(input$wardsK)
    req(absTimesWardsUpdate())
    if (input$wardsK == "all"){
      ""
    }else{
      paste0(
        "Of these, ",
        absTimesWardsUpdate() %>% pull(RxID) %>% length() %>%
          as.character(), 
        " were from ",
        input$wardsK,
        ". "
      )
    }
  })
  
  #for KPIs tab:
  #calculating number of prescriptions as per chosen ward and 
  #priority:
  totalNumberAsChosen <- reactive({
    req(absTimesPiorUpdate())
    absTimesPiorUpdate() %>% pull(RxID) %>% length()
  })
  
  #for KPIs tab:
  #output of number of prescriptions updated according to chosen priority:
  output$numberOfPriorPrescriptions <- renderText({
    req(input$priorityK)
    req(totalNumberAsChosen())
    if (input$priorityK == "all"){
      ""
    }else{
      paste0(
        totalNumberAsChosen() %>%
          as.character(),
        " of those were ",
        input$priorityK,
        " prescriptions. "
      )
    }
  })
  
  #for KPIs tab:
  #extracting vector of RxIDs of data-frame chosen by user (as per
  #ward and priority):
  RxIDvector <- reactive({
    req(absTimesPiorUpdate())
    absTimesPiorUpdate() %>% pull(RxID) 
  })
  
  #for KPIs tab:
  #update of durations dataframes dependent on chosen ward, priority
  #and consideration of suspended periods:
  durationsUpdate <- reactive({
    req(durationsS.df())
    req(durations.df())
    req(RxIDvector())
    if (input$suspendedK){
      durationsS.df() %>% filter(RxID %in% RxIDvector())
    }else{
      durations.df() %>% filter(RxID %in% RxIDvector())
    }
  })
  
  #for KPIs tab:
  #this reactive returns a vector of two values; one is the number
  #of prescriptions completed within a chosen cut-off point, the other 
  #the percentage of all prescriptions as per chosen ward and priority
  prescrWithinCutoff <- reactive({
    # req(durationsUpdate())
    # req(totalNumberAsChosen())
    numberWithinCutoff <- durationsUpdate() %>% 
      filter(totThroughput <= (input$cutOffThroughputK * 60)) %>%
      nrow()
    if (totalNumberAsChosen() == 0){
      proportionWithinCutoff <- 0
    }else{
      proportionWithinCutoff <- (numberWithinCutoff/totalNumberAsChosen())
    }
    return(
      c(
        numberWithinCutoff,
        proportionWithinCutoff
      )
    )
  })
  
  #for KPIs tab:
  #this reactive returns a vector of two values; one is the number
  #of prescriptions completed within a chosen dispensary cut-off point, the other 
  #the percentage of all prescriptions as per chosen ward and priority
  prescrWithinDispCutoff <- reactive({
    #req(durationsUpdate())
    #req(totalNumberAsChosen())
    numberWithinCutoff <- durationsUpdate() %>% 
      filter(totDispThroughput <= (input$cutOffDispensaryK * 60)) %>%
      nrow()
    if (totalNumberAsChosen() == 0){
      proportionWithinCutoff <- 0
    }else{
      proportionWithinCutoff <- (numberWithinCutoff/totalNumberAsChosen())
    }
    return(
      c(
        numberWithinCutoff,
        proportionWithinCutoff
      )
    )
  })
  
  prescrWithinCutoffPlotImage <- reactive({
    #req(prescrWithinCutoff())
    withinCutoff <- prescrWithinCutoff()[2] %>%
      "*"(100) %>%
      round(digits = 1)
    dataForPlot <- data.frame(
      "group" = c("within cutoff", ""),
      "percentage" = c(
        withinCutoff,
        (100-withinCutoff)
      )
    )
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_line(color = "#006400", linetype = 1, size = 1),
        axis.ticks = element_blank(),
        axis.text.x=element_blank(), 
        legend.key=element_blank(),
        legend.text=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title=element_text(size=10, face="plain", hjust = 0.5)
      )
    return(
      ggplot(data = dataForPlot, aes(x = "", y = percentage, fill = group)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0) +
        scale_fill_manual(values=c("#FFFFFF", "#AFFFB6")) +
        blank_theme +
        ggtitle(
          paste0(
            withinCutoff %>%
              as.character(),
            "% of prescriptions were \n completed within ",
            input$cutOffThroughputK %>% as.character(),
            " minutes \n (total process)"
          )
        )
    )
  })
  
  output$prescrWithinCutoffPlot <- renderPlot({
      prescrWithinCutoffPlotImage()
  })
  

  prescrWithinDispCutoffImage <- reactive({
    req(prescrWithinDispCutoff())
    withinCutoff <- prescrWithinDispCutoff()[2] %>%
      "*"(100) %>%
      round(digits = 1)
    dataForPlot <- data.frame(
      "group" = c("within cutoff", ""),
      "percentage" = c(
        withinCutoff,
        (100-withinCutoff)
      )
    )
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_line(color = "#006C6B", linetype = 1, size = 1),
        axis.ticks = element_blank(),
        axis.text.x=element_blank(), 
        legend.key=element_blank(),
        legend.text=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title=element_text(size=10, face="plain", hjust = 0.5)
      )
    return(
      ggplot(data = dataForPlot, aes(x = "", y = percentage, fill = group)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0) +
        scale_fill_manual(values=c("#FFFFFF", "#A5FFFF")) +
        blank_theme +
        ggtitle(
          paste0(
            withinCutoff %>%
              as.character(),
            "% of prescriptions were \n completed within ",
            input$cutOffDispensaryK %>% as.character(),
            " minutes \n (within dispensary only)"
          )
        )
    )
  })
  
  output$prescrWithinDispCutoffPlot <- renderPlot({
      prescrWithinDispCutoffImage()
  })


  
  #for KPIs tab:
  #calculating and output of number of prescriptions (from those chosen as 
  #above) within overall cutoff time:
  output$withinTotCutoff <- renderText({
    req(prescrWithinCutoff())
    return(
      paste0(
        "From registration on the tracker until the end of final checking, ",
        prescrWithinCutoff()[1] %>% as.character(),
        " (",
        prescrWithinCutoff()[2] %>%
          "*"(100) %>%
          round(digits = 1) %>%
          as.character(),
        "%)", 
        " prescriptions were completed within ",
        input$cutOffThroughputK %>% as.character(),
        " minutes. "
      )
    )
  })
  
  #for KPIs tab:
  #calculating and output of number of prescriptions (from those chosen as 
  #above) within dispensary cutoff time:
  output$withinDispCutoff <- renderText({
    req(prescrWithinDispCutoff())
    return(
      paste0(
        "In the dispensary, ",
        prescrWithinDispCutoff()[1] %>% as.character(),
        " (",
        prescrWithinDispCutoff()[2] %>%
          "*"(100) %>%
          round(digits = 1) %>%
          as.character(),
        "%)", 
        " were completed within ", 
        input$cutOffDispensaryK %>% as.character(), 
        " minutes. "
      )
    )
  })
  
  #for KPIs tab:
  #extracting total throughput times as vector from data-frame as 
  #chosen above:
  totThroughVector <- reactive({
    req(durationsUpdate())
    return(
      durationsUpdate() %>% pull(totThroughput)
    )
  })
  
  #for KPIs tab:
  #extracting dispensary throughput times as vector from data-frame as 
  #chosen above:
  dispThroughVector <- reactive({
    req(durationsUpdate())
    return(
      durationsUpdate() %>% pull(totDispThroughput)
    )
  })
  
  #for KPIs tab:
  #extracting the vector with the total waiting times in the 
  #process:
  totWaitingVector <- reactive({
    req(durationsUpdate())
    return(
      durationsUpdate() %>% cbind(totQueue = .$verifQueue + .$dispQueue + .$finCheckQueue) %>%
        pull(totQueue)
    )
  })
  
  #for KPIs tab:
  #extracting the waiting times in the dispensary as a vector:
  dispWaitingVector <- reactive({
    req(durationsUpdate())
    return(
      durationsUpdate() %>% cbind(dispensaryQueue = .$dispQueue + .$finCheckQueue) %>%
        pull(dispensaryQueue)
    )
  })
  
  #for KPIs tab:
  #calculating average waiting times of the selected prescriptions:
  averageWaiting <- reactive({
    req(totWaitingVector())
    if (length(totWaitingVector()) == 0){
      c(0,0,0,0)
    }else if (length(totWaitingVector()) == 1){
      c(
        totWaitingVector() %>% mean(),
        0,
        dispWaitingVector() %>% mean(), 
        0
      )
    }else{
      c(
        totWaitingVector() %>% mean(),
        totWaitingVector() %>% sd(),
        dispWaitingVector() %>% mean(), 
        dispWaitingVector() %>% sd()
      )
    }
  })
  
  #for KPIs tab:
  #calculating average throughput times for overall and dispensary
  #process - return of a vector with the mean value and the standard 
  #deviations:
  averageThroughDur <- reactive({
    req(totThroughVector())
    if (length(totThroughVector()) == 0){
      c(0,0,0,0) #this is to prevent error messages in the app if there
      #are no prescription with the chosen criteria as above
    }else if (length(totThroughVector()) == 1){
      c(
        totThroughVector() %>% mean(), 
        0,
        dispThroughVector() %>% mean(),
        0 #this is to prevent error messages if criteria only apply to 
      ) # one presccription
    }else{
      c(
        totThroughVector() %>% mean(), 
        totThroughVector() %>% sd(),
        dispThroughVector() %>% mean(),
        dispThroughVector() %>% sd()
      )
    }
  })
  
  #for KPIs tab:
  #output of average throughput time and its standard deviation:
  output$meanTotThroughput <- renderText({
    req(averageThroughDur())
    return(
      paste0(
        "The average total throughput time was ",
        averageThroughDur()[1] %>% 
          secondsToHoursAndMinText(),
        " (standard deviation: ",
        averageThroughDur()[2] %>%
          secondsToHoursAndMinText(),
        "). "
      )
    )
  })
  
  #for KPIs tab:
  #output of average disp. throughput time and its standard deviation:
  output$meanDispThroughput <- renderText({
    req(averageThroughDur())
    return(
      paste0(
        "Processing time in the dispensary was, on average, ",
        averageThroughDur()[3] %>% 
          secondsToHoursAndMinText(),
        " (standard deviation: ",
        averageThroughDur()[4] %>%
          secondsToHoursAndMinText(),
        "). "
      )
    )
  })
  
  #for KPIs tab:
  #output of average total waiting times:
  output$totAverageWaiting <- renderText({
    req(averageWaiting())
    return(
      paste0(
        "For the whole process, prescriptions waited, on average, ",
        averageWaiting()[1] %>%
          secondsToHoursAndMinText(), 
        " (standard deviation: ", 
        averageWaiting()[2] %>%
          secondsToHoursAndMinText(),
        "), i.e. ",
        (averageWaiting()[1]/averageThroughDur()[1]) %>%
          "*"(100) %>%
          round(digits = 1) %>%
          as.character(),
        "% of the average total throughput time. "
      )
    )
  })
  
  #for KPIs tab:
  #output of average total waiting times:
  output$totDispensaryWaiting <- renderText({
    req(averageWaiting())
    return(
      paste0(
        "Within the dispensary only, prescriptions waited, on average ",
        averageWaiting()[3] %>%
          secondsToHoursAndMinText(), 
        " (standard deviation: ", 
        averageWaiting()[4] %>%
          secondsToHoursAndMinText(),
        "), i.e. ",
        (averageWaiting()[3]/averageThroughDur()[3]) %>%
          "*"(100) %>%
          round(digits = 1) %>%
          as.character(),
        "% of the average dispensary throughput time."
      )
    )
  })
  
  #for KPIs tab:
  #this returns a vector of two percentages, one reflecting the 
  #time prescriptions spent waiting in the overall process, the other
  #the time spent waiting within the dispensary only:
  proportionWaiting <- reactive({
    req(averageWaiting())
    req(averageThroughDur())
    return(
      c(
        (averageWaiting()[1]/averageThroughDur()[1]),
        (averageWaiting()[3]/averageThroughDur()[3])
      )
    )
  })
  
 
  overallWaitingImage <- reactive({
    req(proportionWaiting())
    timeSpentWaitingPerc <- proportionWaiting()[1] %>%
      "*"(100) %>%
      round(digits = 1)
    dataForPlot <- data.frame(
      "group" = c("activity", "waiting"),
      "percentage" = c(
        (100 - timeSpentWaitingPerc),
        timeSpentWaitingPerc
      )
    )
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_line(color = "#A55428", linetype = 1, size = 1),
        axis.ticks = element_blank(),
        axis.text.x=element_blank(), 
        legend.key=element_blank(),
        legend.text=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title=element_text(size=10, face="plain", hjust = 0.5)
      )
    return(
      ggplot(data = dataForPlot, aes(x = "", y = percentage, fill = group)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0) +
        scale_fill_manual(values=c("#FFFFFF", "#E8B56F")) +
        blank_theme +
        ggtitle(
          paste0(
            timeSpentWaitingPerc %>%
              as.character(),
            "% of throughput time \n spent waiting "
          )
        )
    )
  })
  
  #for KPIs tab:
  #display of proportion of time prescriptions spent waiting (overall process):
  output$overallWaiting <- renderPlot({
      overallWaitingImage()
  })

  
  dispensaryWaitingImage <- reactive({
    req(proportionWaiting())
    timeSpentWaitingPerc <- proportionWaiting()[2] %>%
      "*"(100) %>%
      round(digits = 1)
    dataForPlot <- data.frame(
      "group" = c("activity", "waiting"),
      "percentage" = c(
        (100 - timeSpentWaitingPerc),
        timeSpentWaitingPerc
      )
    )
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_line(color = "#BB6D6F", linetype = 1, size = 1),
        axis.ticks = element_blank(),
        axis.text.x=element_blank(), 
        legend.key=element_blank(),
        legend.text=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title=element_text(size=10, face="plain", hjust = 0.5)
      )
    return(
      ggplot(data = dataForPlot, aes(x = "", y = percentage, fill = group)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0) +
        scale_fill_manual(values=c("#FFFFFF", "#FFC6C4")) +
        blank_theme +
        ggtitle(
          paste0(
            timeSpentWaitingPerc %>%
              as.character(),
            "% of dispensary throughput \n time spent waiting "
          )
        )
    )
  })
  
  
  #for KPIs tab:
  #display of proportion of time prescriptions spent waiting (overall process):
  output$dispensaryWaiting <- renderPlot({
      dispensaryWaitingImage()
  })
  
  #for KPI tab:
  #creating of dataframe of relevant process metrics to be used 
  #for output as in table further below
  processMetricsDF <- reactive({
    req(input$dateRangeK)
    req(absTimesRangeUpdate())
    req(absTimesPiorUpdate())
    req(endK())
    req(input$priorityK)
    req(input$wardsK)
    returnDF <- data.frame(
      chosen_prescriptions = c(
        paste0(
          "all arrived prescriptions between ",
          input$dateRangeK[1] %>% as.character(),
          " and ",
          input$dateRangeK[2] %>% as.character(), 
          "*"
        ),
        paste0(
          "completed prescriptions between ",
          input$dateRangeK[1] %>% as.character(),
          " and ",
          input$dateRangeK[2] %>% as.character()
        )
      ),
      number = c(
        absTimesRangeUpdate() %>% nrow(),
        absTimesRangeUpdate() %>% filter(endOfFinCheckAct <= endK()) %>%
          nrow()
      )
    )
    if (input$wardsK != "all"){
      addedRowsAboutWardsDF <- data.frame(
        chosen_prescriptions = c(
          paste0(
            "arrived prescriptions for ", 
            input$wardsK %>% as.character(), 
            " between ",
            input$dateRangeK[1] %>% as.character(),
            " and ",
            input$dateRangeK[2] %>% as.character()
          ), 
          paste0(
            "completed prescriptions for ", 
            input$wardsK %>% as.character(), 
            " between ",
            input$dateRangeK[1] %>% as.character(),
            " and ",
            input$dateRangeK[2] %>% as.character()
          )
        ),
        number = c(
          absTimesWardsUpdate() %>% nrow(),
          absTimesWardsUpdate() %>% filter(endOfFinCheckAct <= endK()) %>%
            nrow()
        )
      )
      returnDF <- returnDF %>% rbind(addedRowsAboutWardsDF)
    }
    if (input$priorityK != "all"){
      if (input$wardsK == "all"){
        addedRowsAboutPriorityDF <- data.frame(
          chosen_prescriptions = c(
            paste0(
              "arrived prescriptions for all wards between ", 
              input$dateRangeK[1] %>% as.character(),
              " and ",
              input$dateRangeK[2] %>% as.character(), 
              " (",
              input$priorityK %>% as.character(),
              ")"
            ), 
            paste0(
              "completed prescriptions for all wards between ", 
              input$dateRangeK[1] %>% as.character(),
              " and ",
              input$dateRangeK[2] %>% as.character(), 
              " (",
              input$priorityK %>% as.character(),
              ")"
            )
          ), 
          number = c(
            absTimesPiorUpdate() %>% nrow(), 
            absTimesPiorUpdate() %>% filter(endOfFinCheckAct <= endK()) %>%
              nrow()
          )
        )
      }else{
        addedRowsAboutPriorityDF <- data.frame(
          chosen_prescriptions = c(
            paste0(
              "arrived prescriptions for ", 
              input$wardsK %>% as.character(), 
              " between ",
              input$dateRangeK[1] %>% as.character(),
              " and ",
              input$dateRangeK[2] %>% as.character(),
              " (",
              input$priorityK %>% as.character(),
              ")"
            ), 
            paste0(
              "completed prescriptions for ", 
              input$wardsK %>% as.character(), 
              " between ", 
              input$dateRangeK[1] %>% as.character(),
              " and ",
              input$dateRangeK[2] %>% as.character(), 
              " (",
              input$priorityK %>% as.character(),
              ")"
            )
          ), 
          number = c(
            absTimesPiorUpdate() %>% nrow(), 
            absTimesPiorUpdate() %>% filter(endOfFinCheckAct <= endK()) %>%
              nrow()
          )
        )
      }
      returnDF <- returnDF %>% rbind(addedRowsAboutPriorityDF)
    }
    return(returnDF)
  })
  
  #for KPI tab
  #rendering table for output of relevant metrics of the process
  output$processMetrics <- renderTable({
    req(processMetricsDF())
    if (!(processMetricsDF() %>% is.null())){
      return(processMetricsDF())
    }
  })
  
  #for KPI tab:
  #creating a data-frame to display percentages of prescriptions completed
  #within a chosen period
  targetsDF <- reactive({
    req(input$wardsK)
    req(input$priorityK)
    req(input$dateRangeK)
    req(prescrWithinCutoff())
    req(prescrWithinDispCutoff())
    if (input$wardsK == "all"){
      chosenWards <- "all wards"
    }else{
      chosenWards <- input$wardsK
    }
    if (input$priorityK == "all"){
      chosenPriority <- " (all priorities)"
    }else{
      chosenPriority <- paste0(" (",input$priorityK,")")
    }
    #browser()
    return(
      data.frame(
        target = c(
          paste0(
            "prescriptions completed for whole process within ",
            input$cutOffThroughputK %>% as.character(),
            " min. for ",
            chosenWards,
            chosenPriority,
            " between ",
            input$dateRangeK[1] %>% as.character(),
            " and ",
            input$dateRangeK[2] %>% as.character()
          ),
          paste0(
            "prescriptions completed within ",
            input$cutOffDispensaryK %>% as.character(),
            " min. for ",
            chosenWards,
            chosenPriority, 
            " in the dispensary only",
            " between ",
            input$dateRangeK[1] %>% as.character(),
            " and ",
            input$dateRangeK[2] %>% as.character()
          )
        ),
        number = c(
          prescrWithinCutoff()[1] %>% as.integer(),
          prescrWithinDispCutoff()[1] %>% as.integer()
        ),
        percentage = c(
          prescrWithinCutoff()[2] %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1),
          prescrWithinDispCutoff()[2] %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1)
        )
      )
    )
  })
  
  output$targets <- renderTable({
    req(targetsDF())
    return(targetsDF())
  })
  
  #for KPI tab:
  #creating a data-frame to display durations of prescriptions 
  durationsDF <- reactive({
    req(totalNumberAsChosen())
    req(input$wardsK)
    req(input$priorityK)
    req(input$dateRangeK)
    req(averageThroughDur())
    req(averageWaiting())
    
    tValue <- qt(0.975,df = (totalNumberAsChosen() - 1))
    if (input$wardsK == "all"){
      chosenWards <- "all wards"
    }else{
      chosenWards <- input$wardsK
    }
    if (input$priorityK == "all"){
      chosenPriority <- " (all priorities)"
    }else{
      chosenPriority <- paste0(" (",input$priorityK,")")
    }
    return(
      data.frame(
        duration = c(
          paste0(
            "average total throughput time for ",
            chosenWards, 
            chosenPriority,
            " between ",
            input$dateRangeK[1] %>% as.character(),
            " and ",
            input$dateRangeK[2] %>% as.character(), 
            " (durations in min.)"
          ),
          paste0(
            "average waiting time in whole process for ",
            chosenWards, 
            chosenPriority,
            " between ",
            input$dateRangeK[1] %>% as.character(),
            " and ",
            input$dateRangeK[2] %>% as.character(),
            " (durations in min.)"
          ), 
          paste0(
            "average dispensary throughput time for ",
            chosenWards, 
            chosenPriority,
            " between ",
            input$dateRangeK[1] %>% as.character(),
            " and ",
            input$dateRangeK[2] %>% as.character(), 
            " (durations in min.)"
          ), 
          paste0(
            "average waiting time in dispensary for ",
            chosenWards, 
            chosenPriority,
            " between ",
            input$dateRangeK[1] %>% as.character(),
            " and ",
            input$dateRangeK[2] %>% as.character(), 
            " (durations in min.)"
          )
        ),
        time = c(
          averageThroughDur()[1] %>% "/" (60) %>% round(digits = 2),
          averageWaiting()[1] %>% "/" (60) %>% round(digits = 2),
          averageThroughDur()[3] %>% "/" (60) %>% round(digits = 2),
          averageWaiting()[3] %>% "/" (60) %>% round(digits = 2)
        ),
        confidence_interval = c(
          averageThroughDur()[2] %>% "*" (tValue/sqrt(totalNumberAsChosen())) %>% "/" (60) %>% round(digits = 2),
          averageWaiting()[2] %>% "*" (tValue/sqrt(totalNumberAsChosen())) %>% "/" (60) %>% round(digits = 2),
          averageThroughDur()[4] %>% "*" (tValue/sqrt(totalNumberAsChosen())) %>% "/" (60) %>% round(digits = 2),
          averageWaiting()[4] %>% "*" (tValue/sqrt(totalNumberAsChosen())) %>% "/" (60) %>% round(digits = 2)
        ),
        percentage = c(
          100 %>% format(nsmall = 1),
          (averageWaiting()[1]/averageThroughDur()[1]) %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1),
          100 %>% format(nsmall = 1),
          (averageWaiting()[3]/averageThroughDur()[3]) %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1)
        )
      )
    )
  })
  
  output$durationsKPI <- renderTable({
    req(durationsDF())
    return(durationsDF())
  })
  
  dataQualityDF <- reactive({
    req(currentRawData())
    req(absTimes.df())
    req(earliestTime())
    req(latestTime())
    if (!(is.null(currentRawData()))){
      if (input$suspendedK){
        durDF <- durationsS.df()
      }else{
        durDF <- durations.df()
      }
      totalNumberOfTasks <- sum(
        absTimes.df() %>% nrow(),
        currentRawData()[[5]] %>% length(),
        currentRawData()[[4]] %>% length()
      )
      numberOfProcessedRxs <- absTimes.df() %>% nrow()
      if (numberOfProcessedRxs >0){
        return(
          data.frame(
            data_quality_metric = c(
              paste0(
                "tasks registered on PTS** between ",
                earliestTime() %>% as.Date() %>% as.character(),
                " and ",
                latestTime() %>% as.Date() %>% as.character()
              ), 
              "prescriptions eliminated due to missing data",
              "prescriptions arbitrarily eliminated (arriving before 8:00 a.m. or after 6:30 p.m.)",
              "prescriptions remaining for processing by this web-application after eliminations as above",
              "verifying activities below 15 seconds*** (of prescriptions processed by app)",
              "dispensing activities below 15 seconds (of prescriptions processed by app)",
              "final checking activities below 15 seconds (of prescriptions processed by app)",
              "prescriptions where all activities take 15 seconds or above (of prescriptions processed by app)"
            ), 
            number = c(
              totalNumberOfTasks,
              currentRawData()[[5]] %>% length(),
              currentRawData()[[4]] %>% length(),
              absTimes.df() %>% nrow(),
              durDF %>% filter(verifActivity < 15) %>% nrow(),
              durDF %>% filter(dispActivity < 15) %>% nrow(), 
              durDF %>% filter(finCheckActivity < 15) %>% nrow(), 
              durDF %>% filter((verifActivity >= 15)&(dispActivity >= 15)&(finCheckActivity >= 15)) %>%
                nrow()
            ), 
            percentage = c(
              100 %>% format(nsmall = 1),
              currentRawData()[[5]] %>% length() %>% "/" (totalNumberOfTasks) %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1),
              currentRawData()[[4]] %>% length() %>% "/" (totalNumberOfTasks) %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1),
              absTimes.df() %>% nrow() %>% "/" (totalNumberOfTasks) %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1),
              durDF %>% filter(verifActivity < 15) %>% nrow() %>% "/" (numberOfProcessedRxs) %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1),
              durDF %>% filter(dispActivity < 15) %>% nrow() %>% "/" (numberOfProcessedRxs) %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1), 
              durDF %>% filter(finCheckActivity < 15) %>% nrow() %>% "/" (numberOfProcessedRxs) %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1), 
              durDF %>% filter((verifActivity >= 15)&(dispActivity >= 15)&(finCheckActivity >= 15)) %>%
                nrow() %>% "/" (numberOfProcessedRxs) %>% "*" (100) %>% round(digits = 1) %>% format(nsmall = 1)
            )
          )
        )
      }
    } 
  })
  
  output$dataQuality <- renderTable({
    req(dataQualityDF())
    if (!(is.null(dataQualityDF()))){
      return(dataQualityDF())
    }
  })
  
  #Storing of Excel workbook in a reactive that can be downloaded with the downloadHandler
  #below. The reason I put it in a reactive is to see whether this has an impact
  #on the functioning of the downloadbutton (at work it doesn't work). 
  workBookExcel <- reactive({
    req(prescrWithinCutoffPlotImage())
    req(prescrWithinDispCutoffImage())
    req(overallWaitingImage())
    req(dispensaryWaitingImage())
    req(processMetricsDF())
    req(targetsDF())
    req(durationsDF())
    req(dataQualityDF())
    
    if (input$suspendedK){
      titleMessage <- "Suspended periods were considered in the calculations, i.e. subtracted from queue and activity durations, respectively."
    }else{
      titleMessage <- "Suspended periods were ignored in the calculations, i.e. added to queue and activity durations, respectively."
    }
    
    png(filename = "data/plot1.png")
    plot(prescrWithinCutoffPlotImage())
    dev.off()
    png(filename = "data/plot2.png")
    plot(prescrWithinDispCutoffImage())
    dev.off()
    png(filename = "data/plot3.png")
    plot(overallWaitingImage())
    dev.off()
    png(filename = "data/plot4.png")
    plot(dispensaryWaitingImage())
    dev.off()
    
    #creating of Excelworkbook to store data and graphs in:
    ExcelWorkBook <- xlsx::createWorkbook(type = "xls") #alternatively, I could use type = "xlsx"
    #the reason I use "xls", is because at work they still have an older version of Excel installed
    within_cutoff_time_Sheet <- xlsx::createSheet(ExcelWorkBook, sheetName = "within cutoff time")
    xlsx::addPicture("data/plot1.png", within_cutoff_time_Sheet)
    within_disp_cutoff_Sheet <- xlsx::createSheet(ExcelWorkBook, sheetName = "within dispensary cutoff")
    xlsx::addPicture("data/plot2.png", within_disp_cutoff_Sheet)
    overall_waiting_Sheet <- xlsx::createSheet(ExcelWorkBook, sheetName = "overall waiting")
    xlsx::addPicture("data/plot3.png", overall_waiting_Sheet)
    dispensary_waiting_Sheet <- xlsx::createSheet(ExcelWorkBook, sheetName = "waiting in dispensary")
    xlsx::addPicture("data/plot4.png", dispensary_waiting_Sheet)
    
    process_metrics_Sheet <- xlsx::createSheet(ExcelWorkBook, sheetName = "process metrics")
    process_metrics_title_row <- xlsx::createRow(process_metrics_Sheet, rowIndex = 1)
    process_metrics_title_cell <- xlsx::createCell(process_metrics_title_row, colIndex = 1)
    xlsx::setCellValue(process_metrics_title_cell[[1,1]], titleMessage)
    xlsx::addDataFrame(processMetricsDF(),process_metrics_Sheet, col.names=TRUE, row.names=F, startRow = 3,startColumn = 1)
    
    targets_Sheet <- xlsx::createSheet(ExcelWorkBook, sheetName = "targets")
    targets_title_row <- xlsx::createRow(targets_Sheet, rowIndex = 1)
    targets_title_cell <- xlsx::createCell(targets_title_row, colIndex = 1)
    xlsx::setCellValue(targets_title_cell[[1,1]], titleMessage)
    xlsx::addDataFrame(targetsDF(),targets_Sheet, col.names=TRUE, row.names=F, startRow = 3,startColumn = 1)
    
    durations_of_activities_Sheet <- xlsx::createSheet(ExcelWorkBook, sheetName = "durations of activities")
    durations_of_activities_row <- xlsx::createRow(durations_of_activities_Sheet, rowIndex = 1)
    durations_of_activities_cell <- xlsx::createCell(durations_of_activities_row, colIndex = 1)
    xlsx::setCellValue(durations_of_activities_cell[[1,1]], titleMessage)
    xlsx::addDataFrame(durationsDF(),durations_of_activities_Sheet, col.names=TRUE, row.names=F, startRow = 3,startColumn = 1)
    
    data_quality_Sheet <- xlsx::createSheet(ExcelWorkBook, sheetName = "data quality")
    data_quality_row <- xlsx::createRow(data_quality_Sheet, rowIndex = 1)
    data_quality_cell <- xlsx::createCell(data_quality_row, colIndex = 1)
    xlsx::setCellValue(data_quality_cell[[1,1]], titleMessage)
    xlsx::addDataFrame(dataQualityDF(),data_quality_Sheet, col.names=TRUE, row.names=F, startRow = 3,startColumn = 1)
    
    return(
      ExcelWorkBook
    )
  })
  
  output$exportToExcel <- downloadHandler(
    filename = function(){
      paste0(
        "FVdispensary", 
        Sys.Date(), 
        ".xls"
      )
    }, 
    content = function(fileName){
      req(workBookExcel())
      xlsx::saveWorkbook(workBookExcel(), file = fileName)
    }
  )
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #for artific. variation tab:
  
  output$dateAOutput <- renderUI({
    req(earliestTime())
    req(latestTime())
    return(
      dateInput(inputId = "dateA", label = "starting date of period", 
                value = as.Date(earliestTime()),
                min = as.Date(earliestTime()), 
                max = (as.Date(latestTime())-7), daysofweekdisabled = c(0,6))
    )
  })
  
  
  dateChosen <- reactive({
    req(input$dateA)
    return(as.POSIXct(input$dateA, tz = "UTC"))
  })
  
  #for artific. variation tab:
  #Calculation of number of weeks that can be chosen in selectInput widget
  #below and making it available for other output logics:
  numberOfWeeks <- reactive({
    req(latestTime())
    req(dateChosen())
    if (length(dateChosen()) == 0){
      0
    }else{
      (((latestTime() %>% as.Date()) - (dateChosen() %>% as.Date()))/7) %>% 
        trunc()
    }
  })
  
  #for artific. variation tab:
  #this is to create a numeric input widget, where the max. 
  #number is determined by the date previously chosen
  output$weeksInput <- renderUI({
    req(numberOfWeeks())
    if (numberOfWeeks() == 0){
      return(
            h6("Please enter a date above.")
          )
    }else{
      selectInput(inputId = "numberOfWeeks", label = "number of weeks after starting date", 
                  selected = 1,
                  1:numberOfWeeks())
    }
  })
  
  endDate <- reactive({
    req(input$numberOfWeeks)
    req(dateChosen())
    if (length(input$numberOfWeeks) == 0){
      return(
        (dateChosen() + 604800) #604800 seconds in a week
      )
    }else{
      return(
        (dateChosen() + (604800 * as.numeric(input$numberOfWeeks))) #604800 seconds in a week
      )
    }
  })
  
  #for artific. variation tab:
  #updating of vector of prescription numbers depending on data-input at interface:
  absTimesUpdateVectForA <- reactive({
    req(absTimes.df())
    req(endDate())
    #defining workweek as only these days will be looked at in this tab:
    # cat(file=stderr(),endDate %>% as.Date() %>% as.character(),"\n")
    monToFri <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    if (input$wardsA == "all"){
        absTimes.df() %>% filter((startOfVerifQueu >= dateChosen())&(startOfVerifQueu < endDate())) %>%
          filter(dayOfWeek %in% monToFri) %>% pull(RxID)
    }else if(input$wardsA == "medical and surgical wards"){
      absTimes.df() %>% filter((startOfVerifQueu >= dateChosen())&(startOfVerifQueu < endDate())) %>%
        filter(dayOfWeek %in% monToFri) %>% filter(unit %in% medAndSurgWards()) %>% pull(RxID)
    }else if(input$wardsA == "acute wards"){
      absTimes.df() %>% filter((startOfVerifQueu >= dateChosen())&(startOfVerifQueu < endDate())) %>%
        filter(dayOfWeek %in% monToFri) %>% filter(unit %in% acuteWards()) %>% pull(RxID)
    }else if(input$wardsA == "mental health wards"){
      absTimes.df() %>% filter((startOfVerifQueu >= dateChosen())&(startOfVerifQueu < endDate())) %>%
        filter(dayOfWeek %in% monToFri) %>% filter(unit %in% mentalHealthWards()) %>% pull(RxID)
    }else if(input$wardsA == "clinics"){
      absTimes.df() %>% filter((startOfVerifQueu >= dateChosen())&(startOfVerifQueu < endDate())) %>%
        filter(dayOfWeek %in% monToFri) %>% filter(unit %in% clinics()) %>% pull(RxID)
    }else if(input$wardsA == "Women and Children Unit"){
      absTimes.df() %>% filter((startOfVerifQueu >= dateChosen())&(startOfVerifQueu < endDate())) %>%
        filter(dayOfWeek %in% monToFri) %>% filter(unit %in% childrenWards()) %>% pull(RxID)
    }else if(input$wardsA == "off-site wards"){
      absTimes.df() %>% filter((startOfVerifQueu >= dateChosen())&(startOfVerifQueu < endDate())) %>%
        filter(dayOfWeek %in% monToFri) %>% filter(unit %in% outsideFVRHWards()) %>% pull(RxID)
    }else if(input$wardsA == "FVRH D/L"){
      absTimes.df() %>% filter((startOfVerifQueu >= dateChosen())&(startOfVerifQueu < endDate())) %>%
        filter(dayOfWeek %in% monToFri) %>% filter(unit == "FVRH D/L") %>% pull(RxID)
    }else{
      absTimes.df() %>% filter((startOfVerifQueu >= dateChosen())&(startOfVerifQueu < endDate())) %>%
          filter(dayOfWeek %in% monToFri) %>% filter(unit == input$wardsA) %>% 
          pull(RxID)
    }
  })
  
  #for artific. variation tab:
  #defining data-frame that the plot is going to get drawn from 
  #further below:
  dataFrameForPlot <- reactive({
    req(durations.df())
    req(absTimesUpdateVectForA())
    columnName <- switch(
      input$stepA,
      verifying = "arrAtVerifQueue",
      dispensing = "arrAtDispQueue", 
      "final checking" = "arrAtFinCheckQueue"
    )
    return(
      durations.df() %>% filter(RxID %in% absTimesUpdateVectForA()) %>% 
        subset(select = c("RxID", columnName)) %>%
        rename(arrivals = columnName) #I rename the column name as
      #ggplot does not seem to be able to identify a column reading
      #its name from a variable
    )
  })
  
  #for artific. variation tab:
  output$arrivalPlot <- renderPlot({
    #cat(file=stderr(),dataFrameForPlot() %>% colnames(),"\n")
    req(dataFrameForPlot())
    return(
      ggplot(data = dataFrameForPlot()) + geom_histogram(aes(x = arrivals),
                                                         binwidth = 0.5,color = "green4",
                                                         fill = "green", alpha = 0.5) +
        xlab("time of day") + ylab("number of prescriptions") +
        ggtitle(
          paste0("Arrival of prescriptions at ",
                 input$stepA,
                 " queue")
        )
    )
  })
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
  output$dateQOutput <- renderUI({
    req(earliestTime())
    req(latestTime())
    return(
      dateInput(inputId = "dateQ", label = "date", 
                value = as.Date(earliestTime()),
                min = as.Date(earliestTime()), 
                max = (as.Date(latestTime())-7), daysofweekdisabled = c(0,6))
    )
  })
  
  #for queue tab:
  dateChosenQ <- reactive({
    req(input$dateQ)
    return(as.POSIXct(input$dateQ, tz = "UTC"))
  })
  
  #for queue tab:
  #Calculation of number of weeks that can be chosen in selectInput widget
  #below and making it available for other output logics:
  numberOfWeeksQ <- reactive({
    req(dateChosenQ())
    req(latestTime())
    return(
      (((latestTime() %>% as.Date()) - (dateChosenQ() %>% as.Date()))/7) %>% 
        trunc()
    )
  })
  
  #for queue tab:
  #this is to create a numeric input widget, where the max. 
  #number is determined by the date previously chosen
  output$weeksInputQ <- renderUI({
    req(numberOfWeeksQ())
    return(
      selectInput(inputId = "numberOfWeeksQ", label = "number of weeks", 
                  selected = 1,
                  1:numberOfWeeksQ())
    )
  })
  
  #for queue tab:
  #choosing data-interval of at least a week or more, as per user choice:
  endDateQ <- reactive({
    req(input$numberOfWeeksQ)
    req(dateChosenQ())
    if (length(input$numberOfWeeksQ) == 0){
      return(
        (dateChosenQ() + 604800) #604800 seconds in a week
      )
    }else{
      return(
        (dateChosenQ() + (604800 * as.numeric(input$numberOfWeeksQ))) #604800 seconds in a week
      )
    }
  })
  

  #for queue tab:
  #updating of vector of prescription numbers depending on data-input at interface:
  absTimesUpdateVectForQ <- reactive({
    req(absTimes.df())
    req(uniqueWards.vect())
    req(input$wardsQ)
    req(endDateQ())
    #defining workweek as only these days will be looked at in this tab:
    # cat(file=stderr(),endDate %>% as.Date() %>% as.character(),"\n")
    monToFri <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") #ensuring that data is only 
    #for weekdays
    if (input$wardsQ == "all"){
      return(
        absTimes.df() %>% filter((startOfVerifQueu >= dateChosenQ())&(startOfVerifQueu < endDateQ())) %>%
          filter(dayOfWeek %in% monToFri) %>% pull(RxID)
      )
    }else if (input$wardsQ == "inpatient"){
      return(
        absTimes.df() %>% filter((startOfVerifQueu >= dateChosenQ())&(startOfVerifQueu < endDateQ())) %>%
          filter(dayOfWeek %in% monToFri) %>% filter(unit %in% (uniqueWards.vect()[uniqueWards.vect() != "OUT PATIENTS"])) %>% 
          pull(RxID)
      )
    }else{
      return(
        absTimes.df() %>% filter((startOfVerifQueu >= dateChosenQ())&(startOfVerifQueu < endDateQ())) %>%
          filter(dayOfWeek %in% monToFri) %>% filter(unit == input$wardsQ) %>% 
          pull(RxID)
      )
    }
  })

  #for queue tab:
  #defining data-frame that the plot is going to get drawn from 
  #further below:
  dataFrameForPlotQ <- reactive({
    req(durationsS.df())
    req(durations.df())
    req(absTimesUpdateVectForQ())
    arrivalTime <- switch(
      input$stepQ,
      verifying = "arrAtVerifQueue",
      dispensing = "arrAtDispQueue", 
      "final checking" = "arrAtFinCheckQueue"
    )
    queueName <- switch(
      input$stepQ,
      verifying = "verifQueue",
      dispensing = "dispQueue", 
      "final checking" = "finCheckQueue"
    )
    if (input$suspendedQ){
      return(
        durationsS.df() %>% filter(RxID %in% absTimesUpdateVectForQ()) %>% 
          subset(select = c("RxID", queueName, arrivalTime)) %>%
          rename(arrivals = arrivalTime) %>% 
          rename(queue = queueName)
      )
    }else{
      return(
        durations.df() %>% filter(RxID %in% absTimesUpdateVectForQ()) %>% 
          subset(select = c("RxID", queueName, arrivalTime)) %>%
          rename(arrivals = arrivalTime) %>% 
          rename(queue = queueName)
      )
    }
  })
  
  #for queue tab:
  simpleDataframe <- reactive({
    req(dataFrameForPlotQ())
    return(
      dataFrameForPlotQ() %>%
        queueBinner()
    )
  })
  
  #for queue tab:
  output$queuePlotM <- renderPlot({
    req(simpleDataframe())
    return(
      ggplot(data = simpleDataframe(), 
             aes(x = arrival, y = median, ymin = lower, ymax = upper)) + 
        geom_crossbar(stat = "identity", show.legend = T,
                      color = "green4", alpha = 0.5) +
        xlab("time of day") + ylab("waiting time [minutes]") +
        ggtitle(
          paste0("Median waiting times at ",
                 input$stepQ,
                 " queue (25th and 75th percentile)")
        )
    )
  })
  
  #for queue tab:
  output$queuePlotA <- renderPlot({
    req(simpleDataframe())
    return(
      ggplot(data = simpleDataframe(), 
             aes(x = arrival, y = median, ymin = lower, ymax = upper)) + 
        geom_bar(stat = "identity", 
                 show.legend = T,
                 fill = "green",
                 color = "green4", 
                 alpha = 0.5) +
        xlab("time of day") + ylab("waiting time [minutes]") +
        ggtitle(
          paste0("Average waiting times at ",
                 input$stepQ,
                 " queue")
        )
    )
  })
  
  ######################################################################################################
  
  ####  #  #   #
  #  #  #  ## ##
  #     #  # # #
   ##   #  #   #
     #  #  #   #
  #  #  #  #   #
  ####  #  #   #
  
  #######################################################################################################
  #for simulation tab:
  #due to limited space, not the whole matrix is shown, only the part for a role as selected
  #on the left side of the user interface as per input$role; different matrixInput widgets
  #with different names can be created and their input processed in the code further below
  output$shiftInput <- renderUI({
    req(input$role)
    relevantMatrix <- defaultShiftMatrix[input$role,] %>% matrix(nrow = 1)
    colnames(relevantMatrix) <- c("8-9 a.m.", "9-11 a.m.",
                                      "11 a.m.-1 p.m.", "1-3 p.m.",
                                      "3-5 p.m.", "5-6:30 p.m.")
    rownames(relevantMatrix) <- c(input$role)
    return(
      matrixInput(inputId = paste0(
        "shift",
        switch(input$role,"disp. pharmacist" = "DispPharm", 
               "ward pharmacist" = "WardPharm",
               "pharm. tech." = "PharmTech",
               "checking tech." = "CheckTech")
        ), 
        value = relevantMatrix,
        rows = list(
                    names = T, 
                    editableNames = F
                  ),
        cols = list(
                    names = T, 
                    editableNames = F
                  ), 
        class = "numeric"
      )
    )
  })
  
  #for simulation tab:
  #this reactive returns a matrix of the shift patterns of
  #the four relevant roles in the simulation model, as per user input:
  #(it's a little silly that the matrix has to be put back again after taking
  #it apart above, I'm doing all this to save space on the interface, however)
  shiftPatterns <- reactive({
    if ((anyNA(input$shiftDispPharm))|(is.null(input$shiftDispPharm))){
      dispPharmHelper <- defaultShiftMatrix[1,] %>% as.vector()
    }else{
      dispPharmHelper <- input$shiftDispPharm[1,] %>% as.vector()
    }
    if ((anyNA(input$shiftWardPharm))|(is.null(input$shiftWardPharm))){
      wardPharmHelper <- defaultShiftMatrix[2,] %>% as.vector()
    }else{
      wardPharmHelper <- input$shiftWardPharm[1,] %>% as.vector()
    }
    if ((anyNA(input$shiftPharmTech))|(is.null(input$shiftPharmTech))){
      pharmTechHelper <- defaultShiftMatrix[3,] %>% as.vector()
    }else{
      pharmTechHelper <- input$shiftPharmTech[1,] %>% as.vector()
    }
    if ((anyNA(input$shiftCheckTech))|(is.null(input$shiftCheckTech))){
      checkTechHelper <- defaultShiftMatrix[4,] %>% as.vector()
    }else{
      checkTechHelper <- input$shiftCheckTech[1,] %>% as.vector()
    }
    helperMatrix <- dispPharmHelper %>%
      append(wardPharmHelper) %>%
      append(pharmTechHelper) %>%
      append(checkTechHelper) %>%
      matrix(nrow = 4, byrow = T)
    colnames(helperMatrix) <- c("8-9 a.m.", "9-11 a.m.",
                                        "11 a.m.-1 p.m.", "1-3 p.m.",
                                        "3-5 p.m.", "5-6:30 p.m.")
    rownames(helperMatrix) <- c(
        "disp. pharmacist", 
        "ward pharmacist", 
        "pharm. tech.", 
        "checking tech."
      )
    # write.xlsx(x = helperMatrix, file = "data/shiftPatterns.xlsx",
    #            sheetName = "shifts",append = F, row.names = F)
    return(helperMatrix)
  })
  
  output$arrivalDateSimOutput <- renderUI({
    req(earliestTime())
    req(latestTime())
    return(
      dateInput(inputId = "arrivalDateSim", label = "enter date", 
                value = as.Date(earliestTime()),
                min = as.Date(earliestTime()), 
                max = (as.Date(latestTime())-7), daysofweekdisabled = c(0,6))
    )
  })
  
  #for simulation tab:
  #creation of list of two vectors; one vector is to contain the arrival times of 
  #prescriptions on a chosen work-day from the hatch, the other from the wards:
  arrivalTimes <- reactive({
    req(input$arrivalDateSim)
    req(absTimes.df())
    req(uniqueWards.vect())
    chosenDate <- input$arrivalDateSim %>% as.Date() %>% as.POSIXct(tz = "UTC")
    chosenDateAsNumeric <- chosenDate %>% "+" (28800) %>% as.numeric() #28800 sec. are 8 hours - 
    #the simulator clock starts at 8:00 a.m.
    return(
      list(
        arrivalsAtHatchV <- absTimes.df() %>% filter(unit == "OUT PATIENTS") %>% 
          filter((startOfVerifQueu > chosenDate)&(startOfVerifQueu <= (chosenDate + 66600))) %>% #66600 sec. are 18.5 hours
          pull(startOfVerifQueu) %>% 
          as.integer() %>% 
          "-" (chosenDateAsNumeric) %>% #chosenDateAsNumeric is the time at the chosen date at 0:00 hours (as numeric)
          "/" (60), #transforming into minutes (that the simulator requires)
        arrivalsFromWardV <- absTimes.df() %>% 
          filter(unit %in% (uniqueWards.vect()[uniqueWards.vect() != "OUT PATIENTS"])) %>% 
          filter((startOfVerifQueu > chosenDate)&(startOfVerifQueu <= (chosenDate + 66600))) %>% #66600 sec. are 18.5 hours
          pull(startOfVerifQueu) %>% 
          as.integer() %>% 
          "-" (chosenDateAsNumeric) %>% #chosenDateAsNumeric is the time at the chosen date at 0:00 hours (as numeric)
          "/" (60) #transforming into minutes (that the simulator requires)
      )
    )
  })
  
  #for simulation tab:
  #the chosen date for use later:
  chosenDateSim <- reactive({
    req(input$arrivalDateSim)
    return(
      input$arrivalDateSim %>% as.Date() %>% as.POSIXct(tz = "UTC")
    )
  })
  
  #for simulation tab:
  #extracting data-frames containing activity durations for 
  #outpatient and inpatient scripts on the chosen day:
  patientDurations <- reactive({
    req(absTimes.df())
    req(chosenDateSim())
    req(uniqueWards.vect())
    req(durations.df())
    req(durationsS.df())
    
    arrivalsOnChosenDate <- absTimes.df() %>% 
      filter((startOfVerifQueu > chosenDateSim())&(startOfVerifQueu <= (chosenDateSim() + 66600))) #66600 sec. are 18.5 hours
    #extracting a data-frame from the tracker data for the chosen date for use by 
    #the simulator
    outPatRxIDsForChosenDate <- arrivalsOnChosenDate %>%
      filter(unit == "OUT PATIENTS") %>% 
      pull(RxID)
    inPatRxIDsForChosenDate <- arrivalsOnChosenDate %>%
      filter(unit %in% (uniqueWards.vect()[uniqueWards.vect() != "OUT PATIENTS"])) %>%
      pull(RxID)
    allPatRxIDsForChosenDate <- arrivalsOnChosenDate %>%
      pull(RxID)
    if (input$suspendedS == T){ #tick-box whether suspended periods are to be considered or not
      outPatDurForChosenDate <- durationsS.df() %>%         
        filter(RxID %in% outPatRxIDsForChosenDate) %>%   
        dplyr::select(RxID, verifActivity, dispActivity,   
                      finCheckActivity) %>%
        mutate(verifActivity = verifActivity/60, dispActivity = dispActivity/60, 
               finCheckActivity = finCheckActivity/60)  
      inPatDurForChosenDate <- durationsS.df() %>% 
        filter(RxID %in% inPatRxIDsForChosenDate) %>%
        dplyr::select(RxID, verifActivity, dispActivity, 
                      finCheckActivity) %>%
        mutate(verifActivity = verifActivity/60, dispActivity = dispActivity/60, 
               finCheckActivity = finCheckActivity/60)
      #please note that allPatDurForChosenDate contains all columns of the raw-data
      #not just a few as outPatDurForChosenDate or inPatDurForChosenDate
      allPatDurForChosenDate <- durationsS.df() %>% 
        filter(RxID %in% allPatRxIDsForChosenDate) %>%
        mutate(verifQueue = verifQueue/60, verifActivity = verifActivity/60, 
               dispQueue = dispQueue/60, dispActivity = dispActivity/60, 
               finCheckQueue = finCheckQueue/60, finCheckActivity = finCheckActivity/60, 
               totThroughput = totThroughput/60) %>%
        mutate(dispThroughput = verifQueue + verifActivity + dispQueue + dispActivity)
    }else{
      outPatDurForChosenDate <- durations.df() %>%         
        filter(RxID %in% outPatRxIDsForChosenDate) %>%   
        dplyr::select(RxID, verifActivity, dispActivity,   
                      finCheckActivity) %>%
        mutate(verifActivity = verifActivity/60, dispActivity = dispActivity/60, 
               finCheckActivity = finCheckActivity/60)  
      inPatDurForChosenDate <- durations.df() %>% 
        filter(RxID %in% inPatRxIDsForChosenDate) %>%
        dplyr::select(RxID, verifActivity, dispActivity, 
                      finCheckActivity) %>%
        mutate(verifActivity = verifActivity/60, dispActivity = dispActivity/60, 
               finCheckActivity = finCheckActivity/60)
      allPatDurForChosenDate <- durations.df() %>% 
        filter(RxID %in% allPatRxIDsForChosenDate) %>%
        mutate(verifQueue = verifQueue/60, verifActivity = verifActivity/60, 
               dispQueue = dispQueue/60, dispActivity = dispActivity/60, 
               finCheckQueue = finCheckQueue/60, finCheckActivity = finCheckActivity/60, 
               totThroughput = totThroughput/60) %>%
        mutate(dispThroughput = verifQueue + verifActivity + dispQueue + dispActivity)
    }
    return(
      list(
        outPatDurForChosenDate, #durations of outpatient Rxs used for simulator
        inPatDurForChosenDate,  #durations of inpatient Rxs used for simulator
        arrivalsOnChosenDate,   #absolute times for chosen date - for comparison only
        allPatDurForChosenDate  #durations of all Rxs for chosen date - for comparison only
      )
    )
  })
  
  #for simulation tab:
  #creating a vector to complare simulated results with in 
  #output$resultsTableS below
  realDataForComparison <- reactive({
    req(patientDurations())
    req(chosenDateSim())
    numberRxsEntering <- patientDurations()[[3]] %>% nrow()
    RxNumbersOfCompleted <- patientDurations()[[3]] %>% 
      filter(endOfFinCheckAct <= (chosenDateSim() + 66600)) %>%
      pull(RxID)
    vQueue <- patientDurations()[[4]] %>% 
      filter(RxID %in% RxNumbersOfCompleted) %>% #this is important to ensure I only check the average of those Rxs that got completed (to be better comparable with the simulator)
      pull(verifQueue) %>% mean() 
    dQueue <- patientDurations()[[4]] %>% 
      filter(RxID %in% RxNumbersOfCompleted) %>%
      pull(dispQueue) %>% mean() 
    fQueue <- patientDurations()[[4]] %>% 
      filter(RxID %in% RxNumbersOfCompleted) %>%
      pull(finCheckQueue) %>% mean()
    totThroughputDur <- patientDurations()[[4]] %>% 
      filter(RxID %in% RxNumbersOfCompleted) %>%
      pull(totThroughput) %>% mean()
    dispThroughputDur <- patientDurations()[[4]] %>% 
      filter(RxID %in% RxNumbersOfCompleted) %>%
      pull(dispThroughput) %>% mean()
    waitInProcess <- ((vQueue + dQueue + fQueue)/totThroughputDur) %>% 
      round(digits = 1)
    waitInDisp <- ((vQueue + dQueue)/dispThroughputDur) %>% 
      round(digits = 1)
    proportComplet2hTot <- patientDurations()[[4]] %>% 
      filter(RxID %in% RxNumbersOfCompleted) %>%
      filter(totThroughput <= 120) %>% nrow() %>% 
      "/" (numberRxsEntering) 
    proportComplet2hDisp <- patientDurations()[[4]] %>% 
      filter(RxID %in% RxNumbersOfCompleted) %>%
      filter(dispThroughput <= 120) %>% nrow() %>% 
      "/" (numberRxsEntering)
    
    return(
      c(
        numberRxsEntering %>% as.character(), 
        patientDurations()[[3]] %>% filter(endOfFinCheckAct <= (chosenDateSim() + 66600)) %>%
          nrow() %>% as.character(),
        patientDurations()[[3]] %>% filter(unit == "OUT PATIENTS") %>%
          nrow() %>% as.character(), 
        patientDurations()[[3]] %>% filter(unit == "OUT PATIENTS") %>%
          filter(endOfFinCheckAct <= (chosenDateSim() + 66600)) %>%
          nrow() %>% as.character(), 
        patientDurations()[[3]] %>% filter(unit != "OUT PATIENTS") %>%
          nrow() %>% as.character(), 
        patientDurations()[[3]] %>% filter(unit != "OUT PATIENTS") %>%
          filter(endOfFinCheckAct <= (chosenDateSim() + 66600)) %>%
          nrow() %>% as.character(),
        paste0(
          vQueue %>%
            round(digits = 1) %>% as.character(), 
          " min."
        ), 
        paste0(
          patientDurations()[[4]] %>% 
            filter(RxID %in% RxNumbersOfCompleted) %>%
            pull(verifActivity) %>% mean() %>%
            round(digits = 1) %>% as.character(), 
          " min."
        ), 
        paste0(
          dQueue %>%
            round(digits = 1) %>% as.character(), 
          " min."
        ), 
        paste0(
          patientDurations()[[4]] %>% 
            filter(RxID %in% RxNumbersOfCompleted) %>%
            pull(dispActivity) %>% mean() %>%
            round(digits = 1) %>% as.character(), 
          " min."
        ), 
        paste0(
          fQueue %>%
            round(digits = 1) %>% as.character(), 
          " min."
        ), 
        paste0(
          patientDurations()[[4]] %>% 
            filter(RxID %in% RxNumbersOfCompleted) %>%
            pull(finCheckActivity) %>% mean() %>%
            round(digits = 1) %>% as.character(), 
          " min."
        ), 
        paste0(
          totThroughputDur %>%
            round(digits = 1) %>% as.character(), 
          " min."
        ), 
        paste0(
          dispThroughputDur %>%
            round(digits = 1) %>% as.character(), 
          " min."
        ), 
        paste0(
          waitInProcess %>% "*" (100) %>% round(digits = 1) %>%
            as.character(), 
          "%"
        ), 
        paste0(
          waitInDisp %>% "*" (100) %>% round(digits = 1) %>%
            as.character(), 
          "%"
        ), 
        paste0(
          proportComplet2hTot %>% "*" (100) %>%
            round(digits = 1) %>% as.character(), 
          "%"
        ), 
        paste0(
          proportComplet2hDisp %>% "*" (100) %>%
            round(digits = 1) %>% as.character(), 
          "%"
        )
      )
    )
  })
  
  output$startSimulationButton <- renderUI({
    req(shiftPatterns())
    req(input$repeatS)
    if (!((is.null(shiftPatterns()))|(is.na(input$repeatS)))){
      actionButton(inputId = "startSim", "start simulation")
    }
  })
  
  #for simulation tab:
  #obtaining of data provided by the simulator for use below:
  simulatedData <- eventReactive(input$startSim, {
    req(arrivalTimes())
    req(patientDurations())
    showNotification("Please note that many repetitions can take a long time to compute. (50 repetitions probably take more than a minute).", 
                     duration = 30, closeButton = T)
    return(
        simulationResults(shiftPatterns(), 
                          arrivalTimes()[[1]],  #arrival times at hatch
                          arrivalTimes()[[2]],  #arrival times from wards
                          patientDurations()[[1]], #durations for outpatient related activities for chosen day
                          patientDurations()[[2]], #durations for inpatient related activities for chosen day
                          input$arrivalS,  #if T, all arrivals are simulated to come evenly distributed during the day
                          input$repeatS %>% as.numeric())  #number of times simulation run is to be repeated
      )
  })
  
  #for simulation tab:
  #this reactive creates a data-frame to be used for the bar plot further below;
  #data is created by the simulation model:
  simulatedDataForPlot <- reactive({
    req(simulatedData())
    showNotification("Simulation data computed.", 
                     duration = 15, closeButton = T)
    categoriesForPlot <- c("verifying queue", "verifying activity", 
                           "dispensing queue", "dispensing activity", 
                           "final check. queue", "final check. activity", 
                           "disp. throughput", "total throughput")
    if (is.null(simulatedData())){
      return(NULL)
    }else{
      simulatedDurations <- simulatedData() %>% 
        cbind(dispThroughput = .$dispQueue + .$dispensing + .$finCheckQueue + .$finChecking)
      verifQueueV <- simulatedDurations %>% pull(verifQueue)
      verifyingV <- simulatedDurations %>% pull(verifying)
      dispQueueV <- simulatedDurations %>% pull(dispQueue)
      dispensingV <- simulatedDurations %>% pull(dispensing)
      finCheckQueueV <-  simulatedDurations %>% pull(finCheckQueue)
      finCheckingV <- simulatedDurations %>% pull(finChecking)
      dispThroughputV <- simulatedDurations %>% pull(dispThroughput)
      totThroughputV <- simulatedDurations %>% pull(totThroughput)
      verifQueueMean <- verifQueueV %>% mean()
      verifyingMean <- verifyingV %>% mean()
      dispQueueMean <- dispQueueV %>% mean()
      dispensingMean <- dispensingV %>% mean()
      finCheckQueueMean <-  finCheckQueueV %>% mean()
      finCheckingMean <- finCheckingV %>% mean()
      dispThroughputMean <- dispThroughputV %>% mean()
      totThroughputMean <- totThroughputV %>% mean()
      sampleSize <- verifQueueV %>% length()
      tValue <- qt(0.975,df = (sampleSize - 1)) #with tValue I'm referring to the value
      #akin to the Z value for normal distributions, applying only to t-distributions; the multiple 
      #of the standard deviation to include the quantile at hand (here 0.975)
      verifQueueError <- tValue * sd(verifQueueV)/sqrt(sampleSize)
      verifyingError <- tValue * sd(verifyingV)/sqrt(sampleSize)
      dispQueueError <- tValue * sd(dispQueueV)/sqrt(sampleSize)
      dispensingError <- tValue * sd(dispensingV)/sqrt(sampleSize)
      finCheckQueueError <-  tValue * sd(finCheckQueueV)/sqrt(sampleSize)
      finCheckingError <- tValue * sd(finCheckingV)/sqrt(sampleSize)
      dispThroughputError <- tValue * sd(dispThroughputV)/sqrt(sampleSize)
      totThroughputError <- tValue * sd(totThroughputV)/sqrt(sampleSize)
      
      return(
        data.frame(
          activity = factor(categoriesForPlot, levels = categoriesForPlot, 
                            ordered = T),
          meanDuration = c(
            verifQueueMean,
            verifyingMean,
            dispQueueMean,
            dispensingMean,
            finCheckQueueMean,
            finCheckingMean,
            dispThroughputMean,
            totThroughputMean
          ),
          upperError = c(
            (verifQueueMean + verifQueueError),
            (verifyingMean + verifyingError),
            (dispQueueMean + dispQueueError),
            (dispensingMean + dispensingError),
            (finCheckQueueMean + finCheckQueueError),
            (finCheckingMean + finCheckingError),
            (dispThroughputMean + dispThroughputError),
            (totThroughputMean + totThroughputError)
          ), 
          lowerError = c(
            (verifQueueMean - verifQueueError),
            (verifyingMean - verifyingError),
            (dispQueueMean - dispQueueError),
            (dispensingMean - dispensingError),
            (finCheckQueueMean - finCheckQueueError),
            (finCheckingMean - finCheckingError),
            (dispThroughputMean - dispThroughputError),
            (totThroughputMean - totThroughputError)
          ),
          absError = c(
            verifQueueError,
            verifyingError,
            dispQueueError,
            dispensingError,
            finCheckQueueError,
            finCheckingError,
            dispThroughputError,
            totThroughputError
          )
        )
      )
    }
  })
  
  #for simulation tab:
  #the following bar-chart is to display the durations (and later standard deviations)
  #of the various activities and queues of the process:
  output$simulatedDurations <- renderPlot({
    req(simulatedDataForPlot())
      if (!(is.null(simulatedDataForPlot()))){
        dataForBarChart <- simulatedDataForPlot() %>% 
          filter(activity %in% c("verifying queue", "verifying activity", 
                                 "dispensing queue", "dispensing activity", 
                                 "final check. queue", "final check. activity"))
        return(
          ggplot(data = dataForBarChart, 
                 aes(x = activity, y = meanDuration, ymin = lowerError, ymax = upperError)) +
            geom_bar(width = 0.7, stat = "identity", 
                     fill = c("#F1C40F", "#F1C40F", "#00cc00", "#00cc00", "#D35400", "#D35400"), 
                     alpha = c(0.4,0.8,0.4,0.8,0.4,0.8)) +
            geom_errorbar() +
            theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 12)) +
            ggtitle("simulated results") +
            xlab("activities/queues") +
            ylab("minutes")
        )
      }
  })
    
  #for simulation tab:
  #output under the bar-chart for explanations and further results:
  output$textForExplanation <- renderText({
    req(simulatedDataForPlot())
    req(input$repeatS)
    if ((!(is.null(simulatedDataForPlot())))|(!(is.na(input$repeatS)))){
      return(
        paste0(
          "The bar-chart above refers to a simulated day where prescriptions arrive",
          ifelse(input$arrivalS,
                 paste0(" at regular intervals between 9:00 a.m. and 5 p.m. Their number is the same as on ", 
                        input$arrivalDateSim %>% as.Date() %>% as.character(), 
                        ". "),
                 paste0(" between 8:00 a.m. to 6:30 p.m. at exactly the same times as they did on ", 
                       input$arrivalDateSim %>% as.Date() %>% as.character(), 
                       ". ")),
          "Please see below relevant metrics of the simulated process and, for comparison, of the workday of the chosen date. Figures in brackets and bars above indicate 95% confidence intervals. See further below the shift-pattern used by simulator."
        )
      )
    } 
  })
  
  #for simulation tab:
  #output of relevant metrics:
  output$resultsTableS <- renderTable({
    req(simulatedData())
    req(simulatedDataForPlot())
    req(input$repeatS)
    req(arrivalTimes())
    req(realDataForComparison())
    if ((!(is.null(simulatedDataForPlot())))|(!(is.na(input$repeatS)))){
      numberRxEntered <- length(arrivalTimes()[[1]]) + length(arrivalTimes()[[2]])
      numberRxCompleted <- simulatedData() %>% pull(startOfVerifQueu) %>% 
        length() %>% "/" (input$repeatS %>% as.numeric()) %>% round(digits = 0)
      numberComplOutp <- simulatedData() %>% pull(arrival) %>%
        as.character() %>% .[grepl("hatch",.)] %>% length() %>% 
        "/" (input$repeatS %>% as.numeric()) %>% round(digits = 0)
      numberComplInp <- simulatedData() %>% pull(arrival) %>%
        as.character() %>% .[grepl("wards",.)] %>% length() %>% 
        "/" (input$repeatS %>% as.numeric()) %>% round(digits = 0)
      avVerifQueue <- simulatedDataForPlot() %>% filter(activity == "verifying queue") %>% 
        pull(meanDuration)
      avDispQueue <- simulatedDataForPlot() %>% filter(activity == "dispensing queue") %>%
        pull(meanDuration)
      avFinCheckQueue <- simulatedDataForPlot() %>% filter(activity == "final check. queue") %>%
        pull(meanDuration)
      avThroughput <- simulatedDataForPlot() %>% filter(activity == "total throughput") %>%
        pull(meanDuration)
      avDispThroughput <- simulatedDataForPlot() %>% filter(activity == "disp. throughput") %>%
        pull(meanDuration)
      avWaitThroughputFract <- sum(avVerifQueue, avDispQueue, avFinCheckQueue)/avThroughput
      avWaitDispThroughputFract <- sum(avDispQueue, avFinCheckQueue)/avDispThroughput 
      simulatedDurations <- simulatedData() %>% 
        cbind(dispThroughput = .$dispQueue + .$dispensing + .$finCheckQueue + .$finChecking)
      partOfRxsCompletWithinCutOffT <- simulatedDurations %>% 
        filter(totThroughput <= 120) %>%
        pull(totThroughput) %>%
        length() %>%
        "/" (numberRxEntered * input$repeatS) 
      partOfRxsCompletWithinCutOffD <- simulatedDurations %>% 
        filter(dispThroughput <= 120) %>%
        pull(dispThroughput) %>%
        length() %>%
        "/" (numberRxEntered * input$repeatS) 
      return(
        data.frame(
          metric = c(
            "number of Rxs entering process", #
            "number of Rxs completing process", #
            "number of outpatient Rxs entering process", #
            "number of outpatient Rxs completing process", #
            "number of inpatient Rxs entering process", #
            "number of inpatient Rxs completing process", #
            "average verifying queue", #
            "average verifying activity", #
            "average dispensing queue", #
            "average dispensing activity", #
            "average fin. check. queue", #
            "average fin. check. activity", #
            "average throughput time", #
            "average throughput time (dispensary)", #
            "proport. of throughput time waiting", #
            "proport. of disp. throughput time waiting", #
            "proport. of all Rxs completed within 2 h", #
            "proport. of all Rxs processed in disp. within 2 h" #
          ),
          simulation = c(
            numberRxEntered %>% as.character(),
            paste0(
              numberRxCompleted %>% as.character(),
              " (",
              numberRxCompleted %>% "/" (numberRxEntered) %>%
                "*" (100) %>% round(digits = 1) %>% 
                as.character(), 
              "%)"
            ),
            length(arrivalTimes()[[1]]) %>%
              as.character(),
            paste0(
              numberComplOutp %>% as.character(),
              " (",
              numberComplOutp %>% "/" (length(arrivalTimes()[[1]])) %>%
                "*" (100) %>% round(digits = 1) %>%
                as.character(), 
              "%)"
            ),
            length(arrivalTimes()[[2]]) %>%
              as.character(),
            paste0(
              numberComplInp %>% as.character(),
              " (", 
              numberComplInp %>% "/" (length(arrivalTimes()[[2]])) %>%
                "*" (100) %>% round(digits = 1) %>%
                as.character(), 
              "%)"
            ),
            paste0(
              avVerifQueue %>% round(digits = 1) %>% as.character(),
              " min. (\u00b1 ",
              simulatedDataForPlot() %>% filter(activity == "verifying queue") %>% 
                pull(absError) %>% round(digits = 1) %>% as.character(),
              " min.)"
            ),
            paste0(
              simulatedDataForPlot() %>% filter(activity == "verifying activity") %>%
                pull(meanDuration) %>% round(digits = 1) %>% as.character(),
              " min. (\u00b1 ",
              simulatedDataForPlot() %>% filter(activity == "verifying activity") %>%
                pull(absError) %>% round(digits = 1) %>% as.character(),
              " min.)"
            ),
            paste0(
              avDispQueue %>% round(digits = 1) %>% as.character(),
              " min. (\u00b1 ",
              simulatedDataForPlot() %>% filter(activity == "dispensing queue") %>%
                pull(absError) %>% round(digits = 1) %>% as.character(),
              " min.)"
            ),
            paste0(
              simulatedDataForPlot() %>% filter(activity == "dispensing activity") %>%
                pull(meanDuration) %>% round(digits = 1) %>% as.character(),
              " min. (\u00b1 ",
              simulatedDataForPlot() %>% filter(activity == "dispensing activity") %>%
                pull(absError) %>% round(digits = 1) %>% as.character(),
              " min.)"
            ), 
            paste0(
              avFinCheckQueue %>% round(digits = 1) %>% as.character(),
              " min. (\u00b1 ",
              simulatedDataForPlot() %>% filter(activity == "final check. queue") %>%
                pull(absError) %>% round(digits = 1) %>% as.character(),
              " min.)"
            ), 
            paste0(
              simulatedDataForPlot() %>% filter(activity == "final check. activity") %>%
                pull(meanDuration) %>% round(digits = 1) %>% as.character(),
              " min. (\u00b1 ",
              simulatedDataForPlot() %>% filter(activity == "final check. activity") %>%
                pull(absError) %>% round(digits = 1) %>% as.character(),
              " min.)"
            ), 
            paste0(
              avThroughput %>% round(digits = 1) %>% as.character(),
              " min. (\u00b1 ",
              simulatedDataForPlot() %>% filter(activity == "total throughput") %>%
                pull(absError) %>% round(digits = 1) %>% as.character(),
              " min.)"
            ), 
            paste0(
              avDispThroughput %>% round(digits = 1) %>% as.character(),
              " min. (\u00b1 ",
              simulatedDataForPlot() %>% filter(activity == "disp. throughput") %>%
                pull(absError) %>% round(digits = 1) %>% as.character(),
              " min.)"
            ),
            paste0(
              avWaitThroughputFract %>% "*" (100) %>%
                round(digits = 1) %>% as.character(),
              "%"
            ),
            paste0(
              avWaitDispThroughputFract %>% "*" (100) %>%
                round(digits = 1) %>% as.character(), 
              "%"
            ), 
            paste0(
              partOfRxsCompletWithinCutOffT %>% "*" (100) %>%
                round(digits = 1) %>% as.character(),
              "%"
            ), 
            paste0(
              partOfRxsCompletWithinCutOffD %>% "*" (100) %>%
                round(digits = 1) %>% as.character(),
              "%"
            )
          ), 
          reality = realDataForComparison()
        )
      )
    }
  })
  
  #for simulation tab:
  #output of shiftpatterns:
  output$shiftPatternTable <- renderTable({
    shiftPatterns()
  },
  width = "30px",
  rownames = T,
  colnames = T,
  digits = 0,
  spacing = "xs")
  
  # ####################### #
  # # FOR DATA UPLOAD TAB # #
  # ####################### #
  
  
  
  #this reactive returns a data-frame that displays data on the available data-sets 
  availableDataSetsDF <- reactive({
    req(availableDataSetsV())
    #building the data-frame column by column:
    yearVector <- availableDataSetsV() %>% substr(start = 3, stop = 6) %>% as.numeric()
    quarterVector <- availableDataSetsV() %>% substr(start = 8, stop = 9) %>% as.numeric() %>% "-" (1) %>%  "/" (3) %>% "+" (1) %>% trunc()
    quarterStartVector <- availableDataSetsV() %>% substr(start = 3, stop = 12) %>% as.Date()
    quarterEndVector <- availableDataSetsV() %>% substr(start = 15, stop = 24) %>% as.Date()
    #generating and ordering data-frame
    return(
      data.frame(year = yearVector, quarter = quarterVector, start = quarterStartVector, end = quarterEndVector) %>%
        .[order(.$year, .$quarter, .$start),]
    )
  })
  
  #outputting table of available datasets:
  output$availableDataSets <- renderTable({
    req(availableDataSetsDF())
    returnDataFrame <- availableDataSetsDF()
    returnDataFrame$year <- returnDataFrame$year %>% format(digits = NULL)
    returnDataFrame$quarter <- returnDataFrame$quarter %>% format(digits = NULL, justify = "right")
    returnDataFrame$start <- returnDataFrame$start %>% format.Date(format = "%d/%m/%Y")
    returnDataFrame$end <- returnDataFrame$end %>% format.Date(format = "%d/%m/%Y")
    return(
      returnDataFrame
    )
  })
  
  output$selectDataSet <- renderUI({
    req(availableDataSetsV())
    return(
      selectInput(inputId = "selectedDataSet", 
                  label = "select dataset for analysis", 
                  choices = availableDataSetsV())
    )
  })
  
  #data-frame uploaded from Excel file presented by the user:
  uploadedRawData <- reactive({
    req(input$uploadFromExcel)
    showNotification("Trying to upload data. Please wait ...", duration = 15, 
                     closeButton = T)
    #browser()
    if (!( #checking for situation where not an Excel file was uploaded
      substr(
        x = input$uploadFromExcel$datapath, 
        start = (
          nchar(input$uploadFromExcel$datapath) - 3
        ), 
        stop = nchar(input$uploadFromExcel$datapath)
      ) %in% c(".xls", "xlsx")
    )){
      showNotification("The uploaded file is not of Excel format. Data not uploaded.", 
                       duration = 15, type = "error", closeButton = T)
      return(NULL)
    }else{
      rawData <- try(
        read_excel(path = input$uploadFromExcel$datapath, sheet = 1)
      ) #How could I create an error handler that could deal with missing columnnames etc.?
      return(
        rawData
      )
    }
  })

  #transformed data from uploaded Excel file:
  observeEvent(uploadedRawData(), {
    req(uploadedRawData())
    showNotification("Data from Excel sheet successfully uploaded", duration = 15, 
                     closeButton = T)
    #checking whether all important columns available in uploaded dataframe:
    if (!(is.data.frame(uploadedRawData()))){
      showNotification("Uploaded data not a dataframe and cannot be added to existing data sets.", 
                       duration = 15, type = "error", closeButton = T)
      #return(NULL)
    }else if (any(!(c("PrescriptionID", "Ward", "PrescriptionType", 
                      "Activity", "Start", "End") %in% colnames(uploadedRawData())))){
      showNotification("Necessary columns not in uploaded Excel file. Data cannot be added to existing data sets.", 
                       duration = 15, type = "error", closeButton = T)
      #return(NULL)
    }else{
      earliestDateInUpload <- uploadedRawData()$Started %>% min() %>% as.Date()
      latestDateInUpload <- uploadedRawData()$Started %>% max() %>% as.Date()
      #browser()
      #Checking whether the earliest date in the uploaded dataframe is exactly
      #following the latest date of an existing dataset:
      datesAfterExistingDataSets <- availableDataSetsDF()$end + 1
      datesBeforeExistingDataSets <- availableDataSetsDF()$start - 1
      
      #Three contingencies are tested that allow for data to be joined with existing data set or
      #for a new data set to be added. If these conditions are not met, data cannot be added to
      #existing data sets.
      #1.) Uploaded data follows last day of existing data set and is completely within the same quarter
      #    existing data set.
      #2.) Uploaded data precedes first day of existing data set and is completely within the same
      #    quarter as existing data set. 
      #3.) Uploaded data is from a different quarter than all existing data sets and the activities 
      #    therein are all from one quarter only.
      if ((earliestDateInUpload %in% datesAfterExistingDataSets)&
          (latestDateInUpload <= latestDateInQuarter(earliestDateInUpload - 1))){
          showNotification("Trying to transform uploaded data. Please be patient, this can take minutes.", 
                           duration = 15, closeButton = T)
          transformedData <- try(
            uploadedRawData() %>% rawDataTransformer()
            )
          showNotification("Uploaded file successfully transformed. Trying to combine with existing dataset now ...", 
                           duration = 15, closeButton = T)
          toBeAmendedFileName <- availableDataSetsV()[ #this is the underlying dataset that is to be amended
            (
              (substr(availableDataSetsV(),start = 15, stop = 24) == (as.character(earliestDateInUpload-1)))
            )
            ]
          toBeAmendedFile <- readRDS(file = paste0(
            "data/",
            toBeAmendedFileName
          ))
          combinedFileAbsTimes <- rbind(toBeAmendedFile[[1]], transformedData[[1]])
          combinedFileDurs <- rbind(toBeAmendedFile[[2]], transformedData[[2]])
          combinedFileDursS <- rbind(toBeAmendedFile[[3]], transformedData[[3]])
          combinedOutOfRange <- append(toBeAmendedFile[[4]], transformedData[[4]])
          combinedRejected <- append(toBeAmendedFile[[5]], transformedData[[5]])
          combinedFile <- list(combinedFileAbsTimes, combinedFileDurs, combinedFileDursS,
                               combinedOutOfRange, combinedRejected)
          #directory is prepared for saving old .rds files to, if needed
          if (!(dir.exists(paths = "data/old processed raw data"))){
            dir.create(path = "data/old processed raw data")
          }
          file.copy(from = paste0(
            "data/",
            toBeAmendedFileName
          ), to = "data/old processed raw data")
          file.remove(paste0(
            "data/",
            toBeAmendedFileName)
          )
          earliestDateInCombined <- combinedFile[[1]]$startOfVerifQueu %>% min() %>%
            as.Date() %>% as.character()
          latestDateInCombined <- combinedFile[[1]]$startOfVerifQueu %>% max() %>%
            as.Date() %>% as.character()
          saveRDS(combinedFile, file = paste0(
            "data/fa",
            earliestDateInCombined,
            "--",
            latestDateInCombined,
            ".rds"
          ))
          showNotification("Attachment of transformed data to existing dataset successful.", 
                           duration = 15, closeButton = T)
      }else if ((latestDateInUpload %in% datesBeforeExistingDataSets)&
                (earliestDateInUpload >= earliestDateInQuarter(latestDateInUpload + 1))){
          showNotification("Trying to transform uploaded data. Please be patient, this can take minutes.", 
                           duration = 15, closeButton = T)
          transformedData <- try(
            uploadedRawData() %>% rawDataTransformer()
            )
          showNotification("Uploaded file successfully transformed. Trying to combine with existing dataset now ...", 
                           duration = 15, closeButton = T)
          toBeAmendedFileName <- availableDataSetsV()[ #this is the underlying dataset that is to be amended
            (
              (substr(availableDataSetsV(),start = 3, stop = 12) == (as.character(latestDateInUpload + 1)))
            )
            ]
          toBeAmendedFile <- readRDS(file = paste0(
            "data/",
            toBeAmendedFileName
          ))
          combinedFileAbsTimes <- rbind(transformedData[[1]], toBeAmendedFile[[1]])
          combinedFileDurs <- rbind(transformedData[[2]], toBeAmendedFile[[2]])
          combinedFileDursS <- rbind(transformedData[[3]], toBeAmendedFile[[3]])
          combinedOutOfRange <- append(transformedData[[4]], toBeAmendedFile[[4]])
          combinedRejected <- append(transformedData[[5]], toBeAmendedFile[[5]])
          combinedFile <- list(combinedFileAbsTimes, combinedFileDurs, combinedFileDursS,
                               combinedOutOfRange, combinedRejected)
          #directory is prepared for saving old .rds files to, if needed
          if (!(dir.exists(paths = "data/old processed raw data"))){
            dir.create(path = "data/old processed raw data")
          }
          file.copy(from = paste0(
            "data/",
            toBeAmendedFileName
          ), to = "data/old processed raw data")
          file.remove(paste0(
            "data/",
            toBeAmendedFileName)
          )
          earliestDateInCombined <- combinedFile[[1]]$startOfVerifQueu %>% min() %>%
            as.Date() %>% as.character()
          latestDateInCombined <- combinedFile[[1]]$startOfVerifQueu %>% max() %>%
            as.Date() %>% as.character()
          saveRDS(combinedFile, file = paste0(
            "data/fa",
            earliestDateInCombined,
            "--",
            latestDateInCombined,
            ".rds"
          ))
          showNotification("Upload and attachment of uploaded dataset to existing data successful.", 
                           duration = 15, closeButton = T)
          showNotification("The app will now be reset.", duration = 15, closeButton = T, 
                           type = "warning")
      }else if ( #checking if the (earlieast date of the) uploaded dataframe is 
        #in the same year and quarter as any existing dataset
        (
          availableDataSetsDF() %>% 
          filter((year == yearOfDate(earliestDateInUpload))&(quarter == quarterOfDate(earliestDateInUpload))) %>% 
          nrow()
        ) == 0
      ){
        if (latestDateInUpload <= latestDateInQuarter(earliestDateInUpload)){
          showNotification("Uploaded data from different quarter than all existing datasets.", duration = 15, 
                           closeButton = T)
          showNotification("Trying to transform uploaded data. Please be patient, this can take minutes.", 
                           duration = 30, closeButton = T)
          transformedData <- try(
            uploadedRawData() %>% rawDataTransformer()
          )
          showNotification("Uploaded data transformed. Saving new and separate dataset.",
                           duration = 15, closeButton = F)
          saveRDS(transformedData, file = paste0(
            "data/fa",
            earliestDateInUpload %>% as.character(),
            "--",
            latestDateInUpload %>% as.character(),
            ".rds"
          ))
        }else{
          showNotification("Dates of uploaded dataset of different quarter than all existing datasets. Nevertheless, file could not be uploaded as earliest and last date of uploaded dataset not within same quarter.", 
                           duration = 15, closeButton = T, type = "error")
        }
      }else{
        showNotification("Dates of uploaded dataset not adjacent to and not in different quarter than existing datasets. File could not be added to existing datasets.",
                         duration = 15, closeButton = T, type = "error")
      }
      showNotification("Application will now be re-started.", duration = 10, 
                       type = "warning", closeButton = T)
      Sys.sleep(5)
      {js$reset()} #this is to re-set the app
    }
  })
  
  
  
  
} #end of server function




########################################################################################################
#Required to create the Shiny app:
shinyApp(ui = ui, server = server)


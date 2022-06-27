

#dataDir=Sys.getenv('PROJECT_DATA')
dataDir="/Volumes/food/pilot/dev/release_candidate/data/"

source('matchOnTime2.R')
source('matchNearest.R')

mydata = read.table(paste0(dataDir,'/original/CollectingFoodAndDri_DATA_2022-01-08_1329.csv'), sep=',', header=1, stringsAsFactors=FALSE, quote="\"")
diaryDates = read.table(paste0(dataDir,'/original/participation-dates.csv'), sep='\t', header=1, stringsAsFactors=FALSE)
deviceIDmapping = read.table(paste0(dataDir, '/original/device_ids.txt'), sep='\t', header=1, stringsAsFactors=FALSE)

registered = mydata[which(mydata$q1_pid!=""),]
#dim(registered)


# Remove duplicate records

dupRecordIds = read.csv(paste0(dataDir, 'original/duplicate-records.txt'))
ixDup = which(mydata$record_id %in% dupRecordIds$recordID)
mydata = mydata[-ixDup,]




##
## get all the redcap food form entries

foodeventsAll = mydata[which(mydata$redcap_repeat_instrument=="q2foodform"),c("record_id", "q2foodform_timestamp", "foodform_date_time", "foodform_text", "foodform_rating", "foodform_comments")]



# remove all food events where participant did not enter a intake timestamp
print('remove web form submission with no intake timestamp')
print(dim(foodeventsAll))
ix = which(is.na(foodeventsAll$foodform_date_time) | foodeventsAll$foodform_date_time=="")
foodeventsAll = foodeventsAll[-ix,]
print(dim(foodeventsAll))



# format date/time from string
foodeventsAll$q2foodform_timestamp = strptime(foodeventsAll$q2foodform_timestamp, format='%Y-%m-%d %H:%M:%S')

#dim(foodeventsAll)



ix = which(!is.na(mydata$q1_pid) & mydata$q1_pid!="")
recordIDs = unique(mydata$record_id[ix])

print("Unique IDs with web form food diaries:")
print(recordIDs)


# get all alexa intake events

alexaEventsAll = read.table(paste0(dataDir,'/original/intake_event_alexa20211208.tab'), sep='\t', header=1, stringsAsFactors=FALSE)
alexaElementsAll = read.table(paste0(dataDir,'/original/intake_element_alexa20211208.tab'), sep='\t', header=1, stringsAsFactors=FALSE, quote="\"")
alexaEventsAll$creation_timestamp = strptime(alexaEventsAll$creation_timestamp, format='%Y-%m-%d %H:%M:%S')


results = data.frame(pid=character(), record_id=integer(), exclude=character(), n_web=integer(), n_alexa_p=integer(), n_alexa_c=integer(), n_paired=integer(), n_paired_c=integer(), n_paired_c_timematch=integer())

##
## for each web food form entry, try to map to alexa intake events

for (recordID in recordIDs) {

  # get pid, and alexa start and end dates for this record ID
  
  pid = mydata$q1_pid[which(mydata$record_id == recordID & mydata$q1_pid!="")] 

  print(paste0("RECORD: ", recordID, " (", pid, ")"))


  ## data swapped for 2 participants
  ## web_food_link_supplied

  webLinkSupplied = diaryDates$web_food_link_supplied[which(diaryDates$pid==pid)]
  foodDiaryRecordID = NA
  if (!is.na(webLinkSupplied) & webLinkSupplied!="") {
    
    ## use web diary of other participant
    print('SWAPPING FOOD DIARIES FOR SWAPPED PARTICIPANTS')
    foodDiaryRecordID = mydata$record_id[which(mydata$q1_pid==webLinkSupplied)]
    print(foodDiaryRecordID)
  }


  ## exclude participants
  ## e.g withdrew from study, boxes mixed up
  exclude = diaryDates$exclude[which(diaryDates$pid==pid)]
  if (!is.na(exclude) & exclude!="") {
    print(paste0('EXCLUDED: ', exclude))
    thisRes = data.frame(pid=pid, record_id=recordID, exclude=exclude, n_web=NA, n_alexa_p=NA, n_alexa_c=NA, n_paired=NA, n_paired_c=NA, n_paired_c_timematch=NA)
    results = rbind(results, thisRes)
    next
  }


  startDate = strptime(diaryDates$diary_start[which(diaryDates$pid==pid)], format='%d/%m/%Y')
  endDate = strptime(diaryDates$diary_end[which(diaryDates$pid==pid)], format='%d/%m/%Y')

  if (is.na(startDate) | is.na(endDate)) {
    print(paste0("Record ", recordID, "(PID ", pid, ") has no start or end date for alexa use period.")) 
    next
  }

  # get box letter participant used
  device = diaryDates$device[which(diaryDates$pid==pid)]

  ##
  ## check if there was a rearrangement of the alexa period, to a different date range/device
  startDate2 = strptime(diaryDates$diary_start2[which(diaryDates$pid==pid)], format='%d/%m/%Y')
  if (!is.na(startDate2) ) {
    startDate = startDate2
    endDate = strptime(diaryDates$diary_end2[which(diaryDates$pid==pid)], format='%d/%m/%Y')
    device = diaryDates$device2[which(diaryDates$pid==pid)]
  }

  # get alexa device ID from device box (A,B,C or D), used to link to alexa input
  
  alexaDeviceID = deviceIDmapping$device_id[which(deviceIDmapping$device == device)]
#  print(alexaDeviceID)

  # get all web form food diary events for this participant

  if (!is.na(foodDiaryRecordID)) {
    ix = which(foodeventsAll$record_id == foodDiaryRecordID)
  } else {
    ix = which(foodeventsAll$record_id == recordID) 
  }

  foodevents = foodeventsAll[ix,]

  if (nrow(foodevents)>0) {
    foodevents$id = 1:nrow(foodevents)
  }

  # get alexa events for this participant

  ix = which(alexaEventsAll$creation_timestamp>=startDate & alexaEventsAll$creation_timestamp<=endDate & alexaEventsAll$device_id == alexaDeviceID)
  alexaEvents = alexaEventsAll[ix,]
  alexaEventsComplete = alexaEvents


  ##
  ## restrict to useful fields and save alexa events/elements and web events to file

  ## alexa events
  alexaEventsComplete$intake_date_time = paste(alexaEventsComplete$intake_date, alexaEventsComplete$intake_time)
  alexaEventsComplete = alexaEventsComplete[,c("event_id","creation_timestamp", "intake_date_time", "partial")]
  write.table(alexaEventsComplete, paste0(dataDir, '/derived/alexa-events-complete-',recordID,'.csv'), sep=',', row.names=FALSE)

  ## alexa elements
  alexaElementsComplete = alexaElementsAll[which(alexaElementsAll$event_id %in% alexaEventsComplete$event_id),]
  write.table(alexaElementsComplete, paste0(dataDir, '/derived/alexa-elements-complete-',recordID,'.csv'), sep=',', row.names=FALSE)
  
  ## save web events
  foodevents$foodform_comments = gsub("\"", "", foodevents$foodform_comments)
  foodevents$foodform_text = gsub("\"", "", foodevents$foodform_text)
  write.table(foodevents, paste0(dataDir, '/derived/food-events-',recordID,'XX.csv'), sep=',', row.names=FALSE)

  ## create matrix of possible matches
  ## a possible match is an assignment of an alexa event to a web event, where it is submitted within the 30 min period before the web submission
  ## C: completed alexa entry, P: partial alexa entry


  if (nrow(foodevents)>0 & nrow(alexaEventsComplete)>0) {
    matches = matrix(NA, nrow=nrow(foodevents), ncol = nrow(alexaEventsComplete), dimnames=list(paste0("f",foodevents$id), paste0("a",alexaEventsComplete$event_id)))
  } else if (nrow(alexaEventsComplete)>0) {
    matches = matrix(NA, nrow=nrow(foodevents), ncol = nrow(alexaEventsComplete), dimnames=list(NULL, paste0("a",alexaEventsComplete$event_id)))
  } else if (nrow(foodevents)>0) {
    matches = matrix(NA, nrow=nrow(foodevents), ncol = nrow(alexaEventsComplete), dimnames=list(paste0("f",foodevents$id), NULL))
  } else {
    matches = matrix(NA, nrow=0, ncol=0)
  }

  if (nrow(foodevents)>0 & nrow(alexaEventsComplete)>0) {
  for (i in 1:nrow(foodevents)) {

    ## 30 mins before alexa event was entered

    # complete alexa entries
    ixB = which(difftime(alexaEventsComplete$creation_timestamp, foodevents$q2foodform_timestamp[i], units="mins") <0 & difftime(alexaEventsComplete$creation_timestamp, foodevents$q2foodform_timestamp[i], units="mins") > -30 & alexaEventsComplete$partial == 0)
    matches[i,ixB] = "C"

    # partial alexa entries
    ixB = which(difftime(alexaEventsComplete$creation_timestamp, foodevents$q2foodform_timestamp[i], units="mins") <0 & difftime(alexaEventsComplete$creation_timestamp, foodevents$q2foodform_timestamp[i], units="mins") > -30 & alexaEventsComplete$partial == 1)
    matches[i,ixB] = "P"
    
  }
#  print(matches)


  ## for identified potential matches, match on intake time/date
  ## append 'T' to these cells
  matches = matchOnTime2(foodevents, alexaEventsComplete, alexaElementsComplete, matches)

  ## for partial matches, match to next entered web submission
  ## append 'N' to these cells
  matches = matchNearest(foodevents, alexaEventsComplete, alexaElementsComplete, matches)

#  print(matches)
  }

  ## after this matching process the cells have the following possible values:
  ## NA, C, P, CT, CN, PT, PN


  ##
  ## summarise data

  ## number of web submissions
  numWeb = nrow(foodevents)
  print(paste0('Total number of web submissions: ', numWeb))

  ## number of partial alexa submissions
  numAlexaP = length(which(alexaEventsComplete$partial == 1))
  print(paste0('Total number of partial alexa submissions: ', numAlexaP))
  
  ## number of complete alexa submissions
  numAlexaC = length(which(alexaEventsComplete$partial == 0))
  print(paste0('Total number of completed alexa submissions: ', numAlexaC))

  ## number of web submissions with alexa attempt made - paired-attempt
  numPaired = length(unique(which(matches == "CT" | matches == "CN" | matches == "PT" | matches == "PN", arr.ind = TRUE)[,'row']))
  print(paste0('number of web submissions with at least 1 alexa attempt made nearby: ', numPaired))

  ## number of paired attempts with completed submission
  numPairedC = length(unique(which(matches == "CT" | matches == "CN", arr.ind = TRUE)[,'row']))
  print(paste0('number of web submissions with completed alexa submissions nearby: ', numPairedC))

  ## number of paired-attempts with matching intake date and time
  numSameTime = length(unique(which(matches == "CT", arr.ind = TRUE)[,'row']))
  print(paste0('number of web submissions with completed alexa submission with matching intake date and time: ', numSameTime))



  thisRes = data.frame(pid=pid, record_id=recordID, exclude=NA, n_web=numWeb, n_alexa_p=numAlexaP, n_alexa_c=numAlexaC, n_paired=numPaired, n_paired_c=numPairedC, n_paired_c_timematch=numSameTime)
  results = rbind(results, thisRes)

  # save matrix of diary matches
  write.csv(matches, paste0(dataDir, '/derived/diary-matches-',recordID,'.csv'))

}

# save summaries of exclusions and matches etc for all participants
write.table(results, paste0(dataDir, '/derived/diary-results.csv'), sep=',', row.names=FALSE)

warnings()



dataDir="/Volumes/food/pilot/dev/release_candidate/data/"
resdir='/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/'

library(ggplot2)
library(pluralize)

source('getSample.R')
sampleData = getSample('counterpart')


first=TRUE

resAll = data.frame(idx=c(),recordID=c(), entryID=c(), same=c(), total=c(), webonly=c(), alexaonly=c(), timeDiff=c(), timeWeb=c())



idx=1


for (recordID in sampleData$record_id) {
print(recordID)

# load data for participant
matches = read.table(paste0(dataDir, '/derived/diary-matches-',recordID,'.csv'), sep=',', header = 1)

#foodEntries = read.table(paste0(dataDir, '/derived/food-events-',recordID,'XX.csv'), sep=',', header=1)
foodEntries = read.csv(paste0(dataDir, '/derived/food-events-',recordID,'XX.csv'))

alexaEvents = read.csv(paste0(dataDir, '/derived/alexa-events-complete-',recordID,'.csv'))

alexaElements = read.csv(paste0(dataDir, '/derived/alexa-elements-complete-',recordID,'.csv'))

# for each match in matches

rowIdxs = unique(which(matches == "CT" | matches == "CN", arr.ind = TRUE)[,'row'])
#print(which(matches == "CT" | matches == "CN", arr.ind = TRUE)[,'row'])
#print(length(rowIdxs))

#textDF = data.frame(webText=c(), alexaText=c())

#print(matches)

if (length(rowIdxs)>0) {

  res = data.frame(idx=c(), recordID=c(), entryID=c(), same=c(), total=c())

  for (rowIdx in rowIdxs) {

    # get ids for match
    foodEntryID = gsub("f","",matches[rowIdx,"X"])
    colIdx = which(matches[rowIdx,] == "CT" | matches[rowIdx,] == "CN")
    alexaEventID = colnames(matches)[colIdx]

    # if more than one match then take the first web entry - the one entered nearest to the alexa entry
    if (length(colIdx)>1) {
      colIdx = colIdx[1]
    }

    # get alexa event text
    alexaEventID = gsub("a","",colnames(matches)[colIdx])
    aEvent = alexaEvents[which(alexaEvents$event_id == alexaEventID),]
    aElements = alexaElements[which(alexaElements$event_id == alexaEventID),]

    # get web entry
    webEntry = foodEntries[which(foodEntries$id == foodEntryID),]


    ## save data for manual review
    tosave = data.frame(recordID=recordID, foodEntryID=foodEntryID, webEntry=gsub("\n", " || ", webEntry$foodform_text), alexaEntry=paste(aElements$description, collapse=' || '))
    tosave$different_eating_occurence=""
    tosave$same_item=""
    tosave$same_item_different_detail=""
    tosave$same_item_alexa_less_detail=""
    tosave$same_item_alexa_more_detail=""
    tosave$same_item_alexa_misspelt=""
    tosave$same_item_web_missspelt=""
    tosave$extra_alexa_item=""
    tosave$extra_web_item=""
    tosave$total_alexa_items=""
    tosave$total_web_items=""

    if (first == TRUE) {
      write.table(tosave, paste0(dataDir, '/derived/manual/entries_to_compare.csv'), row.names=FALSE, sep=',', append=FALSE, col.names=TRUE)
    }
    else {
      write.table(tosave, paste0(dataDir, '/derived/manual/entries_to_compare.csv'), row.names=FALSE, sep=',', append=TRUE, col.names=FALSE)
    }
    first = FALSE


     webEntry$foodform_text = gsub(" +", " ", webEntry$foodform_text)

     # convert entries to vector of words
     webTextTokens = unlist(strsplit(webEntry$foodform_text, " "))
     alexaTextTokens = unlist(strsplit(paste(aElements$description, collapse=' '), " "))

     ## preprocess to make consistent representation

     # singularize
     webTextTokens = tolower(singularize(webTextTokens))
     alexaTextTokens = tolower(singularize(alexaTextTokens))

     # consistent numbers
     webTextTokens = gsub("^one$", "1", webTextTokens)
     alexaTextTokens = gsub("^one$", "1", alexaTextTokens)

     webTextTokens = gsub("^two$", "2", webTextTokens)
     alexaTextTokens = gsub("^two$", "2", alexaTextTokens)
     
     webTextTokens = gsub("^a$", "1", webTextTokens)
     alexaTextTokens = gsub("^a$", "1", alexaTextTokens)


     # get counts for term intersection and differences
     sameCount = length(intersect(webTextTokens, alexaTextTokens))
     webonly= length(setdiff(webTextTokens, alexaTextTokens))
     alexaonly= length(setdiff(alexaTextTokens, webTextTokens))

     # calculate difference between web and alexa intake times
     aEvent$intake_date_time = strptime(aEvent$intake_date_time, format='%Y-%m-%d %H:%M:%S')
     webEntry$foodform_date_time = strptime(webEntry$foodform_date_time, format='%Y-%m-%d %H:%M:%S')
     timeDiff = difftime(aEvent$intake_date_time, webEntry$foodform_date_time, units="mins")
     timeWeb = webEntry$foodform_date_time

     # add summaries to results df
     res = rbind(res, data.frame(idx=idx, recordID=recordID, entryID=foodEntryID, same=sameCount, webonly=webonly, alexaonly=alexaonly, timeDiff=timeDiff, timeWeb=timeWeb))

     idx = idx+1
  }



  resAll = rbind(resAll, res)

  # make gap between participants bars
  idx = idx+1
}

}


write.table(resAll, paste0(dataDir, '/derived/auto-comparison.csv'), sep=',', row.names=FALSE, col.names=TRUE)








dataDir="/Volumes/food/pilot/dev/release_candidate/data/"


###
### load data

library("xlsx")

myReviewData = read.xlsx(paste0(dataDir,'/derived/manual/food_diary_review/entries_to_compare_lacm-v3.xlsx'), 1, TRUE)
print(dim(myReviewData))

set.seed(1234567)

## create 5 sets with 10 random entries (no overlap across them i.e. sampled without replacement)

numRows = nrow(myReviewData)

allsample = sample(1:numRows, 50, replace=FALSE)


for (i in 1:5) {

  # get sample for independent review i
  startIdx = ((i-1)*10)+1
  endIdx = startIdx + 9
  print(paste0(i, ',', startIdx, ': ', endIdx))

  thissample = allsample[startIdx:endIdx]


  # get rows for this sample
  thisrows = myReviewData[thissample, ]

  # get record id and food entry id, and remove them from thisrows, we store them separately
  thisrecordids = thisrows[,c('recordID','foodEntryID'), drop=FALSE]
  thisrows$recordID = NULL
  thisrows$foodEntryID = NULL

  # remove notes column
  thisrows$notes = NULL

  # remove my assignments from the data
  thisrowsBlank = thisrows
  thisrowsBlank[1:10,3:ncol(thisrowsBlank)] = NA

  # change web entry to use || to separate items
  thisrowsBlank$webEntry = gsub("   ", " || ", thisrowsBlank$webEntry)
  
  # save dataset for independent review 
  write.xlsx(thisrowsBlank, paste0(dataDir,'/derived/manual/entries_to_compare-sample',i,'.xlsx'), row.names=FALSE, showNA=FALSE)
  
  # save record ids so we have these for comparing with my review after
  write.csv(thisrecordids, paste0(dataDir,'/derived/manual/entries_to_compare-sample-recordIDs-',i,'.csv'), row.names=FALSE)
  
}




getAssessment <- function(dataDir, thisFile) {

# this file path and name
#thisFile = files[1]

# get sample num
sampleNum = sub(".*sample([1-9])*.*", "\\1", thisFile)
#print(sampleNum)

# load particpant ID and example ID linker file
linker = read.csv(paste0(dataDir, '/derived/manual/food_diary_review/samples/entries_to_compare-sample-recordIDs-',sampleNum,'.csv'))


# read in this assessors assessment
assessment = read.xlsx(thisFile, sheetIndex=1)
assessment$samplenum = sampleNum
assessment$indep = TRUE
assessment = cbind(assessment, linker)

# get my assessment for this sample
myassessThis = merge(myassess, linker, by=c('recordID', 'foodEntryID'), all.x=FALSE, all.y=FALSE)
stopifnot(nrow(myassessThis)==10)
myassessThis$samplenum = sampleNum
print(colnames(myassessThis))
print(colnames(assessment))

res = merge(myassessThis, assessment, all=TRUE, sort=FALSE)


res$same_item_both_misspelt[which(is.na(res$same_item_both_misspelt))] = 0
res$item_with_web_entry_issue[which(is.na(res$item_with_web_entry_issue))] = 0

return(res)

}



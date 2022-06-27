

getSample <- function(sampleType) {

dataDir="/Volumes/food/pilot/dev/release_candidate/data/"

mydata = read.table(paste0(dataDir,'/derived/diary-results.csv'), sep=',', header=1, stringsAsFactors=FALSE)

print(paste0("Number of participants who filled in first questionnaire: ", nrow(mydata)))


print(unique(mydata$exclude))

for (excludex in unique(mydata$exclude, na.rm=TRUE)) {

  if (is.na(excludex)) {
    next
  }

  ix = which(mydata$exclude == excludex)

  print(paste0("Number of participants with exclusion reason \"", excludex, "\": ", length(ix)))
  print(mydata$pid[ix])

  mydata = mydata[-ix, ]
  print(paste0("Remaining: ", nrow(mydata)))
}

mydata$exclude = NULL


if (sampleType == "counterpart") {

  ## remove if they don't have any partial or complete attempts
  ix = which(mydata$n_alexa_c==0 & mydata$n_alexa_p==0)
  print(paste0("Number of participants with no partial or complete (paired or unpaired) attempts: ", length(ix)))
  mydata = mydata[-ix, ]
  print(paste0("Remaining: ", nrow(mydata)))


  ## only include those with >0 partial or complete paired attempts
  ## i.e. they tried and submitted the web form soon after submitting to alexa

  ix = which(mydata$n_paired_c==0)
  print(paste0("Number of participants with no complete paired attempts: ", length(ix)))
  mydata = mydata[-ix, ]
  print(paste0("Remaining: ", nrow(mydata)))

}

#print(mydata)


return(mydata)

}

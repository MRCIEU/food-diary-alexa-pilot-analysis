

dataDir="/Volumes/food/pilot/dev/release_candidate/data/"

source('getSample.R')
sampleData = getSample('counterpart')


## proportion of completed Alexa entries with the correct time


pdf("/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/scatter_counterpart_time.pdf")
plot(sampleData$n_paired_c, sampleData$n_paired_c_timematch, xlab="N completed counterpart entries", ylab="N completed counterpart entries with matched timestamp")
lines(x=c(0,50), y=c(0,50))
dev.off()



##
## evaluate intake timestamps


## overall proportion across all participants

totalPaired = sum(sampleData$n_paired_c)
totalPairedAndMatched = sum(sampleData$n_paired_c_timematch)

proportionTimestamp = totalPairedAndMatched/totalPaired
print(paste0("Proportion of counterpart pairs with matching intake timestamp: ", totalPairedAndMatched, "/", totalPaired, "=", proportionTimestamp))

## summarise the proportion correct for each participant

sampleData$proportionTimestamp = sampleData$n_paired_c_timematch / sampleData$n_paired_c

summary(sampleData$proportionTimestamp)

print("Proportion intake timestamps correct")
print(paste(mean(sampleData$proportionTimestamp, na.rm=TRUE), sd(sampleData$proportionTimestamp, na.rm=TRUE)))



pdf("/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/hist_timestamp_matching.pdf")
hist(sampleData$proportionTimestamp)
dev.off()


pdf("/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/scatter_timestamp_matching_n_entries.pdf")
plot(sampleData$proportionTimestamp, sampleData$n_paired_c, xlab="Proportion of completed counterpart entries with matching timestamp", ylab="Number of completed counterpart entries")
dev.off()





###
### summaries stratifying on accents and previous Alexa use

redcapData = read.table(paste0(dataDir,'/original/CollectingFoodAndDri_DATA_2022-01-08_1329.csv'), sep=',', header=1, stringsAsFactors=FALSE)


###
### summarise q1 data for usage sample - basic demographics

# merge in q1 data
registered = redcapData[which(redcapData$q1_pid!="" & redcapData$record_id %in% sampleData$record_id),]
mydata = merge(sampleData, registered, by.x = "pid", by.y = "q1_pid")

# Do you consider yourself to have a strong UK regional accent?
print("Accent UK")
# 1: Yes, 2: a little, 3: no, 4: prefer not to answer

mydata$accent = NA
mydata$accent[which(mydata$accent_second == 3 & mydata$accent_regional == 3)] = 0
mydata$accent[which(mydata$accent_second == 1 | mydata$accent_second == 2 | mydata$accent_regional == 1 | mydata$accent_regional == 2)] = 1
table(mydata$accent)

print("Proportion Timestamp for those with no accent")
summary(mydata$proportionTimestamp[which(mydata$accent == 0)])
totalPaired = sum(mydata$n_paired_c[which(mydata$accent == 0)])
totalPairedAndMatched = sum(mydata$n_paired_c_timematch[which(mydata$accent == 0)])
proportionTimestamp = totalPairedAndMatched/totalPaired
print(paste0("Proportion of counterpart pairs with matching intake timestamp: ", proportionTimestamp))

print("Proportion Timestamp for those with an accent")
summary(mydata$proportionTimestamp[which(mydata$accent == 1)])
totalPaired = sum(mydata$n_paired_c[which(mydata$accent == 1)])
totalPairedAndMatched = sum(mydata$n_paired_c_timematch[which(mydata$accent == 1)])
proportionTimestamp = totalPairedAndMatched/totalPaired
print(paste0("Proportion of counterpart pairs with matching intake timestamp: ", proportionTimestamp))




mydata$device = NA
mydata$device[which(mydata$alexa_home == 2 | mydata$alexa_home == 3)] = 0
mydata$device[which(mydata$alexa_home == 1)] = 1
table(mydata$alexa_home)


print("Proportion Timestamp for those with no device at home")
summary(mydata$proportionTimestamp[which(mydata$device == 0)])
totalPaired = sum(mydata$n_paired_c[which(mydata$device == 0)])
totalPairedAndMatched = sum(mydata$n_paired_c_timematch[which(mydata$device == 0)])
proportionTimestamp = totalPairedAndMatched/totalPaired
print(paste0("Proportion of counterpart pairs with matching intake timestamp: ", proportionTimestamp))

print("Proportion Timestamp for those with a device at home")
summary(mydata$proportionTimestamp[which(mydata$device == 1)])
totalPaired = sum(mydata$n_paired_c[which(mydata$device == 1)])
totalPairedAndMatched = sum(mydata$n_paired_c_timematch[which(mydata$device == 1)])
proportionTimestamp = totalPairedAndMatched/totalPaired
print(paste0("Proportion of counterpart pairs with matching intake timestamp: ", proportionTimestamp))





dataDir="/Volumes/food/pilot/dev/release_candidate/data/"

source('getSample.R')
sampleData = getSample('usage')

redcapData = read.table(paste0(dataDir,'/original/CollectingFoodAndDri_DATA_2022-01-08_1329.csv'), sep=',', header=1, stringsAsFactors=FALSE)


###
### summarise q1 data for usage sample - basic demographics

# q1 data
registered = redcapData[which(redcapData$q1_pid!="" & redcapData$record_id %in% sampleData$record_id),]



# Sex
print("Sex")
print("1: male, 2: female")
nsex = table(registered$sex)
print(nsex)
prop.table(nsex)

# Age bands
print("Age")
print("1: <20, 2: 20-29, 3:30-39, 4:40-49, 5:50-59, 6:60+, 7:Prefer not to answer")
nage = table(registered$age_band)
print(nage)
prop.table(nage)

# Do you consider yourself to have a strong UK regional accent?
print("Accent UK")
print("1: Yes, 2: a little, 3: no, 4: prefer not to answer")
naccentReg = table(registered$accent_regional)
print(naccentReg)
prop.table(naccentReg)

# Do you consider yourself to have a strong accent due to English being a second language?
print("Accent second language")
print("1: Yes, 2: a little, 3: no, 4: prefer not to answer")
naccentSec = table(registered$accent_second)
print(naccentSec)
prop.table(naccentSec)

#  Do you currently have a voice-controlled device (e.g Alexa) at home? 
print("Have device")
print("1: yes and I use it, 2: Yes but it's used by others, not me, 3: No")
nalexaHome = table(registered$alexa_home)
print(nalexaHome)
prop.table(nalexaHome)





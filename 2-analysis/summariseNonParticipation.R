

dataDir="/Volumes/food/pilot/dev/release_candidate/data/"


###
### load data

redcapData = read.table(paste0(dataDir,'/original/CollectingFoodAndDri_DATA_2022-03-07_1227.csv'), sep=',', header=1, stringsAsFactors=FALSE)
print(dim(redcapData))


###
### get non participation sample

# q1 data
registered = redcapData[which(redcapData$participant == 0 & redcapData$q1registration_timestamp != "[not completed]"),]

print(dim(registered))


# one participant removed because they did take part in the alexa study just couldn't set it up at home
# one test participant also removed
removeRecordIds = read.csv(paste0(dataDir, 'original/non-participation-removals.txt'))
ixRem = which(registered$record_id %in% removeRecordIds$recordID)
registered = registered[-ixRem,]

print(dim(registered))


###
### summarise non participation questionnaire responses

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

#  Do you currently have a voice-controlled device (e.g Alexa) at home? 
print("Have device")
print("1: yes and I use it, 2: Yes but it's used by others, not me, 3: No")
nalexaHome = table(registered$alexa_home)
print(nalexaHome)
prop.table(nalexaHome)



# reasons did not take part - a - g

print('a: Not available during the study session times')
n = table(registered$non_reasons___a)
print(n)
prop.table(n)

print('b: Data privacy concerns around Amazon collecting information on my diet')
n = table(registered$non_reasons___b)
print(n)
prop.table(n)

print('c: Data privacy concerns around researchers collecting information on my diet')
n = table(registered$non_reasons___c)
print(n)
prop.table(n)

print('d: Concerns that Alexa will inadvertently listen to other conversations')
n = table(registered$non_reasons___d)
print(n)
prop.table(n)

print('e: I don\'t eat or drink during my working hours')
n = table(registered$non_reasons___e)
print(n)
prop.table(n)

print('f: Picking up and returning the device was inconvenient')
n = table(registered$non_reasons___f)
print(n)
prop.table(n)

print('g: other')
n = table(registered$non_reasons___g)
print(n)
prop.table(n)



print('Number of participants that had one of the three privacy concerns')
ix = which(registered$non_reasons___b==1 | registered$non_reasons___c==1 | registered$non_reasons___d==1)
print(length(ix))



# reasons did not take part - other

ix = which(registered$non_reasons_other!="")
print(registered$non_reasons_other[ix])
#print(registered[ix,c('non_reasons_other', 'record_id')])



# In the future, would you be happy to use a voice-controlled system

print("Alexa happy to use at home")
print("yes: 1, no: 2, not sure: 3")
n = table(registered$alexa_happy_at_home)
print(n)
prop.table(n)

# not happy reasons

print('a: Data privacy concerns around researchers collecting information about me')
n = table(registered$alexa_nothappy_reasons___a)
print(n)
prop.table(n)

print('b:  Data privacy concerns around Amazon collecting information about me.')
n = table(registered$alexa_nothappy_reasons___b)
print(n)
prop.table(n)

print('c: Concerns that Alexa will inadvertently listen to other conversations, which may then be accessed by researchers')
n = table(registered$alexa_nothappy_reasons___c)
print(n)
prop.table(n)

print('d: Concerns that the system will inadvertently listen to other conversations, which may then be used by the company that owns it (e.g. Amazon).')
n = table(registered$alexa_nothappy_reasons___d)
print(n)
prop.table(n)

print('e: other')
n = table(registered$alexa_nothappy_reasons___e)
print(n)
prop.table(n)


# other reasons

ix = which(registered$nothap_hom_reas_oth!="")
print(registered$nothap_hom_reas_oth[ix])


# Would you be happy to use a voice-controlled system (e.g. Alexa) on a wearable device such as a smart watch, for research

print("happy to use alexa on wearable")
print("yes: 1, no: 2, not sure: 3")
n = table(registered$alexa_happy_wearable)
print(n)
prop.table(n)

# not happy reasons

print('a: Data privacy concerns around researchers collecting information about me.')
n = table(registered$alexa_nothap_wear_reas___a)
print(n)
prop.table(n)

print('b: Data privacy concerns around Amazon collecting information about me.')
n = table(registered$alexa_nothap_wear_reas___b)
print(n)
prop.table(n)

print('c: Concerns that Alexa will inadvertently listen to other conversations, which may then be accessed by researchers.')
n = table(registered$alexa_nothap_wear_reas___c)
print(n)
prop.table(n)

print('d: Concerns that the system will inadvertently listen to other conversations, which may then be used by the company that owns it (e.g. Amazon).')
n = table(registered$alexa_nothap_wear_reas___d)
print(n)
prop.table(n)

print('e: I wouldn\'t like the idea of wearing a device all the time because of how it looks and / or feels.')
n = table(registered$alexa_nothap_wear_reas___e)
print(n)
prop.table(n)

print('f: other')
n = table(registered$alexa_nothap_wear_reas___f)
print(n)
prop.table(n)


# other reasons

ix = which(registered$nothapp_wear_other!="")
if (length(ix)>0) {
  print(registered$nothapp_wear_other[ix])
} else {
  print('no other reasons for wearable')
}

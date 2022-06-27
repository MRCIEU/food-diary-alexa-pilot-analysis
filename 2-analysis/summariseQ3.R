

dataDir="/Volumes/food/pilot/dev/release_candidate/data/"


###
### load data

redcapData = read.table(paste0(dataDir,'/original/CollectingFoodAndDri_DATA_2022-03-07_1227.csv'), sep=',', header=1, stringsAsFactors=FALSE)
print(dim(redcapData))


###
### get post participation questionnaire data

# q3 data
q3 = redcapData[which(redcapData$q3participation_complete == 2),]
print(q3$record_id)

print(dim(q3))

source('getSample.R')
sampleData = getSample('usage')
print(sampleData$record_id)

ix = which(q3$record_id %in% sampleData$record_id)
q3 = q3[ix,]

print(dim(q3))




# one participant removed because they did take part in the alexa study just couldn't set it up at home
removeRecordIds = read.csv(paste0(dataDir, 'original/non-participation-removals.txt'))
ixRem = which(q3$record_id %in% removeRecordIds$recordID)
if (length(ixRem)>0) {
  q3 = q3[-ixRem,]
}


###
### summarise non participation questionnaire responses


# reasons did take part

print('reasons did take part')

print('a) Interested in nutritional research')
n = table(q3$q2_reasons_part___a)
print(n)
prop.table(n)

print('b) I wanted to support research.')
n = table(q3$q2_reasons_part___b)
print(n)
prop.table(n)

print('c) I wanted to support others in the IEU.')
n = table(q3$q2_reasons_part___c)
print(n)
prop.table(n)

print('d) I feel I should be a research participant when asked, as I benefit from other participants.')
n = table(q3$q2_reasons_part___d)
print(n)
prop.table(n)

print('e) I wanted to try a voice controlled system.')
n = table(q3$q2_reasons_part___e)
print(n)
prop.table(n)

print('f) I wanted to try Alexa.')
n = table(q3$q2_reasons_part___f)
print(n)
prop.table(n)

print('g) I wanted to see how well Alexa could interact with me.')
n = table(q3$q2_reasons_part___g)
print(n)
prop.table(n)

print('h) I wanted to see how well Alexa could record my dietary information.')
n = table(q3$q2_reasons_part___h)
print(n)
prop.table(n)

print('i: other')
n = table(q3$q2_reasons_part___i)
print(n)
prop.table(n)


# reasons did take part - other

print('other:')
ix = which(q3$q2_reasons_other!="")
print(q3$q2_reasons_other[ix])
#print(q3[ix,c('q2_reasons_other', 'record_id')])



# experience using alexa

print('I was able to accurately tell Alexa what I ate or drank.')
print('1:completely agree;2:somewhat agree;3:neutral;4:disagree somewhat;5:completely disagree')
n = table(q3$q2_exp_a)
print(n)
prop.table(n)

print('I was able to estimate accurate quantities describing how much I ate.')
print('1:completely agree;2:somewhat agree;3:neutral;4:disagree somewhat;5:completely disagree')
n = table(q3$q2_exp_b)
print(n)
prop.table(n)

print('I chose not to record particular snacks/meals (e.g. because it was unhealthy).')
print('1:completely agree;2:somewhat agree;3:neutral;4:disagree somewhat;5:completely disagree')
n = table(q3$q2_exp_c)
print(n)
prop.table(n)

print('Sometimes I chose to be selective with the truth.')
print('1:completely agree;2:somewhat agree;3:neutral;4:disagree somewhat;5:completely disagree')
n = table(q3$q2_exp_d)
print(n)
prop.table(n)

print('I remembered to submit my food/drink information.')
print('1:completely agree;2:somewhat agree;3:neutral;4:disagree somewhat;5:completely disagree')
n = table(q3$q2_exp_e)
print(n)
prop.table(n)


## Other comments about your experience supplying your dietary information.

print('other:')
ix = which(q3$q2_experience_other!="")
print(q3$q2_experience_other[ix])


####
#### how often the following things happened 

print('Alexa interjected when I hadn\'t finished telling her when I ate or drank.')
print('1:never;2:rarely;3:occasionally;4:often;5:always')
n = table(q3$q2_oft_a)
print(n)
prop.table(n)

print('Alexa interjected when I hadn\'t finished telling her what I ate or drank.')
print('1:never;2:rarely;3:occasionally;4:often;5:always')
n = table(q3$q2_oft_b)
print(n)
prop.table(n)



####
#### other comments

print('other:')
ix = which(q3$q2_often_other!="")
print(q3$q2_often_other[ix])

####
#### aspects of alexa use

print('How convenient or inconvenient did you find providing information using Alexa?')
print('a. Very inconvenient;2:b. Somewhat inconvenient;3:c. OK;4:d. Somewhat convenient;5:e. Very convenient')
n = table(q3$q2_convenient)
print(n)
prop.table(n)

print('How enjoyable or unenjoyable did you find providing information using Alexa?')
print('1:very unenjoyable .... 5: very enjoyable')
n = table(q3$q2_enjoy)
print(n)
prop.table(n)

print('How efficient or inefficient did you find providing information using Alexa?')
print('1:very inefficient .... 5: very efficient')
n = table(q3$q2_efficient)
print(n)
prop.table(n)

print('How easy or hard did you find providing information using Alexa?')
print('1:very easy .... 5: very hard;6: could not use at all')
n = table(q3$q2_easy)
print(n)
prop.table(n)


####
#### concerns taking part

print('Did you have any concerns about taking part in this study (please select all that apply)?')

print('a) Data privacy concerns around Amazon collecting information on my diet.')
n = table(q3$q2_concerns___a)
print(n)
prop.table(n)

print('b) Data privacy concerns around researchers collecting information on my diet.')
n = table(q3$q2_concerns___b)
print(n)
prop.table(n)

print('c) Concerns that Alexa will inadvertently listen to other conversations.')
n = table(q3$q2_concerns___c)
print(n)
prop.table(n)

print('d) other')
n = table(q3$q2_concerns___d)
print(n)
prop.table(n)


## other concerns

print('other:')
ix = which(q3$q2_concerns_other!="")
print(q3$q2_concerns_other[ix])






###
### summarise other food diary methods they have used


#if (FALSE) {
print('used other method to collect diet data')

ix = which(!is.na(q3$q2_method1_type) & q3$q2_method1_type!="")

print(paste0('Number of participants who have previously used another food diary method: ', length(ix)))

for (i in ix) {

  print(paste0('####### participant record id ',q3$record_id[i],' #######'))
  print(q3$q2_method1_type[i])
  print(q3$method1_details[i])
  print(paste('convenience:', q3$method1_convenience[i]))
  print(paste('enjoy:',q3$q2_method1_enjoy[i]))
  print(paste('efficient:',q3$q2_method1_efficient[i]))

  if (!is.na(q3$q2_method2_type[ix[i]]) & q3$q2_method2_type[i]!="") {
    print(paste0('####### participant record id ',q3$record_id[i],' #######'))
    print(q3$q2_method2_type[i])
    print(q3$method2_details[i])
    print(paste('convenience:', q3$method2_convenience[i]))
    print(paste('enjoy:',q3$q2_method2_enjoy[i]))
    print(paste('efficient:',q3$q2_method2_efficient[i]))
  }

  if (!is.na(q3$q2_method3_type[ix[i]]) & q3$q2_method3_type[i]!="") {
    print(paste0('####### participant record id ',q3$record_id[i],' #######'))
    print(q3$q2_method3_type[i])
    print(q3$method3_details[i])
    print(paste('convenience:', q3$method3_convenience[i]))
    print(paste('enjoy:',q3$q2_method3_enjoy[i]))
    print(paste('efficient:',q3$q2_method3_efficient[i]))
  }
}


#stop()
#}










# In the future, would you be happy to use a voice-controlled system

print('happy to use at home')
print("yes: 1, no: 2, not sure: 3")
n = table(q3$q2_alexa_happy_at_home)
print(n)
prop.table(n)

# not happy reasons

print('a: Data privacy concerns around researchers collecting information about me')
n = table(q3$q2_nothappy_reasons___a)
print(n)
prop.table(n)

print('b:  Data privacy concerns around Amazon collecting information about me.')
n = table(q3$q2_nothappy_reasons___b)
print(n)
prop.table(n)

print('c: Concerns that Alexa will inadvertently listen to other conversations, which may then be accessed by researchers')
n = table(q3$q2_nothappy_reasons___c)
print(n)
prop.table(n)

print('d: Concerns that the system will inadvertently listen to other conversations, which may then be used by the company that owns it (e.g. Amazon).')
n = table(q3$q2_nothappy_reasons___d)
print(n)
prop.table(n)

print('e: other')
n = table(q3$q2_nothappy_reasons___e)
print(n)
prop.table(n)


# other reasons

print('other:')
ix = which(q3$q2_nothap_hom_reas_oth!="")
print(q3$q2_nothap_hom_reas_oth[ix])


# Would you be happy to use a voice-controlled system (e.g. Alexa) on a wearable device such as a smart watch, for research

print('happy wearable')
print("yes: 1, no: 2, not sure: 3")
n = table(q3$q2_alexa_happy_wearable)
print(n)
prop.table(n)

# not happy reasons

print('a: Data privacy concerns around researchers collecting information about me.')
n = table(q3$q2_alexa_nothap_wear_reas___a)
print(n)
prop.table(n)

print('b: Data privacy concerns around Amazon collecting information about me.')
n = table(q3$q2_alexa_nothap_wear_reas___b)
print(n)
prop.table(n)

print('c: Concerns that Alexa will inadvertently listen to other conversations, which may then be accessed by researchers.')
n = table(q3$q2_alexa_nothap_wear_reas___c)
print(n)
prop.table(n)

print('d: Concerns that the system will inadvertently listen to other conversations, which may then be used by the company that owns it (e.g. Amazon).')
n = table(q3$q2_alexa_nothap_wear_reas___d)
print(n)
prop.table(n)

print('e: I wouldn\'t like the idea of wearing a device all the time because of how it looks and / or feels.')
n = table(q3$q2_alexa_nothap_wear_reas___e)
print(n)
prop.table(n)

print('f: other')
n = table(q3$q2_alexa_nothap_wear_reas___f)
print(n)
prop.table(n)


# other reasons

print('other:')
ix = which(q3$q2_nothapp_wear_other!="")
print(q3$q2_nothapp_wear_other[ix])





print('other combined:')
ix = which(q3$q2_experience_other!="" | q3$q2_often_other!="")
#print(q3[ix,c('q2_experience_other', 'q2_often_other')])

for (i in 1:length(ix)) {

  ixThis = ix[i]
  print('xxxxx')
  print(q3$q2_experience_other[ixThis])
  print(q3$q2_often_other[ixThis])

}

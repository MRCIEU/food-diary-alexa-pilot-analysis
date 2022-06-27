
library("xlsx")

dataDir="/Volumes/food/pilot/dev/release_candidate/data/"


##
## get all completed assessments

completeDir = paste0(dataDir, '/derived/manual/food_diary_review/samples/completed')
files = list.files(completeDir, '.*xlsx', full.names=TRUE)
print(files)

##
## load my assessments

myassess = read.xlsx(paste0(dataDir, '/derived/manual/food_diary_review/entries_to_compare_lacm-v3.xlsx'), sheetIndex=1)
myassess$indep = FALSE


##
## 

source('getAssessment.R')

resAll = getAssessment(dataDir, files[1])

if (length(files)>1) {
  for (i in 2:length(files)) {

    res = getAssessment(dataDir, files[i])

    resAll = rbind(resAll, res)
  }
}


print(dim(resAll))



resAll$to_misspelt_count = NULL
resAll$notes = NULL
resAll$"working_out...notes" = NULL 
resAll$"NA." = NULL
resAll$Calculated.Alexa.Total  = NULL 
resAll$Alexa.totals.match  = NULL 
resAll$Calculated.Web.Total = NULL 
resAll$Web.totals.match = NULL 

# set x axis position
resAll$samplenum = as.numeric(resAll$samplenum)
resAll$idx = resAll$samplenum
resAll$idx[which(resAll$indep==TRUE)] = resAll$samplenum[which(resAll$indep==TRUE)] + 0.2




##
## plot my and their assessments side by side

library(tidyr)
dataLong = gather(resAll, type, value, same_item:total_web_items)



## remove the totals that we don't want to include in the stacked bar
dataLongParts = dataLong
dataLongParts = dataLongParts[which(! dataLongParts$type %in% c('total_alexa_items', 'total_web_items')),] 


## create an x position



## plot stacked bar
library(ggplot2)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#33FF66", "#FFCC99", "#33FCFF", "#3346FF", "#C8CBF2", "#962683")



#print(unique(data$type))

dataLongParts$type[which(dataLongParts$type=="extra_alexa_item")] = "Extra Alexa item"
dataLongParts$type[which(dataLongParts$type=="extra_alexa_item_with_major_entry_issue")] = "Extra Alexa item with major entry issue"
dataLongParts$type[which(dataLongParts$type=="extra_web_item")] = "Extra web item"
dataLongParts$type[which(dataLongParts$type=="item_with_major_alexa_entry_issue")] = "Alexa item has major entry issue"
dataLongParts$type[which(dataLongParts$type=="same_item")] = "Same item entered with Alexa and web form, with same information"
dataLongParts$type[which(dataLongParts$type=="same_item_alexa_less_detail")] = "Same item entered with Alexa and web form, with less detail in Alexa entry"
dataLongParts$type[which(dataLongParts$type=="same_item_alexa_misspelt")] = "Same item entered with Alexa and web form, Alexa has some mispelt words but still understandable"
dataLongParts$type[which(dataLongParts$type=="same_item_alexa_more_detail")] = "Same item entered with Alexa and web form, with more detail in Alexa entry"
dataLongParts$type[which(dataLongParts$type=="same_item_different_detail")] = "Same item entered with Alexa and web form, with different detail in each"
dataLongParts$type[which(dataLongParts$type=="same_item_web_misspelt")] = "Same item entered with Alexa and web form, web form has some mispelt words but still understandable"
dataLongParts$type[which(dataLongParts$type=="same_item_both_misspelt")] = "Same item entered with Alexa and web form, both have some spelling mistakes"
dataLongParts$type[which(dataLongParts$type=="item_with_alexa_entry_issue")] = "Same item entered with Alexa and web form, Alexa has some incorrect words / duplicate words / extra nonsensical words"
dataLongParts$type[which(dataLongParts$type=="same_item_alexa_more_detail_alexa_misspelt")] = "Same item entered with Alexa and web form, with more detail in Alexa entry but also some mispelt words"
dataLongParts$type[which(dataLongParts$type=="item_with_web_entry_issue")] = "Same item entered with Alexa and web form, web has some incorrect words / duplicate words / extra nonsensical words"

dataLongParts$type = as.factor(dataLongParts$type) 
dataLongParts$type = relevel(dataLongParts$type, "Extra web item") 
dataLongParts$type = relevel(dataLongParts$type, "Extra Alexa item with major entry issue") 
dataLongParts$type = relevel(dataLongParts$type, "Extra Alexa item") 



(unique(dataLongParts$type))

# set x axis position
#dataLongParts$idx = dataLongParts$samplenum
#dataLongParts$idx[which(dataLongParts$indep==TRUE)] = dataLongParts$samplenum[which(dataLongParts$indep==TRUE)] + 0.2


ggplot() +
geom_bar(data=dataLongParts, aes(fill=type, y=value, x=idx), position="stack", stat="identity") +
theme(legend.position="bottom", legend.direction='vertical') +
xlab('Samples') +
scale_x_continuous(breaks=c(1,1.2,2,2.2,3,3.2,4,4.2,5,5.2), labels=c('LACM','Assessor 1','LACM','Assessor 2','LACM','Assessor 3','LACM','Assessor 4','LACM', 'Assessor 5')) +
theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  scale_fill_manual(values=cbPalette, name='Item classification')

resdir='/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/'
ggsave(paste0(resdir, 'manual-comparison-assessors-stackedbar.pdf'), width=8)





resdir='/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/'

## read in data

library(openxlsx)
datadir = '/Volumes/food/pilot/dev/release_candidate/data/derived/manual/food_diary_review/'
x = read.xlsx(paste0(datadir, 'entries_to_compare_lacm-v3.xlsx'))


## how many with a value in the different_eating_occurence column.
## this is the number we have reviewed so far


ix = which(!is.na(x$different_eating_occurrence))
print(paste0('number with review: ', length(ix)))


## how many that we don't evaluate because they are most likely
## not the same eating occurence

ix = which(x$different_eating_occurrence==1)
print(paste0('number with different eating occurence: ', length(ix)))


## restrict to the those for same eating occurence
ix = which(x$different_eating_occurrence==0)
x = x[ix,]



## calculate the number of misspellings in alexa and the number of those that are 'two' vs 'to'


print(paste0('Num to mispelt: ', sum(x$to_misspelt_count, na.rm=TRUE)))
print(paste0('Num more detail + mispelt: ', sum(x$same_item_alexa_more_detail_alexa_misspelt, na.rm=TRUE)))
print(paste0('Num mispelt:', sum(x$same_item_alexa_misspelt, na.rm=TRUE)))


totalMispelt = sum(x$same_item_alexa_more_detail_alexa_misspelt, na.rm=TRUE) + sum(x$same_item_alexa_misspelt, na.rm=TRUE)
print(paste0('Total mispelt: ', totalMispelt))



## get the total numbers across all participants for each column

colnames = colnames(x)[6:21]
for (colIdx in 6:22) {
  print(colnames(x)[colIdx])

  total = sum(x[,colIdx], na.rm=TRUE)
  print(total)
}




# remove column not used in plots
x$to_misspelt_count = NULL




## calculate the number of same items and total web items for each participant


#head(x)

colsToAggregate = colnames(x)[-which(colnames(x)%in%c('recordID', 'foodEntryID', 'webEntry', 'alexaEntry', 'notes'))]
xAgg = aggregate(x[,colsToAggregate], by=list(recordID=x$recordID), sum, na.rm=TRUE)


# order the participants by the total number of web items
xidx = sort(xAgg$total_web_items, index.return=TRUE, decreasing=TRUE)
xAgg$idx[xidx$ix] = 1:length(xidx$ix)





## plot manual comparison
## stacked bars for each participant

library(tidyr)
#dataLong = gather(xAgg, type, value, same_item_different_detail:total_web_items)
dataLong = gather(xAgg, type, value, same_item:total_web_items)



## remove the totals that we don't want to include in the stacked bar
dataLongParts = dataLong
dataLongParts = dataLongParts[which(! dataLongParts$type %in% c('total_alexa_items', 'total_web_items')),] 



## plot stacked bar
library(ggplot2)

ggplot() +
geom_bar(data=dataLongParts, aes(fill=type, y=value, x=recordID), position="stack", stat="identity") +
geom_point(data=xAgg,aes(x=recordID, y=total_web_items)) +
theme(legend.position="bottom", legend.direction='vertical')
ggsave(paste0(resdir, 'manual-comparison-stackedbar-with-recordIDs.pdf'))

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


ggplot() +
geom_bar(data=dataLongParts, aes(fill=type, y=value, x=idx), position="stack", stat="identity") +
#geom_point(data=xAgg,aes(x=idx, y=total_web_items)) +
theme(legend.position="bottom", legend.direction='vertical') +
xlab('Participants') +
theme(axis.ticks.x = element_blank(), axis.text.x=element_blank()) +
  scale_fill_manual(values=cbPalette, name='Item classification')


ggsave(paste0(resdir, 'manual-comparison-stackedbar.pdf'), width=8)


dataDir="/Volumes/food/pilot/dev/release_candidate/data/"
resdir='/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/'

library(ggplot2)
library(pluralize)

resAll = read.table(paste0(dataDir, '/derived/auto-comparison.csv'), sep=',', header=1)

##
## reorder the participants in decreasing order of the number of entries

resAll$xx = 1
xx = aggregate(xx ~recordID, data=resAll, FUN="sum")
colnames(xx)[which(colnames(xx) == 'xx')] = 'numEntries'
resAll = merge(resAll, xx, all.x = TRUE, all.y = FALSE, sort=FALSE)
resAll = resAll[sort(resAll$numEntries, index.return = TRUE, decreasing = TRUE)$ix,]

# make a new idx column, for the x position we want each bar on the plot
resAll$idx = 0
idxThis = 1
currRecordID = resAll$recordID[1]
for (i in 1:nrow(resAll)) {

  if (resAll$recordID[i] != currRecordID) {
    # increment by 1 extra to make a gap between bars of each participant
    idxThis = idxThis+1
    currRecordID = resAll$recordID[i]
    
  }
  resAll$idx[i] = idxThis

  idxThis = idxThis+1

}

# get the idx that we should split on to get two plots
recordIDSplit = resAll$recordID[floor(nrow(resAll)/2)]
idxSplit = resAll$idx[max(which(resAll$recordID==recordIDSplit))]

###
### plot stacked bar


# convert to long DF - one line for value and one for total, for each entry for each participant
library(tidyr)
dataLong = gather(resAll, type, value, same:alexaonly)

cbPalette <- c("#999999", "#56B4E9", "#009E73")


dataLong$type[which(dataLong$type=="same")] = "Word in both web and Alexa entry"
dataLong$type[which(dataLong$type=="webonly")] = "Word in web entry only"
dataLong$type[which(dataLong$type=="alexaonly")] = "Word in Alexa entry only"


# plot for first half
ggplot() +
 geom_bar(data=dataLong[which(dataLong$idx<=idxSplit),], aes(fill=type, y=value, x=idx), position="stack", stat="identity") +
 theme(legend.position="bottom", legend.direction='vertical') +
 xlab('Participants') +
 theme(axis.ticks.x = element_blank(), axis.text.x=element_blank()) +
 scale_fill_manual(values=cbPalette, name='Item classification') + ylab('Number of words') + ylim(0,40) + xlim(0, idxSplit+1)
  ggsave(paste0(resdir, 'bow-comparison-stackedbar1.pdf'), width=8, height=3.5)



# we split into two plots so start the idx at 1 for the first record in the second plot
dataLong$idx2 = dataLong$idx - idxSplit

x = dataLong[which(dataLong$idx>idxSplit),c("value", "idx2", "type")]

warnings()

# plot for second half
ggplot() +
 geom_bar(data=x, aes(fill=type, y=value, x=idx2), position="stack", stat="identity") +
 theme(legend.position="bottom", legend.direction='vertical') +
 xlab('Participants') +
 theme(axis.ticks.x = element_blank(), axis.text.x=element_blank()) +
 scale_fill_manual(values=cbPalette, name='Item classification') + ylab('Number of words') + ylim(0,40) + xlim(0, max(dataLong$idx2)+1)
ggsave(paste0(resdir, 'bow-comparison-stackedbar2.pdf'), width=8, height=3.5)




###
### time plot to compare intake time of web form vs alexa

# get time as minute in day
resAll$timeWeb = strptime(resAll$timeWeb, format='%Y-%m-%d %H:%M:%S')
resAll$timeWebTMins = resAll[,'timeWeb']$min + resAll[,'timeWeb']$hour*60

# convert to hours
resAll$timeDiff = resAll$timeDiff/60

# add 1 day so that we can use log scale on y axis (i.e. no negative values)
resAll$timeDiff = resAll$timeDiff + 1440

# make sure no zeros
#resAll$timeDiff = resAll$timeDiff + 0.0001

# set y axis breaks to whole day values (offset by 1440 for hack above)
scaleYbreaks = c(-1440+0.0001, 0, 1440, 6*1440) + 1440

# convert to factor so can be used to decide shape and color
resAll$recordID = as.factor(resAll$recordID)

ggplot() +
 geom_point(data=resAll,mapping=aes(x=timeWebTMins, y=timeDiff, shape=recordID, color=recordID)) +
 scale_shape_manual(values=c(seq(0,25), seq(0, length(unique(resAll$recordID))-26))) +
 xlab('Intake time entered on web form') + 
 ylab('Intake time difference (Alexa-web) in days') +
 scale_x_continuous(breaks=seq(0, 1440, 360), labels=c("00:00", "06:00", "12:00", "18:00", "00:00")) +
 ylim(c(-1, 7*1440)) +
 guides(color = FALSE, shape = FALSE) +
 scale_y_continuous(breaks=scaleYbreaks, labels=c("-1", "0", "1", "6"), trans="log10")
ggsave(paste0(resdir, 'time-comparison.pdf'), width=8, height=3.5)




warnings()







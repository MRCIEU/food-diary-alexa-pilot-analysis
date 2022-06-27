

#dataDir=Sys.getenv('PROJECT_DATA')
dataDir="/Volumes/food/pilot/dev/release_candidate/data/"

source('getSample.R')
sampleData = getSample('usage')



###
### summarise number of food diary entries for usage sample

summary(sampleData)


print("Number of completed web submissions:")
print(paste(mean(sampleData$n_web), sd(sampleData$n_web)))

print("Number of completed alexa submissions:")
print(paste(mean(sampleData$n_alexa_c), sd(sampleData$n_alexa_c)))

print("Number of partial alexa submissions:")
print(paste(mean(sampleData$n_alexa_p), sd(sampleData$n_alexa_p)))

sampleData$diaryDiff = sampleData$n_web - sampleData$n_alexa_c
print("Diff between web and alexa submissions:")
print(paste(mean(sampleData$diaryDiff), sd(sampleData$diaryDiff)))


# paired t test of number of entries in web versus alexa
t.test(sampleData$n_web, sampleData$n_alexa_c, paired=TRUE)


pdf("/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/hist_n_web.pdf")
hist(sampleData$n_web)
dev.off()

pdf("/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/hist_n_alexa_c.pdf")
hist(sampleData$n_alexa_c)
dev.off()


# histogram of the number of partial entries for each participant
pdf("/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/hist_n_alexa_p.pdf")
hist(sampleData$n_alexa_p, xlab="Number of partial Alexa entries", main="")
dev.off()

pdf("/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/hist_n_diff.pdf")
hist(sampleData$diaryDiff)
dev.off()

# number of entries via web form versus alexa
pdf("/Users/lm0423/OneDrive\ -\ University\ of\ Bristol/my-projects/2021-alexa/results/scatter_n_diaries.pdf")
plot(sampleData$n_web, sampleData$n_alexa_c, xlab="Number of web entries", ylab="Number of Alexa entries", pch=18)
lines(x=c(0,50), y=c(0,50))
dev.off()



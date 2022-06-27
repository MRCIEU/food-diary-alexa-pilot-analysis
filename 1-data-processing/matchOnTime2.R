
##
## match on intake date/time for all candidate matches (where alexa entry was submitted up to 30 mins before web entry)

matchOnTime2 <- function(foodevents, alexaEventsComplete, alexaElementsComplete, matches) {

  ## find all non NA indexes

  ## for each of these matches, find those with same intake date / time

  for (i in 1:nrow(foodevents)) {

    if (foodevents$foodform_date_time[i] == "") {
      next
    }

    # find alexa entry indexes where they are a candidate match (i.e. submitted soon before) and match on intake time
    ixTimeMatched =  which(!is.na(matches[i,]) & difftime(alexaEventsComplete$intake_date_time, foodevents$foodform_date_time[i], units="mins") >=-5 & difftime(alexaEventsComplete$intake_date_time, foodevents$foodform_date_time[i], units="mins")<=5)

    # append T to those matched on intake time
    matches[i, ixTimeMatched] = paste0(matches[i, ixTimeMatched], "T")
  }

  return(matches)

}




matchNearest <- function(foodevents, alexaEventsComplete, alexaElementsComplete, matches) {


  ## for each of these matches, find those with same intake date / time
  ## for each partial alexa event with a match, assign to the web event submitted soonest after this partial entry was recorded

  for (i in 1:nrow(alexaEventsComplete)) {


    ## we only need to match the alexa events that haven't been matched on time
    ## but that do have some web events submitted soon after

    # find indexes for web events near the alexa entry and matched on intake time (CT/PT) or just near the alexa entry (C/P)
    if (alexaEventsComplete$partial[i] == 0) {
      ix = which(matches[,i] == "C")
      ixPT = which(matches[,i] == "CT")
    } else {
      ix = which(matches[,i] == "P")
      ixPT = which(matches[,i] == "PT")
    }


    ## if partial alexa entry has not been matched on intake time
    ## assign it to the next submitted web event

    if (length(ix)>0 & length(ixPT) == 0) {

      nearestIx = min(ix)

      # append N to those matched on intake time
      matches[nearestIx, i] = paste0(matches[nearestIx, i], "N")
    }
  }


  return(matches)

}


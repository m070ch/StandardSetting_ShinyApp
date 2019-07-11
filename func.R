#Helper file with functions for standard setting app

# Function to assign difficulty rating to bookmark
# take the average between the difficulty of the item that the bookmark 
# corresponds to and the difficulty of the item just after it in the booklet
placeBookmark <- function(bookletNum, difficulties, judgements){
  # remove difficulty from judgements before refreshing them
  judgements <- subset(judgements, select = c(Judge, Bookmark))
  # get only difficulties from booklet we're working on
  difficulties <- subset(difficulties, Booklet == bookletNum)
  # get the max index for the booklet
  bookletMax <- max(difficulties$Index)
  # make field thats just booklet index+1
  judgements$BookPlus <- ifelse(judgements$Bookmark+1 > bookletMax, 
                                judgements$Bookmark, judgements$Bookmark+1)
  # get difficulty for booklet index
  judgements <- merge(judgements, difficulties[,c("Index", "Difficulty")], 
                      by.x = "Bookmark", 
                      by.y = "Index", 
                      all.x = TRUE)
  # get difficulty for booklet index+1
  judgements <- merge(judgements, difficulties[,c("Index", "Difficulty")], 
                      by.x = "BookPlus", 
                      by.y = "Index", 
                      all.x = TRUE)
  # in case of negative or 0 bookmark placement, makes difficulty of booklet index = 
  #   difficulty of booklet index+1
  judgements$Difficulty.x <- ifelse(is.na(judgements$Difficulty.x) | judgements$Bookmark == 0, 
                                    judgements$Difficulty.y, judgements$Difficulty.x)
  # take mean of difficulty of booklet index and difficulty of booklet index+1
  judgements$Difficulty <- (judgements$Difficulty.x+judgements$Difficulty.y)/2
  # return only columns we want displayed
  return(judgements[order(judgements$Judge),c("Judge", "Bookmark", "Difficulty")])
}

# Function to analyze results, calculates mean judgement and confidence
# interval around mean, and pass rate for those potential cuts and the 
# 'decision' cut from shiny app user input
analyzeResults <- function(judgements, scores){
  # take the mean judgement per judges
  agg <- with(judgements, aggregate(x = list(Difficulty = Difficulty), 
                                    by = list(Judge = Judge), 
                                    FUN = mean))
  # take the mean of judge's mean judgement
  judgementMean <- mean(agg$Difficulty)
  # calculate the standard error joudge-to-judge
  se <- round(sd(agg$Difficulty)/(sqrt(nrow(agg)-1)), digits = 2)
  # form a data frame with a confidence interval for the mean judgement
  # and a decision
  return(
    transform(
      data.frame(
        Cut = c(
          round(judgementMean-se*2, digits = 2), # lower bound
          round(judgementMean, digits = 2), # judgement mean
          round(judgementMean+se*2, digits = 2) # upper bound
        ),
        row.names = c("Lower Bound", "Mean", "Upper Bound")
        #^ add row names
      ), Pass.Rate = sapply(Cut, function(x, scores) 
        nrow(scores[which(scores$IRT_Score >= x),] )/nrow(scores), 
        scores)
         #^ calculate a pass rate based on the different cuts
    )
  )
}
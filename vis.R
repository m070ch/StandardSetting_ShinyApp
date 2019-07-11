#Helper file with visualization functions for standard setting app

# function to make the booklet line plot
bookletPlot <- function(bookletNum, difficulties, judgements){
  # use only item difficulties from proper booklet
  difficulties <- subset(difficulties, Booklet == bookletNum)
  # turn booklet number into factor so that ggplot treats it as a discrete 
  # variable
  difficulties$Booklet <- as.factor(difficulties$Booklet)
  judgements$Booklet <- as.factor(bookletNum)
  
  #add frequency of each bookmark placement
  t <- as.data.frame(table(judgements[,"Bookmark"]))
  judgements <- merge(judgements, t, by.x = "Bookmark", by.y = "Var1")
  judgements$Freq <- with(judgements, ifelse(Freq == 1, "", 
                                             as.character(judgements$Freq)))
  
  # create plot, all elements plotted along logit difficulty scale (x-axis)
  ggplot()+ 
    # difficulty of items in booklet (circles)
    geom_point(data = difficulties, mapping = aes(x = Difficulty, y = Booklet), 
               shape = 21, alpha = .75, size = 12, fill = "#4286f4")+ 
    # difficulty of judge's bookmark placements (lines)
    geom_point(data = judgements, mapping = aes(x = Difficulty, y = Booklet), 
               shape = 124, size = 17) + 
    # name x axis
    scale_x_continuous(name = "Logit Difficulty")+ 
    # frequency of bookmark placement in booklet (text above lines)
    geom_text(data = judgements, 
              mapping = aes(x = Difficulty, y = Booklet, label = Freq), 
              nudge_y = length(unique(difficulties$Booklet))/10+.05, 
              check_overlap = TRUE, show.legend = FALSE, size = 5)+ 
    # index of booklet items (text above circles)
    geom_text(data = difficulties, 
              mapping = aes(x = Difficulty, y = Booklet, label = Index), 
              nudge_y = length(unique(difficulties$Booklet))/10, 
              check_overlap = TRUE, show.legend = FALSE, size = 5)+
    # increase text size with theme function
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 20))
}

# function to make the impact histogram 
impactPlot <- function(finalJudgements, scores, decision){
  ggplot()+
    # histogram of score distribution
    geom_histogram(data = scores, mapping = aes(x = IRT_Score), stat = "bin",
                   fill = "#4286f4")+ 
    #vertical lines
    # decision argument (red)
    geom_vline(xintercept = decision, 
               size = 2, linetype = 'dashed', color = "#f4424e")+
    # lower bound of final judgements confidence interval (orange)
    geom_vline(xintercept = finalJudgements["Lower Bound", "Cut"], 
               size = 2, color = "#f48042")+
    # mean of final judgements confidence interval (orange)
    geom_vline(xintercept = finalJudgements["Mean", "Cut"], 
               size = 2, color = "#f48042")+
    # upper bound of final judgements confidence interval (orange)
    geom_vline(xintercept = finalJudgements["Upper Bound", "Cut"], 
               size = 2, color = "#f48042")+
    # increase text size with theme function
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 20)) + 
    # name x axis and set steps for axis
    scale_x_continuous(name = "Logit Test Performance", 
                       breaks = seq(from = floor(min(scores$IRT_Score)),
                                    to = ceiling(max(scores$IRT_Score)),
                                    by = .5)) 
}
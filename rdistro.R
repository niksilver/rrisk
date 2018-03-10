# Get the score for how much of our sample, x, is between the lower and upper bound.
# prob = probability (proportion) of sample that should be in the range.
# Score is the difference between the actual and the ideal.
# score = 0 means it's perfect
# score > 0 means that number too many in the range.
# score < 0 means that number too few in the range.

scoreForInterval <- function(prob, lower, upper, x) {
  expected <- prob * length(x)
  actual <- length( x[lower <= x & x <= upper] )
  actual - expected
}

# Add an interval to a matrix of intervals.
# The nth interval can be accessed by intervals[,n]
# The probabilites can be accessed by intervals["prob",]
# The number of intervals can be accessed by ncol(intervals)

# Get the scores for all the intervals against a given sample, x

scores <- function(intervals, x) {
  sapply(
    1:ncol(intervals)
    , function(i) scoreForInterval(intervals["prob", i], intervals["lower", i], intervals["upper", i], x)
  )
}

addInterval <- function(intervals, prob, lower, upper) {
  len = length(intervals)
  if (len %% 3 != 0) {
    stop(c("length of intervals matrix should be divisible by 3, but it has length ", len))
  }
  dim(intervals) <- NULL
  intervals <- c(intervals, prob, lower, upper)
  dim(intervals) <- c(3, len/3 + 1)
  rownames(intervals) <- c("prob", "lower", "upper")
  intervals
}

# Pick two different intervals. The output will be
#      [,1] [,2]
# [1,]   p1  p2
# [2,]  lo1 lo2
# [3,]  up1 up2

pickTwoIntervals <- function(intervals) {
  cols <- ncol(intervals)
  if (is.null(cols) || cols < 2) {
    stop("Not enough columns in intervals matrix")
  }
  samp <- sample(1:cols, 2, replace = FALSE)
  out <- c( intervals[,samp[1]], intervals[,samp[2]] )
  dim(out) <- c(3, 2)
  out
}

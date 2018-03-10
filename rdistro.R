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

# Redistribute the sample once, using the following algorithm:
#   - Pick the intervals with the lowest and highest score.
#   - Calculate the average of their absolute scores.
#   - Redistribute that many samples from the 'high' interval
#     to the 'low' one.

redistributeOnce <- function(intervals, x) {
  scores <- scores(intervals, x)
  cat("scores = ", scores, "\n")
  minmax <- range( scores )
  min <- minmax[1]
  max <- minmax[2]
  minIdx <- match(min, scores)
  maxIdx <- match(max, scores)
  fromProb <- intervals["prob", maxIdx]
  fromLower <- intervals["lower", maxIdx]
  fromUpper <- intervals["upper", maxIdx]
  toProb <- intervals["prob", minIdx]
  toLower <- intervals["lower", minIdx]
  toUpper <- intervals["upper", minIdx]
  countInFromRange = length( x[fromLower <= x && x <= fromUpper] )
  countToMove = (abs(max) + abs(min)) / 2
  proportionToMove = countToMove / countInFromRange
  cat("(", fromLower, ", ", fromUpper, ") -> (", toLower, ", ", toUpper, "), moving ", proportionToMove, "\n")
  redist <- function(i) {
    if (fromLower <= i && i <= fromUpper && runif(1) < proportionToMove) {
      i2 <- runif(1, toLower, toUpper)
      cat(i, " -> ", i2, "\n")
      i2
    } else {
      i
    }
  }
  if (is.finite(proportionToMove)) {
    sapply(x, redist)
  } else {
    x
  }
}

# Some initial data...

m1 <- addInterval(c(), 1.0, 0, 100)
m1 <- addInterval(m1,  0.5, 0, 30)
m1 <- addInterval(m1,  0.8, 0, 50)
m1 <- addInterval(m1,  0.5, 30, 100)

x <- runif(100, 0, 100)

# Plot the initial data...

# hist(x, plot = TRUE)

# Redistribute once and plot

# x <- redistributeOnce(m1, x); hist(x, plot = TRUE)

## What proportion of a vector is between the lower and upper bound?

proportionInRange <- function(lower, upper, x) {
  length( x[lower <= x & x <= upper] ) / length(x)
}

# Add an interval to a matrix of intervals.
# The nth interval can be accessed by intervals[,n]
# The probabilites can be accessed by intervals["prob",]
# The number of intervals can be accessed by ncol(intervals)

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

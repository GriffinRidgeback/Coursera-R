add2 <- function(x, y) {
  x + y
}

above10 <- function(x) {
  use <- x > 10 # logical vector
  x[use]
}

above <- function(x, n = 10) {
  use <- x > n # logical vector
  x[use]
}

# y is a dataframe
columnmean <- function(y, removeNA = TRUE) {
  
  # get the number of columns in our dataframe
  nc <- ncol(y)
  
  # vector to hold the mean value for each column
  means <- numeric(nc)
  
  for (i in 1:nc) {
    means[i] <- mean(y[, i], na.rm = removeNA)
  }
  
  # last expression gets returns
  means
}

cube <- function(x, n) {
  x^3
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
How to use these functions:
source the .R file
z <-makeVector(1:5)
z$get()
z$set(1:10)
z$get()
z$setmean(mean(z$get()))

then source("cachemean.R")
q <- cachemean(z, 1:100, c(1, 2, 3, 4, 5))
shows "getting cached data"
r <- makeVector(1:5)
q <- cachemean(r, 1)
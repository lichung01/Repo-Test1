makeVector <- function(x = numeric()) {
    ## Assign local m to NULL
    m <- NULL
    ##set function 
    set <- function(y) {
      ## Assign global values X and m
      x <<- y
      m <<- NULL
    }
    ## get function
    get <- function()
      x
    ## setmean function
    setmean <- function(mean) 
      m <<- mean
    ##  getmean function
    getmean <- function() 
      m
    ## 
    list(set = set, 
         get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
  ## Assign mean value of vector x to m 
  m <- x$getmean()
  ## Check if value of m is the same then return cache value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get vector and assign to data
  data <- x$get()
  ## Calculate mean value for data
  m <- mean(data, ...)
  ## Set local mean m to global mean m
  x$setmean(m)
  m
}


##  Test case for Vector
##  > b <- makeVector()
##  > b$set(c(1,2,3,4))
##  > cachemean(b)
##  [1] 2.5

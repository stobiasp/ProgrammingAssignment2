

## Function that defines all the cache of the data sets; Based off the means function provided in the example but 
#modified for what was asked. Original names not changed from the mean.

makeCacheMatrix <- function(x =matrix()) {
  

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
  }
  



##function that checks if a matrix exists and if so pulls the value and 
#if not it sets the value.

#Based off the means function provided in the example but 
#modified for what was asked. Original names not changed from the mean.

cacheSolve <- function(x, ...) {


    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    dim<-length(data)/2
    data<-matrix(data,nrow=dim,ncol=dim)
    m <- solve(data)
    x$setmean(m)
    m
  }
  
# Place valies of matrix into variable m
m<-c(1,2,9,4)

# Run makeCacheMatrix on m and results into x
x<-makeCacheMatrix(m)

# run cacheSolve for matrix inverse
cacheSolve(x)


#results

#[,1]        [,2]
#[1,] -0.2857143  0.64285714
#[2,]  0.1428571 -0.07142857





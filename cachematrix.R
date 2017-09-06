## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. In this program we are going to have two functions.
#makeCacheMatrix - caches the inverse of a given matrix
#cacheSolve - It returns the inverse of any given matrix.
#Unit testcase
#> source("cachematrix.R")
#> x<-rbind(c(-8, -3), c(24, 2))
#> x
#     [,1] [,2]
#[1,]   -8   -3
#[2,]   24    2
#> m<-makeCacheMatrix(x)
#> y<-cacheSolve(m)
#> z<-cacheSolve(m)
#getting cached data
#> identical(y,z)
#[1] TRUE
#> x%*%y
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
#>

## This function stores the inverse of a given matrix. By default the inverse value will be null. It is the responsibility of the caller to set the value to it. Once inverse object is set, this function stores it in cache for future references.
## This method creates a list containing the following functions
# 1. get - returns the matrix
# 2. set - set the matrix with empty cache
# 3. getInverse - returns the inverse for the previously set matrix if exists. Or null if doesnt exist.
# 4. setInverse - sets the inverse value for the matrix previously set.
makeCacheMatrix <- function(x = matrix()) {
inverse<-NULL
get<-function()x
set<-function(y){
  x<<-y
  inverse<<-NULL
}
getInverse<-function()inverse
setInverse<-function(inv)inverse<<-inv
list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## This function returns the inverse of any given matrix. In order to improve performance, it first checks if the inverse has already been calculated before or not, by searching it in cache. If cached object found, it is returned. If not found in cache, inverse is calculated, stored in cache for future and returned.
#It is assumed that the matrix is always inversible.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

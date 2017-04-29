## This function creates a list containing the following functions
## Set the values of a matrix
## Get the values of a matrix
## Set the inverse of a metrix
## Get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
   I <- NULL
   set <- function(y) {
       x<<-y
       I<<-NULL
   }
   get <- function() x
   setInverse <- function(inverse) I <<- inverse
   getInverse <- function () I
   list (set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## This function calculates the inverse of a matrix in the list above.
## It checks to see if the inverse already exists.  If so, it gets the inverse from the cache.
## Rf the inverse does not exist, it calculates the inverse and sets it in the cache

cacheSolve <- function(x, ...) {
        In <- x$getInverse()
        if(!is.null(In)) {
             message("getting cached data")
             return(In)
                }
        data <- x$get()
        I<-solve(data)
        x$setInverse(I)
        I
}




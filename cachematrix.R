
## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the variables used in this function
    matrizinversa <- NULL
    
    # Setter function: sets the matrix and clears the inverse cache
    set <- function(y) {
        x <<- y
        matrizinversa <<- NULL
    }
    
        # Getter function: returns the current matrix
    get <- function() x
        # Setter for the inverse: caches the inverse of the matrix
    setInversa <- function(calculoinversa) matrizinversa <<- calculoinversa
    
        # Getter for the inverse: returns the cached inverse if it exists
    getInversa <- function() matrizinversa
    
    # Return a list of the functions defined above
    list(set = set, get = get, setInversa = setInversa, getInversa = getInversa)
}


## Function to compute or retrieve the cached inverse of the matrix
cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse
    solinversa <- x$getInversa()
    
    # If the inverse is already cached, return it
    if (!is.null(solinversa)) {
        message("getting cached data")
        return(solinversa)
    }
    
    # If not cached, compute the inverse, cache it, and return it
    data <- x$get()
    solinversa <- solve(data, ...)
    x$setInversa(solinversa)
    solinversa
}

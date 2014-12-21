## The makeCacheMatrix and cacheSolve functions use the R concepts of caching
## and lexical scopping to make computation of a matrix inverse faster; the
## cacheSolve function uses the R 'solve' function

## The makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse using the <<- or superassignment operator

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL 
    
  ##Set the value of the matrix
  set <- function(y) { 
    
    x <<- y 
    m <<- NULL 
    
  }
  
  ## Get the value of the matrix
  get <- function() {
    
    x
  
  }
  
  ## Set the value of the inverse
  setinverse <- function(inverse) { 
    
    m <<- inverse 
    
  }
  
  ## Get the value of the matrix
  getinverse <- function() {
    
    m
    
  }
  
  ## Create a list housing the four functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then it should retrieve the inverse from the cache 

cacheSolve <- function (x, ...) {
  
  ## Compare matrix to what was there. If an inverse has already been calculated this gets it
  
  m <- x$getinverse()
  
  ## Check to see if cachesolve has been run before
  if(!is.null(m)){    
          
        message("getting cached data")
        return(m)             
    }
    
    ## 1. Get the value of the input matrix with the get function
    y <- x$get()
    
    ## 2. Compute the value of the inverse of the input matrix using the solve() function
    m <- solve(y, ...) 
    
    ## 3. Run the setinverse() function on the inverse to cache the inverse
    x$setinverse(m) 
    
    ## 4. Return the inverse
    m 
  
  }

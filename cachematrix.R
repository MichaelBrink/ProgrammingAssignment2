#The following 2 functions will create a matric whose inverse can be cached. A second function either computes the inverse of the matric created
#in the first function OR returns it's inverse if the inverse is already stored in the cache.

#Function 1
#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL #Create a var to store the inverse of x, assign null value (for now)
  set <- function(y) { #this function assigns the inverse variable the cached inverse value stored in  << y
    x <<- y
    inv_x <<- NULL
  }
    get <- function() { #retrieve the cached value assigned to x (from y)
    return(x)
  }
    setinv <- function(inverse) { #Set the inverse value to the cache in the var, 'inverse'
    inv_x <<-inverse
  }
    getinv <- function() {
    return(inv_x) #return the inverse value stored in the cached
  }
    list(set = set, get = get,setinv = setinv,getinv = getinv) 
}


#Function 2

  #This function computes the inverse of the special "matrix" 
  #returned by makeCacheMatrix above. If the inverse has already 
  #been calculated (and the matrix has not changed), then the cachesolve 
  #should retrieve the inverse from the cache.
  
 cacheSolve <- function(x, ...) {
  inv_x <- x$getinv() #get the cahced inverse value
    if (!is.null(inv_x)) { #if the cached value is null, then do nothing else...
  } else {
    inv_x <- solve(x$get()) #use R's solve function to compute the inverse (if x is a square, invertiable matrix)
    x$setinv(inv_x)  
  }
  return(inv_x) #Return the inverse matrix
}
    
 #Test inputs     
      mike <- diag(runif(12, 0, 1),12) # create a 12 x 12 matrix with random numbers (runif) between 0 and 1
      mike_cached <- makeCacheMatrix(mike) #cache the inverse of the matrix using makeCacheMatrix
      cacheSolve(mike_cached) # Call the cached inverse values using cacheSolve

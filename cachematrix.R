## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL #sets the value of m to NULL
  set<-function(y){ #set the value of the matrix
    x<<-y #caches the inputted matrix so that cacheSolve can check whether it has changed
    m<<-NULL #sets the value of m (the matrix inverse if used cacheSolve) to NULL
  }
  get<-function() x #get the value of the matrix
  setmatrix<-function(solve) m<<- solve  #compute the value of the inverse 
  getmatrix<-function() m #get the value of the inverse and caches it within the setmatrix function
  list(set=set, get=get, # creates a list with the four functions
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()  # if an inverse has already been calculated, this gets it
  if(!is.null(m)){ # check to see if cacheSolve has been run before
    message("getting cached data")
    return(m)
  }
  matrix<-x$get() # run the get function to get the value of the input matrix
  m<-solve(matrix, ...)  # compute the value of the inverse of the input matrix
  x$setmatrix(m) # run the setmatrix function on the input inverse matrix to cache it
  m # return the inverse
}
## cachematrix.R

## R_Programming class
## ProgrammingAssignment2 for week 3
## 21-Feb-2015
## CUAgrad74

## There are two main functions in the program: makeCacheMatrix and cacheSolve.
## The purpose of the functions is to return the inverse of a passed matrix argument.

## The inverse of the passed matrix is computed if it has not been computed 
## previously and it is then cached into another matrix object.
## If the inverse has been computed previously, it is retrieved from the cache
## instead of being computed again.


## The makeCacheMatrix function defines two matrices that are combined into
## the list cacheMatrix: origmtrx contains the originally passed matrix
## and invrtmtrx contains its inverse. invrtmtrx is initially set to NULL.
## A logical incache reflects whether the inverse of the argument matrix is
## currently cached in invrtmtrx.  The list cacheMatrix is returned.

makeCacheMatrix <- function(x = matrix()) {

  origmtrx <<- matrix()
  invrtmtrx <<- matrix()

  incache <<- FALSE
  
  origmtrx <<- x
  invrtmtrx <<- NULL
  
  cacheMatrix <<- list(original=origmtrx, inverse=invrtmtrx)
}


## The cacheSolve function returns a matrix that is the inverse of the matrix
## argument passed in the function call. If the inverse of THAT matrix has already
## been computed and is in cache, the cached matrix is returned.  If the inverse
## of the matrix is not already in cache, it is computed and assigned to cacheMatrix$inverse.
## The logical incache is set to TRUE. cacheMatrix$inverse is returned.


cacheSolve <- function(x, ...) {

  if (!identical(cacheMatrix$original, x)) {  ## These are different matrices, can't use cache
    
      message("new matrix passed, computing inverse")  
      cacheMatrix$original <<- x
      incache <<- FALSE
      }
    
  else if (incache) {
        message("using cached inverse matrix")
        return(cacheMatrix$inverse)     
    }

  cacheMatrix$inverse <<- solve(x)
  incache <<- TRUE
  
  return(cacheMatrix$inverse)
}

## End cachematrix.R
## The objective of this R script is to take a square invertible matrix value from the user,
##store this matrix value in an object with associated member functions, and compute the 
#inverse of this matrix whose value is accessible through one of the functions associated with
#the object. The special feature of this R script is the usage of the '<<-' operator which
#allows us to cache the matrix value and its associated inverse matrix encapsulated in an
#object we have created. The ability to access the cached values for a matrix and its 
#associated inverse can help speed up execution of a script which requires computation of an 
#inverse of a large number of matrices. The motivation for following this approach follows from
#the fact that computation of the inverse of a matrix is a computationally expensive procedure.

#The makeCacheMatrix function creates a makeCacheMatrix object. This object contains matrix
#data together with four functions associated with it (setmatrix, getmatrix, setmatrix_inverse,
#and getmatrix_inverse). makeCacheMatrix takes as its argument a square, invertible
#matrix and returns a list containing the functions to set the matrix, get the matrix,
#set the inverse of the matrix, and get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
#matrix_inverse set to NULL, initializing it as an object in the
  #makeCacheMatrix environment
  matrix_inverse <- NULL 
  
  #mutator function which can be used to change the matrix value using a cached value
  setmatrix <- function(y) { 
    x <<- y #assign the input argument to the x object in the parent environment
    matrix_inverse <<- NULL #assign NULL value to matrix_inverse object in parent environment
  }
  
  #accessor function allowing retrieval of the matrix
  getmatrix <- function() x 
  
  #mutator function for assigning a cached value to matrix_inverse
  setmatrix_inverse <- function(inverse) matrix_inverse <<- inverse 
  
  #accessor function for retrieving matrix_inverse
  getmatrix_inverse <- function() matrix_inverse 
  
  #line below will help us access each function associated with the makeCacheMatrix object 
  #using $ operator.Alternative is to use double square bracket operator.
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setmatrix_inverse = setmatrix_inverse,
       getmatrix_inverse = getmatrix_inverse)

}




## The cacheSolve function computes a matrix that is the inverse of the matrix argument to 
#makeCacheMatrix, and stores the value in the setmatrix_inverse function associated with the
#object. This matrix inverse value is retrievable through the getmatrix_function associated
#with the object. Argument x to the cacheSolve function must necessarily be a makeCacheMatrix
#object since the CacheSolve function is using the accessor and mutator functions associated
#with the object.
cacheSolve <- function(x, ...) {
inverse <- x$getmatrix_inverse()
  
  #if the matrix inverse has already been computed, retrieve it from the cache and skip 
  #further computation
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #get the matrix using accessor function associated with makeCacheMatrix object
  matrix_data <- x$getmatrix()
  
  #compute the inverse of the matrix matrix_data
  inverse <- solve(matrix_data, ...)
  
  #set the value of the matrix inverse in the cache 
  x$setmatrix_inverse(inverse)
  
  #return the value of the matrix inverse to the parent environment by printing this value
  inverse
}

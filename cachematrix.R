## This function takes in an argument of type matrix and it returns a list of 4 functions
## to set and get the matrix in the cache, and to set and get its inverse in the cache

makeCacheMatrix <- function(mtx = matrix()) {
  mtx_inv <- NULL                            #clear the mymatrix_inverse vector in the environment      
  
  set <- function(m) {
    mtx <<- m                                    #cache the matrix we passed in 
    mtx_inv <<- NULL                             #empty the matrix inverse cache when we set a new matrix to avoid out-of-sync  
  }
  
  get <- function() mtx                          #retrieve the initial matrix we stored in the special matrix object
  
  setinverse <- function(inv){
    mtx_inv <<- inv                              #store in the cache the inverse of the matrix
  }   
  
  getinverse <- function() mtx_inv               #gets from the cache the inverse of the matrix we stored
  
  
  list(set = set, get = get,                     #set the return list's object to the getter/setter functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes in a special matrix of type makeCacheMatrix. 
## It will check if the inverse of the matrix was already set through
## the setinverse() function and if not, it will compute it and store in the cache of the special matrix
cacheSolve <- function(x, ...) {
  
  matrix_inverse<-x$getinverse()         #get the matrix's inverse, from the cache
  
  if(is.null(matrix_inverse)){           #check if inverse of that matrix in the cache
    matrix<-x$get()                      #grab the actual matrix from the special matrix obj
    matrix_inverse<-solve(matrix)        #compute the inverse of the matrix
    x$setinverse(matrix_inverse)         #store the inverse of the matrix in cache for future
    
  }
  return (matrix_inverse)
}
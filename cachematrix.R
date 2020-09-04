## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##creating a function that enables to create a matrix and cache its inverse matrix.
##defining the argument x = matrix
makeCacheMatrix <- function(x = matrix()) {
  
    ##first initialize the inverse variable
     inverse_var <- NULL
     ##define function to set a new value of the matrix
      set<- function(y){
       x <<- y 
      ## in case of a new matrix, reset inverse variable to NULL
      inverse_var <- NULL
    }
  
   ##define another function get, to get or return the value of the matrix
   get<- function() {x}
   ## now define function to assign the value of inverse var
    set_inv_Matrix <- function(inverse) {inverse_var <<- inverse }
    ## now get the value of the inverse variable when its called
    get_inv_matrix <- function() {inverse_var  }
    list(set= set, get= get, 
       set_inv_Matrix = set_inv_Matrix, get_inv_matrix = get_inv_matrix)
}


## Write a short comment describing this function
##create a function to compute the inverse of the "matrix" returned by 
##makeCacheMatrix above.If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_var <- x$get_inv_matrix()
    ##the if statement will return the inverse var if its not =null
    if(!is.null(inverse_var)) {
        message("Inverse data has been cached ")
        return(inverse_var)
      }
     ##store x$get() function in a variable
     matrix_data <- x$get()
     ##now perform solve function on our data
      inverse_var <- solve(matrix_data, ...)
      x$set_inv_Matrix(inverse_var)
      inverse_var
}


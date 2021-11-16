## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  # First store the whole matrix in some storage space from which we can able to
  # check whether the matrix is same or not. In this case, we should keep things 
  # in mind that the comparison should not be costlier than the calculation of 
  # inverse. The computational complexity of comparison part is out-of-scope in
  # this implementation.
  matFile <- list()
  calVal <- list()
  checkPreset <- function(y){
    # print(matFile)
    presence <- mapply(identical, MoreArgs = list(y), matFile)
    if (any(presence)){
      idx = which(presence==TRUE)
      return (list(TRUE, calVal[idx]))
    } else{
      return (list(FALSE, FALSE))
    }
      
  }
  
  set <- function(mat, val){
    matFile <<- list(matFile, mat)
    calVal <<- list(calVal, val)
   }
  list(set = set, check = checkPreset)
}
n <- makeCacheMatrix()
n$set(A, inv(A))
n$check(inv(A))

## Write a short comment describing this function
# It will take two arguments. First argument is the matrix and the second argument 
# will be an object of makeCacheMatrix

cacheSolve <- function(x, n) {
  k <- n$check(x)
  print(k[1])
  if (k[[1]]){
    print("Using Cached value Invere of the matrix")
    print(k[[2]])
  } else {
    val = inv(x)
    print(val)
    n$set(x, val)
  }
}



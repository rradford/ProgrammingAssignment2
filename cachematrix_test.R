#
###  source("ProgrammingAssignment2/cachematrix.R")
###  source("ProgrammingAssignment2/cachematrix_test.R")
#
#



# is.identity checks if an (uncached) matrix is the identity matrix,
# up to a noise tolerance.
# Pulled from https://class.coursera.org/rprog-015/forum/thread?thread_id=442#post-2918
#
# Returns TRUE if the matrix is the identity matrix, and
# FALSE otherwise.
#
# Arguments:
# x          The matrix to test.
# tolerance  The maximum amount of allowable noise on each
#            element.
is.identity <- function(x, tolerance = 0.001) {
  if (class(x) != "matrix") {
    # it's not a matrix
    return(FALSE);
  }
  n <- nrow(x)
  if (n != ncol(x)) {
    # it's not square
    return(FALSE);
  }
  return(all(abs(x - diag(n) < tolerance)))
}

testM1 <- matrix(c(-1, -2, 1, 1), 2,2)
testM2 <- 6*diag(2)
testM3 <- matrix(c(1, 2, 3, 4), 2,2)
testM4 <- matrix(c(5, 2, -7, -3), 2,2)
testM5 <- matrix(c(1, 3, 2, 4), 2,2)
#This matrix is not invertible!
#testM5 <- matrix(c(4, 2, 2, 1), 2,2)

testCases <- list( testM1, testM2, testM3, testM4)

for( mtx in testCases){
    print(mtx)
    myCM <- makeCacheMatrix(mtx)
    myInv <- cacheSolve(myCM)
    print(myInv)
    if( !is.identity(mtx %*% myInv)) {
        stop("BAD - Not inverse")
    }
    message("Good - Inverse")
}


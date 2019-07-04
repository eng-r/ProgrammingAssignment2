## ----------------------------------------------------------------------------
## SUMMARY: 
##          Coursera: Johns Hopkins University: Data Science Specialization: 
##          R Programming: Week 03: Assignment 02
## REFS
##          1. https://www.coursera.org/learn/r-programming/peer/tNy8H/...
##             ...programming-assignment-2-lexical-scoping
##          2. https://github.com/rdpeng/ProgrammingAssignment2
##
## Author:  oseng, https://github.com/eng-r
## Contact: <public e-mail> 51923315+eng-r@users.noreply.github.com
##
## Intent:  Functions that cache the inverse of a matrix
## Date:    04-July-2019 03:56 PM
## Version: 0.1.2
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## Function creates a special "matrix" object that can cache its inverse
## ----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL # make sure the first time we call, cached value zeroed

  set <- function(y) {
          # assign values to objects in outer environment
          x <<- y       
          inverseMatrix <<- NULL   # reset context during the 'set' invocation 
  }

  get <- function() x              # return original object

  setinverse <- function(x_inv) inverseMatrix <<- x_inv
  getinverse <- function() inverseMatrix

  # create list of functions in use
  mylist <- list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
  # return the list
  mylist
}

## ----------------------------------------------------------------------------
## Function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the function retrieves the 
## inverse from the cache
##
## NOTE 1: 
## Input: x - the special CacheMatrix obtainable by makeCacheMatrix(x0) call 
##        where x0 would be a real matrix
## NOTE 2:
## re: 'and the matrix has not changed' - 'set' takes care about it 
## via NULL resetting mechanism 
## ----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  # first, give a try to get inverse as if it was previosly cached
  invx <- x$getinverse()
  if(!is.null(invx)) {
     # yeap, that worked out, we have cache 
     message("cacheSolve: yes-cache")
     # this check also worked out, so we can safely return cached inverse
     return (invx) 
  }

  # as soon as we are here, that means we were not able to retrieve the cache
  # so let's put it in 
  tmp <- x$get()
  invx <- solve(tmp, ...)
  x$setinverse(invx)

  message("cacheSolve: no-cache")  
  # Return a matrix that is the inverse of 'x'
  invx
}

## ****************************************************************************
## ****************************************************************************
##
##                       ******* HOW TO TEST *******
##
## ****************************************************************************
## ****************************************************************************

## INSTRUCTIONS 1.0. Simple verification: 
##
## run R-Studio, create/swith to a project
## cleanup by rm(list=ls())
## use: source("cachematrix.R") to load this script
## ls() to check the functions are loaded
## prepare some matrix, e.g. x<-matrix(1:4, 2,2)
## use y <- cacheSolve(makeCacheMatrix(x))
## type x to see the value 
## type y to see the value
## do y%*%x to check that it is Identity matrix with proncipal diagonal= 1 1

## INSTRUCTIONS 1.1. Simple verificaion via wrapped cacheTestSimple_01() 
## call should return TRUE, it is a basic sanity check

## INSTRUCTIONS 2.0. 
## do call cacheTestSimple_01(1), cacheTestSimple_01(2), ..., 
## cacheTestSimple_01(1000) and check every time it returns TRUE
## NOTE: may go wrong if randomization creates a non invertible square matrix
##       ---> then simply run again
## WARNING: may hangs if test size is too large

## INSTRUCTIONS 3.0. 
## do call cacheTest_03() and check messages

## ****************************************************************************
## Simple sanity check 
## ****************************************************************************
cacheTestSimple_01 <- function() {
 x <-    matrix(1:4, 2, 2)
 list <- makeCacheMatrix(x)
 y <-    cacheSolve(list)
 xyproduct <- y%*%x # matrix multiplication, expected is the Identity Matrix
 reference <- diag(2) # create the Identity Matrix to check against
 checkResult <- identical(xyproduct, reference)
 # TRUE if correct
 checkResult 
}

## ****************************************************************************
## Simple sanity check with arbitrary dim
## ****************************************************************************
cacheTestSimple_02 <- function(dim) {
 N <- dim*dim
 rndMatrix <- runif(N) # hopefully it can do a good job to create an invertible 
 x <-    matrix(rndMatrix, dim, dim)
 list <- makeCacheMatrix(x)
 y <-    cacheSolve(list)
 xyproduct <- y%*%x # matrix multiplication 
 # get rid of too small values, and not being exactly '1'
 # expected: the Identity Matrix (all 1s at principal diagonal)
 xyproduct <- round(xyproduct)   
 # create the Identity Matrix to check against
 reference <- diag(dim) 
 # do teh check now!
 checkResult <- identical(xyproduct, reference)
 # TRUE if correct
 checkResult 
}

# -------------------- Version 0.1.2 -----------------------------------------
## ****************************************************************************
## Utility functions for cacheTest_03() test
## ****************************************************************************
createTestMatrix <- function(dim) {
 N <- dim*dim
 # hopefully it can do a good job to create an invertible square matrix 
 # that always has a solution
 rndMatrix <- runif(N) 
 x <- matrix(rndMatrix, dim, dim)
 x
}

## ****************************************************************************
testTestMatrix <- function(x) {
 list <- makeCacheMatrix(x)
 y <-    cacheSolve(list)
 xyproduct <- y%*%x # matrix multiplication 
 # get rid of too small values, and not being exactly '1'
 # expected: the Identity Matrix (all 1s at principal diagonal)
 xyproduct <- round(xyproduct)   
 # create the Identity Matrix to check against
 reference <- diag(dim(x)[1]) 
 # do teh check now!
 checkResult <- identical(xyproduct, reference)
 # TRUE if correct
 checkResult 
}

## ****************************************************************************
## Check if cache was ever used 
## ****************************************************************************
cacheTest_03 <- function(dim) {
 m1 <- createTestMatrix(dim)
 m2 <- createTestMatrix(dim)  # since we use rnd, 2 matrix should be different
 if(identical(m1, m2)) {
     message("cacheTest_03: hmmm... something really weird")
 }
 else {
     message("cacheTest_03: 2 distinctive test matrix were created")
 }

 message("cacheTest_03: please check that you have 2 messages from cacheSolve() about yes-cache")
 list <- makeCacheMatrix(m1)
 y <- cacheSolve(list)
 y <- cacheSolve(list)
 y <- cacheSolve(list)

 message("cacheTest_03: please check that you have all messages from cacheSolve() about no-cache")
 list <- makeCacheMatrix(m1)
 y <- cacheSolve(list)
 list$set(m2);
 y <- cacheSolve(list)
 list$set(m1);
 y <- cacheSolve(list)

 message("cacheTest_03: please check that you have messages from cacheSolve() about no-cache 4 times and 2 times yes-cache")
 list <- makeCacheMatrix(m1)
 y <- cacheSolve(list)
 list$set(m2);
 y <- cacheSolve(list)
 list$set(m1);
 y <- cacheSolve(list)
 list$set(m1);
 y <- cacheSolve(list)

 y <- cacheSolve(list)
 y <- cacheSolve(list)
}

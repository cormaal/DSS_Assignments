# File:     cachematrix.R
# Project:  R Programming
# Author:   Alexander Cormack
# Date:     13 August 2022


# INTRODUCTION ###########################

# This second programming assignment will require you to write an R function that is able to cache
# potentially time-consuming computations. For example, taking the mean of a numeric vector is typically
# a fast operation. However, for a very long vector, it may take too long to compute the mean, especially
# if it has to be computed repeatedly (e.g. in a loop). If the contents of a vector are not changing,
# it may make sense to cache the value of the mean so that when we need it again, it can be looked up
# in the cache rather than recomputed. In this Programming Assignment you will take advantage of
# the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object. 


# ASSIGNMENT: CACHING THE INVERSE OF A MATRIX ###################################

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
# of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion
# that we will not discuss here). Your assignment is to write a pair of functions that cache
# the inverse of a matrix.

# Write the following functions:

# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#    If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve
#    the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function in R. For example,
# if x is a square invertible matrix, then solve(x) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.

# FUNCTION 1 ###################################

# The 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.
# The body of makeCacheMatrix contains four functions (getters and setters) and defines two data objects:
# x (a matrix object) and inv_x (the inverse of x)

# set() assigns the input argument to the x object in the parent environment, and assign the value of NULL
# to the inv_x object in the parent environment. This line of code clears any value of inv_x that had been cached
# by a prior execution of cacheSolve(). Therefore, if there is already a valid inverse matrix cached in inv_x,
# whenever x is reset, the value of inv_x cached in the memory of the object is cleared, forcing subsequent calls
# to cacheSolve() to recalculate the inverse matrix rather than retrieving the wrong value from cache.

# get() also takes advantage of the lexical scoping features in R. Since the symbol x is not defined within get(),
# R retrieves it from the parent environment of makeCacheMatrix().

# setInv() calls the solve() function and uses the <<- form of the assignment operator to assign the input argument
# to the value of inv_x in the parent environment.

# getInv() also takes advantage of lexical scoping to find the correct symbol inv_x to retrieve its value.

# Lastly, makeCacheMatrix creates a new object by returning a list. It is important to note that each element
# in the list is named. Naming the list elements is what allows us to use the $ form of the extract operator
# to access the functions by name.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inv_x <<- solve
        getInv <- function() inv_x
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


# FUNCTION 2 ###################################

# The 'cacheSolve' function is required to populate and/or retrieve the inverse matrix from an object
# of type makeCacheMatrix(). Like makeCacheMatrix(), cacheSolve() starts with a single argument, x,
# and an ellipsis that allows the caller to pass additional arguments into the function. Next, the function
# attempts to retrieve an inverse matrix from the object passed in as the argument. First, it calls the
# getInv() function on the input object. Then it checks to see whether the result is NULL. Since
# makeCacheMatrix() sets the cached mean to NULL whenever a new inverse matrix is set into the object,
# if the value here is not equal to NULL, we have a valid, cached inverse matrix and can return it
# to the parent environment. If the result of !is.null(inv_x) is FALSE, cacheSolve() gets the matrix
# from the input object, calculates its inverse with solve(), uses the setInv() function on the input object
# to set the inverse matrix in the input object, and then returns the inverse matrix to the
# parent environment by printing it.


cacheSolve <- function(x, ...) {
        inv_x <- x$getInv()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setInv(inv_x)
        inv_x
}

# THE SOLUTION WORKS! ###################################

# Here are some lines to show that the solution works (demonstrates repeatability).

# First I created a non-sigular square (3x3) matrix):

## > my_matrix <- matrix(c(3, 2, 7, 4, 1, 4, 4, 8, 5), 3, 3)

## > my_matrix
##      [,1] [,2] [,3]
## [1,]   3    4    4
## [2,]   2    1    8
## [3,]   7    4    5


# Then I created the special 'matrix' object with the first function:

## > cache_matrix <- makeCacheMatrix(my_matrix)


# After that I computed the inverse of the special 'matrix' with the second function:

## > cacheSolve(cache_matrix)
##              [,1]        [,2]        [,3]
## [1,] -0.252336449 -0.03738318  0.26168224
## [2,]  0.429906542 -0.12149533 -0.14953271
## [3,]  0.009345794  0.14953271 -0.04672897


# Lastly I repeated the previous step to show the solution was retrieved from cache:

## > cacheSolve(cache_matrix)
## getting cached data
##              [,1]        [,2]        [,3]
## [1,] -0.252336449 -0.03738318  0.26168224
## [2,]  0.429906542 -0.12149533 -0.14953271
## [3,]  0.009345794  0.14953271 -0.04672897
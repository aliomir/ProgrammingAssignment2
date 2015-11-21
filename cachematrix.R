##The Function will calculate the inverse of the matrix. However, if the inverse has already been calculated, 
##the function will retrieve the previously calculated inverse.


## makeCacheMatrix will store the previously input matrix and its respective inverse

makeCacheMatrix <- function(x = matrix()) { ##Creating a function called makeCacheMatrix which will have input as x(matrix)
    i<- NULL                                ##Setting an object called i which is currently empty - for storing inverses
    set<- function(y){                      ##Creating another function call set which will have input as y - this is where we can change our matrix
        x<<-y                               ##Set function will allow the input value (y) to be passed to x - i.e. change our matrix
        i<<-NULL                            ##Since we are changing the input matrix, we would want i to be null again in case it is storing an inverse of previous matrix
    }
    get<- function() {x}                    ##Creating a fucntion called get which will have no input but will give output of x - our matrix
    setinverse <- function(matrixinverse) {i <<- matrixinverse} ##creating a fucntion called setinverse which has input passes on to i
    getinverse <- function() {i}            ##getinverse is a function which will have no input but gets you the inverse
            
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ##We have created a list where each function is stored by its name
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {    ##takes input as x
        i<-x$getinverse()           ## i will be provided the value of inverse already calcualted
        if(!is.null(i)){            ## if i is not null
            message("getting cached data")  ##print the previous inverse
            return(i)
            }
            data<-x$get()           ##else get the input matrix and find its inverse and store it in i
            i<-solve(data,...)
            x$setinverse(i)         
            i
        }

## La inversa de una matriz es usualmente un c�lculo costoso y puede haber algunos beneficios de almacenar en cach� 
## la inversa de una matriz en lugar de calcularla repetidamente. Abajo est�n un par de funciones que se utilizan para 
## crear un objeto que almacena una matriz y almacena en cach�, su inversa.

## Esta funci�n crea un objeto "matriz" que puede almacenar en cach�, su inversa.
## x: una matriz cuadrada invertible
## return: una lista que contiene funciones para
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         esta lista se utiliza como entrada para cacheSolve ()


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: calcula la inversa de la matriz creada anteriormente por makeCacheMatrix. Si la inversa ha sido
## ya calculada (y la matriz no ha cambiado), recuperar� el inverso directamente del cach�.


cacheSolve <- function(x, ...) {
  ## x: salida de makeCacheMatrix ()         
  ## return: inversa de la entrada de matriz original a makeCacheMatrix ()
  inv <- x$getInverse()
  
  # si la inversa ha sido ya calculada
  if (!is.null(inv)) {
    # obtenla de la cache y salta el c�lculo
    message("obtener datos en cach�")
    return(inv)
  }
  
  # si no, calcula la inversa
  mat <- x$get()
  inv <- solve(mat, ...)
  # establece el valor de la inversa en el cach�, a trav�s de la funci�n setInverse.
  x$setInverse(inv)
  
  return(inv)
  
}

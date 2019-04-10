## La inversa de una matriz es usualmente un cálculo costoso y puede haber algunos beneficios de almacenar en caché 
## la inversa de una matriz en lugar de calcularla repetidamente. Abajo están un par de funciones que se utilizan para 
## crear un objeto que almacena una matriz y almacena en caché, su inversa.

## Esta función crea un objeto "matriz" que puede almacenar en caché, su inversa.
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
## ya calculada (y la matriz no ha cambiado), recuperará el inverso directamente del caché.


cacheSolve <- function(x, ...) {
  ## x: salida de makeCacheMatrix ()         
  ## return: inversa de la entrada de matriz original a makeCacheMatrix ()
  inv <- x$getInverse()
  
  # si la inversa ha sido ya calculada
  if (!is.null(inv)) {
    # obtenla de la cache y salta el cálculo
    message("obtener datos en caché")
    return(inv)
  }
  
  # si no, calcula la inversa
  mat <- x$get()
  inv <- solve(mat, ...)
  # establece el valor de la inversa en el caché, a través de la función setInverse.
  x$setInverse(inv)
  
  return(inv)
  
}

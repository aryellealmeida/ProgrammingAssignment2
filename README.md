makeCacheMatrix <- function() {
    # Inicializar a matriz e a inversa em cache como NULL
    cache <- NULL
    
    # Definir função para definir a matriz
    set <- function(matriz) {
        cache <<- matriz
        # Limpar a inversa armazenada em cache quando uma nova matriz é definida
        cache$inverse <<- NULL
    }
    
    # Definir função para obter a matriz
    get <- function() {
        cache
    }
    
    # Definir função para obter a inversa da matriz
    getInverse <- function() {
        if (!is.null(cache$inverse)) {
            message("Recuperando inversa da cache.")
            return(cache$inverse)
        } else {
            message("Calculando inversa e armazenando em cache.")
            cache$inverse <- solve(cache)
            return(cache$inverse)
        }
    }
    
    # Retornar a lista de funções
    list(set = set, get = get, getInverse = getInverse)
}
cacheSolve <- function(matriz, ...) {
    # Obter a inversa da cache
    inversa <- matriz$getInverse()
    # Se a inversa não for NULL, retorná-la
    if (!is.null(inversa)) {
        message("Recuperando inversa da cache.")
        return(inversa)
    }
    # Caso contrário, calcular a inversa e armazená-la em cache
    else {
        message("Calculando inversa e armazenando em cache.")
        inversa <- solve(matriz$get(), ...)
        matriz$setInverse(inversa)
        return(inversa)
    }
}

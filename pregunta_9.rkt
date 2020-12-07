#lang scheme

;;agregarpares lista
;;La función recibe una lista donde obtiene todo los números pares
;;Lista con los números pares de una lista dada
(define (agregarpares lista)
  (filter even? lista)
)

;;agregarimpares lista
;;La función recibe una lista donde obtiene todo los números impares
;;Lista con los números impares de una lista dada
(define (agregarimpares lista)
  (filter odd? lista)
)

(define (conga lista1 lista2)
  (let alternar ((resultado '())(pares (agregarpares (append lista1 lista2)))(impares (agregarimpares (append lista1 lista2))))
    (if (null? pares)
        resultado
        (alternar (append resultado (list (car pares)) (list (car impares)))(remove (car pares) pares)(remove (car impares) impares)))
  )
)
#lang scheme
;;(suma lista)
;;Suma los elementos de la lista dada
;; Retorna el valor de la suma de los elementos de la lista entregada.
(define (suma lista)
  (let sum ((total 0)(list lista))
    (if (null? list)
        total
        (sum (+ total (car list))(cdr list)))
  )
)
(define (div lista)
  (let division ((lista1 '())(lista2 lista))
    (cond
      ((= (suma lista1) (suma lista2)) (if (null? lista2) '() (cons lista1 (list lista2))))
      ((> (suma lista1) (suma lista2)) '())
      ((< (suma lista1) (suma lista2)) (division (append lista1 (list (car lista2))) (cdr lista2)))
    )
  )
)
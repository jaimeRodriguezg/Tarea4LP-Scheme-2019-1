#lang scheme
;;factorialsimple n
;;Calcula el factorial de n usando recursión simple.
;;Retorna el valor del factorial del numero entregado.
(define factorialsimple
  (lambda(n)
    (let fact ((i n))
      (if (= i 0)
          1
          (* i (fact(- i 1))))
    )
  )
)

;;(combinatoriasimple a b)
;;Calcula el valor de la combinatoria de a sobre b, llamando a la función factorialsimple.
;;Retorna el valor de la combinatoria de a sobre b.
(define (combinatoriasimple a b)
  (/ (factorialsimple a) (* (factorialsimple b) (factorialsimple (- a b))))
 )

(define (hipersimple x k N n)
  (/ (* (combinatoriasimple k x) (combinatoriasimple (- N k) (- n x))) (combinatoriasimple N n))
)

;;factorialcola n
;;Calcula el factorial de n usando recursión de cola.
;;Retorna el valor del factorial del numero entregado.
(define factorialcola
  (lambda (n)
    (let fact ((i n) (a 1))
      (if (= i 0)
          a
          (fact (- i 1) (* a i)))
    )
  )
)

;;(combinatoriacola a b)
;;Calcula el valor de la combinatoria de a sobre b, llamando a la función factorialcola.
;;Retorna el valor de la combinatoria de a sobre b.
(define (combinatoriacola a b)
  (/ (factorialcola a) (* (factorialcola b) (factorialcola (- a b))))
 )

(define (hipercola x k N n)
  (/ (* (combinatoriacola k x) (combinatoriacola (- N k) (- n x))) (combinatoriacola N n))
)
#lang scheme
;;(calzar matriz intento)
;;la función intenta hacer calzar la matriz original (matriz)
;;con una de las matrices propuestas(intento) que corresponden a las
;;soluciones de los cuadrados magicos de 3x3.
;;Retorna #f si no es posible hacer calzar la matriz con el intento y #t si es posible que calzen entre sí.
(define (calzar matriz intento)
  (let calzar ((matriz matriz) (intento intento))
    (if (null? matriz)
        #t
        (if (= (car matriz) -1)
            (calzar (cdr matriz) (cdr intento))
            (if (= (car matriz)(car intento))
                (calzar (cdr matriz) (cdr intento))
                #f)))
  )
)

(define (magia matriz)
  (define fila1 (car matriz))
  (define fila2 (cadr matriz))
  (define fila3 (caddr matriz))
  (set! matriz (append fila1 fila2 fila3))
  (cond ((calzar matriz '(8 1 6 3 5 7 4 9 2)) '((8 1 6)(3 5 7)(4 9 2)))
        ((calzar matriz '(4 9 2 3 5 7 8 1 6)) '((4 9 2)(3 5 7)(8 1 6)))
        ((calzar matriz '(8 3 4 1 5 9 6 7 2)) '((8 3 4)(1 5 9)(6 7 2)))
        ((calzar matriz '(6 7 2 1 5 9 8 3 4)) '((6 7 2)(1 5 9)(8 3 4)))
        ((calzar matriz '(2 9 4 7 5 3 6 1 8)) '((2 9 4)(7 5 3)(6 1 8)))
        ((calzar matriz '(2 7 6 9 5 1 4 3 8)) '((2 7 6)(9 5 1)(4 3 8)))
        ((calzar matriz '(4 3 8 9 5 1 2 7 6)) '((4 3 8)(9 5 1)(2 7 6)))
        ((calzar matriz '(6 1 8 7 5 3 2 9 4)) '((6 1 8)(7 5 3)(2 9 4)))
  )
)
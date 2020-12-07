#lang scheme
;;(inOrder lista)
;;recibe una lista y la recorre en inorden
;;Entrega los nodos del arbol según como son visitados.
(define (inOrder lista)
  (if (null? lista)
      '()
      (append (inOrder (cadr lista))
              (list (car lista))
              (inOrder (caddr lista))))
)

;;(distancia arbol numero)
;;Va buscando el numero dentro del arbol y aumentando en 1 cada vez que pasa al siguiente nodo.
;;Devuelve la altura del nodo dentro del arbol.
(define (distancia arbol numero)
  (define (izq lista) (cadr lista))
  (define (der lista) (caddr lista))
  (let distancia((d 0)(arbol arbol))
    (if (null? arbol)
        d
        (cond
          ((= numero (car arbol)) d)
          ((member numero (inOrder (izq arbol)))(distancia (+ 1 d) (cadr arbol)))
          ((member numero (inOrder (der arbol)))(distancia (+ 1 d) (caddr arbol)))))
  )
)

;;(mayores arbol nodos)
;;Compara las distancias de cada nodo a la raiz y devuelve los que se encuentran a la mayor distancia.
;;Devuelve una lista con los nodos más alejados de la raíz.
(define (mayores arbol nodos)
  (let mayor((resultado '())(m 0)(nodos nodos))
    (if (null? nodos)
        resultado
        (cond
          ((> (distancia arbol (car nodos)) m)(mayor (list (car nodos)) (distancia arbol (car nodos)) (cdr nodos)))
          ((= (distancia arbol (car nodos)) m)(mayor (append resultado (list (car nodos))) m (cdr nodos)))
          ((< (distancia arbol (car nodos)) m)(mayor resultado m (cdr nodos)))))
   )
)

(define (hoja arbol)
  (define nodos (inOrder arbol))
  (if (= (length (mayores arbol nodos)) 1)
      (car (mayores arbol nodos))
      (mayores arbol nodos))
)
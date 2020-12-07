#lang scheme

;;lista-master list
;;La función recibe un grafo (lista) donde obtiene todo los nodos maestros
;;Lista con los nodos maestros
(define (lista-master grafo)
  (if (null? grafo)
      '()
      (append (list (car (car grafo)))
              (lista-master (cdr grafo))))
)

;;lista-slave list
;;La función recibe un grafo (lista) donde obtiene todo los nodos no maestros
;;Lista con los nodos no maestros
(define (lista-slave grafo)
  (if (null? grafo)
      '()
      (append (cdr (car grafo))
              (lista-slave (cdr grafo))))
)

;;join list
;;La función recibe un grafo (lista) donde obtiene todo el primer elemento de la lista, donde se obtiene un nodo maestro, con sus nodos no maestros correspondientes
;;Lista con nodo maestro y nodos no maestros
(define (join grafo)
  (car (map cons (lista-master grafo) (lista-slave grafo)))
)

;;lista-completa-slave list
;;La función recibe un grafo (lista) donde se ordena la lista ya generada de los nodos no maestros, de la siguiente forma '(a b c d)
;;Lista ordenada con los nodos no maestros
(define (lista-completa-slave grafo)
  (if (null? grafo)
      '()
    (append (car grafo)
            (lista-completa-slave(cdr grafo))))
)

(define (maestro nodo grafo)
  (if (and (not (member nodo (lista-master grafo))) (not (member nodo (lista-completa-slave (lista-slave grafo)))))  
      #f
      (if (and (eqv? nodo (car (join grafo))) (not (member (car (join grafo)) (lista-completa-slave (lista-slave grafo)))))
          #f
          (if (list? (member nodo (cdr (join grafo))))
          (list(car (join grafo)))
          (maestro nodo (cdr grafo)))))
)
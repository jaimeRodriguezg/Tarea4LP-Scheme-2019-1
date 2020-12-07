#lang scheme
;;(incrementar lista)
;;Recibe una lista e incrementa en 1 cada uno de sus elementos.
;;Devuelve una lista con cada número de la lista original aumentado en 1.
(define (incrementar lista)
  (let incrementar ((resultado '())(lista lista))
    (if (null? lista)
        resultado
        (incrementar (append resultado (list (+ 1 (car lista)))) (cdr lista)))))

;;(menor lista)
;;Encuentra el menor numero dentro de una lista dada.
;;Retorna el menor elemento de la lista.
(define (menor lista)
  (let menor ((m (car lista))(lista lista))
    (if (null? lista)
        m
        (if (< (car lista) m)
            (menor (car lista) (cdr lista))
            (menor m (cdr lista))))))

;;(ocuparcaja tiempos pos num)
;;Actualiza la lista de tiempos que se demoran las cajas cambiando el 0
;;que se encuentra en la posición pos por el numero entregado
;;(nueva cantidad de tiempo que se demorará en desocuparse la caja que se encontraba previamente disponible).
;;Retorna una lista de tiempos con los valores actualizados.
(define (ocuparcaja tiempos pos num)
  (let ocuparcaja ((resultado '())(tiempos tiempos)(pos pos)(cont 0)(num num))
    (if (null? tiempos)
        resultado
        (if (= cont pos)
            (ocuparcaja (append resultado (list num)) (cdr tiempos) pos (+ 1 cont) num)
            (ocuparcaja (append resultado (list (car tiempos))) (cdr tiempos) pos (+ 1 cont) num)))))

;;(transcurrirtiempo lista numero)
;;Resta el número entregado a cada uno de los elementos de la lista.
;;Transcurre el tiempo necesario para que una caja se desocupe(llegue a cero)
;;es por esto que se elige el minimo de los tiempos para transcurrir.
;;Retorna una lista de tiempos con los valores actualizados.
(define (transcurrirtiempo lista numero)
  (let transcurrir ((resultado '()) (lista lista) (numero numero))
    (if (null? lista)
        resultado
        (transcurrir (append resultado (list (- (car lista) numero))) (cdr lista) numero))))

;;(ceros lista)
;;Crea una lista de ceros del mismo tamaño de la lista entregada.
;;Retorna una lista de ceros, cuyo largo es el mismo al de la lista entregada.
(define (ceros lista)
  (let ceros ((resultado '())(lista lista))
    (if (null? lista)
        resultado
        (ceros (append resultado '(0))(cdr lista)))))

;;(disponible lista)
;;Busca algun 0 dentro de la lista entregada, esto es
;;buscar si hay una caja desocupada dentro de los tiempos que falta para que se desocupen las cajas.
;;Retorna #f si no hay ninguna caja disponible para su uso inmediato y en caso de que encuentre una caja disponible
;devuelve la posición de esta, lo que corresponde a "que caja es la desocupada".
(define (disponible lista)
  (let disponible ((lista lista)(pos 0)(max (- (length lista) 1)))
    (if (> pos max)
        #f
        (if (= (list-ref lista pos) 0)
            pos
            (disponible lista (+ 1 pos) max)))))

(define (caja cajeros lista)
  (define tiemposcajas (ceros (range cajeros)))
  (let caja ((resultado '())(tiempos tiemposcajas)(clientes lista))
    (if (null? clientes)
        (incrementar resultado)
        (if (eq? (disponible tiempos) #f)
            (caja resultado (transcurrirtiempo tiempos (menor tiempos)) clientes)
            (caja (append resultado (list (disponible tiempos))) (ocuparcaja tiempos (disponible tiempos) (car clientes))(remove (car clientes) clientes))))))
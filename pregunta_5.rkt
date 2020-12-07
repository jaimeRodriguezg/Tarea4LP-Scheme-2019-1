#lang scheme
(define i (lambda (x) (* x x)))
(define p (lambda (x) (* 2 x)))

(define (p_i i p x)
  (cond
    ((even? x) (+ (p (p x)) (i x)))
    (else (+ (i (i x)) (p x)))
  )
)
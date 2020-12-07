#lang scheme
(define (decbin numero)
  (let decbin ((num numero)(resultado ""))
    (if (zero? num)
        (if (eq? resultado "")
            "0"
            resultado)
        (if (zero? (remainder num 2))
            (decbin (quotient num 2) (string-append "0" resultado))
            (decbin (quotient num 2) (string-append "1" resultado))))
  )
)
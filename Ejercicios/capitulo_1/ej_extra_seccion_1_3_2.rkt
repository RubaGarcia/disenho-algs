#lang racket

(define (f x)
  (let ((a (+ x 10))
        (b (* a 5)))
    (+ a b))
  )

; Este codigo falla debido a que al realizar la sustitucion, primero sustituye a por (+ x 10) y b por (* a 5)
; La funcion con la que termina trabajando es (+ (+ x 10) (* a 5)) y el programa desconoce el valor de a, a pesar de haberlo sustituido antes
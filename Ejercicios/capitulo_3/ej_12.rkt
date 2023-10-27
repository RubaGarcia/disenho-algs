#lang racket

(define (append x y)
  (if (null? x)
      y
      (mcons (mcar x) (append (mcdr x) y))))


(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))


(define x (mcons 'a (mcons 'b null)))
(define y (mcons 'c (mcons 'd null)))
(define z (append x y))

z
(mcons 'a (mcons 'b (mcons 'c (mcons 'd '()))))

(mcdr x)
;(mcons 'b '())
;nos ha devuelto el segundo elemento de x, como z une una copia de x con y
;no modifica el elemento original

(define w (append! x y))

w
(mcons 'a (mcons 'b (mcons 'c (mcons 'd '()))))

(mcdr x)
;(mcons 'b (mcons 'c (mcons 'd '())))
;nos ha devuelto el segundo elemento de x concatenado con los elementos de
;la lista y, ésto es porque la funcion "append!" toma los punteros de ambos
;elementos concatenando el primero con el segundo, modificando así el x original








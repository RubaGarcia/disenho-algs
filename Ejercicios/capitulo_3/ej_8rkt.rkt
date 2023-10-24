#lang racket
(define temp 1)
(define (f x)
  (cond ((= x 0) (set! temp 0) temp)
        ((= x 1) temp)))

(+ (f 0) (f 1))
(set! temp 1) ;linea auxiliar para poder comprobar ambas de una
(+ (f 1) (f 0))
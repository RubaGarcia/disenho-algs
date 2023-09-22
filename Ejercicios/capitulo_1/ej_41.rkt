#lang racket

(define (inc x)
  (+ x 1))

(define (double f)
  (lambda (n) (f (f n))))

(((double (double double)) inc) 5)

;(((double (double double)) inc) 5)
;>21

;un double afecta a otro double tal que las operaciones del double recibido por
;parámetro son elevadas al cuadrado
#lang racket

(define (reverse list) (reverse_aux list null))
(define (reverse_aux list list2)
  (if (null? list)
      list2
      (reverse_aux (cdr list) (cons (car list) list2))))



(define (deep-reverse l)
  (if (pair? l)
      (reverse l)
      (cdr l)))

(define x 
  (list (list 1 2) (list 3 4)))

x
;((1 2) (3 4))

(reverse x)
;;((3 4) (1 2))

(deep-reverse x)
;((4 3) (2 1))
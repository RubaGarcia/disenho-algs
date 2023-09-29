#lang racket

(define (reverse list) (reverse_aux list null))
(define (reverse_aux list list2)
  (if (null? list)
      list2
      (reverse_aux (cdr list) (cons (car list) list2))))

(reverse (list 1 4 9 16 25))
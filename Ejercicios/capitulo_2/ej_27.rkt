#lang racket

(define (reverse list) (reverse_aux list null))
(define (reverse_aux list list2)
  (if (null? list)
      list2
      (reverse_aux (cdr list) (cons (car list) list2))))



(define (deep-reverse list) (deep-reverse-aux list null))

(define (deep-reverse-aux list1 list2)
  (if (null? list1)
      list2
      (deep-reverse-aux (cdr list1) (cons (check-nested-list (car list1)) list2))))

(define (check-nested-list elem)
  (if (list? elem)
      (deep-reverse-aux elem null)
      elem))

(define x 
  (list (list 1 2) (list 3 4)))

x
;((1 2) (3 4))

(reverse x)
;;((3 4) (1 2))

(deep-reverse x)
;((4 3) (2 1))
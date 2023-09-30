#lang racket
(define x 
  (list (list 1 2) (list 3 4)))

(define (fringe list)
  (cond
    ((null? list) '()) ; Si la lista está vacía, devuelve una lista vacía
    ((list? (car list)) ; Si el primer elemento es una lista
     (append (fringe (car list)) (fringe (cdr list)))) ; Aplana la lista y procesa el resto de la lista
    (else ; Si el primer elemento no es una lista
     (cons (car list) (fringe (cdr list)))))) ;

(displayln x)
(fringe x)
(fringe (list x x))
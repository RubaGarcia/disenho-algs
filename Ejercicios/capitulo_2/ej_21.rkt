#lang racket
(define (square-list items) ;metodo que define square-list mediante el uso de constante
  (if (null? items)
      items
      (cons (* (car items) (car items)) (square-list(cdr items)))))


(define (square-list2 items) ;metodo que define square-list mediante el uso del metodo map
  (map (lambda (x) (* x x)) items)) 
  
(square-list (list 1 2 3 4))
(square-list2 (list 1 2 3 4))

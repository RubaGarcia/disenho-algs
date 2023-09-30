#lang racket


(define (reverse list)
  (if (null? (cdr list))
      (car list)
      (join (reverse (cdr list)) (car list))))

(define (join elem1 elem2)
  (if (list? elem1)
      (if (list? elem1)
          (cons elem2 null)
          (cons (car elem1) (join (cdr elem1) elem2)))
      (list elem1 elem2)))



(define (deep-reverse list)
    (if (null? (cdr list))
      (internal-reverse (car list))
      (join (deep-reverse (cdr list)) (internal-reverse (car list)))))



(define (internal-reverse elem)
  (if (list? elem)
    (deep-reverse elem)
    elem))

(define x 
  (list (list 1 2) (list 3 4)))

x
;((1 2) (3 4))

(reverse x)
;;((3 4) (1 2))

(deep-reverse x)
;((4 3) (2 1))
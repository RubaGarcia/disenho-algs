#lang racket

(define (reverse list)
  (if (null? (cdr list))
      (car list)
      (join (reverse (cdr list)) (car list))))

(define (join elem1 elem2)
  (if (list? elem1)
      (if (null? elem1)
          (cons elem2 null)
          (cons (car elem1) (join (cdr elem1) elem2)))
      (list elem1 elem2)))
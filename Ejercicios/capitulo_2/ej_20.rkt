#lang racket

(define (same-parity x . list)
  (if (even? x)
      (if (both-even? x (car list))
          (same-parity-list (join x (car list)) (cdr list))
          (same-parity-list x (cdr list)))
      (if (both-odd? x (car list))
          (same-parity-list (join x (car list)) (cdr list))
          (same-parity-list x (cdr list)))))

(define (same-parity-list x list)
  (if (null? list)
      x
      (if (even? (get-first x))
          (if (both-even? (get-first x) (car list))
              (same-parity-list (join x (car list)) (cdr list))
              (same-parity-list x (cdr list)))
          (if (both-odd? (get-first x) (car list))
              (same-parity-list (join x (car list)) (cdr list))
              (same-parity-list x (cdr list))))))


(define (both-even? x y)
  (and (even? x) (even? y)))

(define (both-odd? x y)
  (and (odd? x) (odd? y)))

(define (join elem1 elem2)
  (if (list? elem1)
      (if (null? elem1)
          (cons elem2 null)
          (cons (car elem1) (join (cdr elem1) elem2)))
      (list elem1 elem2)))

(define (get-first x)
  (if (list? x)
      (car x)
      x))
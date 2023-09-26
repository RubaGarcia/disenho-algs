#lang racket
(define (cons a b) 
  (* (expt 2 a) (expt 3 b)))

(define (acumulative-car z n)
  (if (= (modulo z 2) 0) (acumulative-car (/ z 2) (+ n 1)) n ))
(define (car z) 
  (acumulative-car z 0))

(define (acumulative-cdr z n)
  (if (= (modulo z 3) 0) (acumulative-cdr (/ z 3) (+ n 1)) n ))
(define (cdr z) 
  (acumulative-cdr z 0))
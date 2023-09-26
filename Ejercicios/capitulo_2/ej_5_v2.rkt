#lang racket

(define (cons x y) (* (expt 2 x) (expt 3 y)))

(define (iter-div num d count)
  (if (= (modulo num d) 0)
      (iter-div (/ num d) d (+ 1 count))
      count))

(define (car num) (iter-div num 2 0))

(define (cdr num) (iter-div num 3 0))
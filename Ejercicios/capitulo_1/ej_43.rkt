#lang racket

(define (compose f g) (lambda (n) (f (g n))))

(define (repeated f x)
  (if (<= x 1)
      f
      (compose f (repeated f (- x 1)))))

(define (square x)
  (* x x))
      
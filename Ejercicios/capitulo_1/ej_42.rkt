#lang racket

(define (compose f g) (lambda (n) (f (g n))))

(define (square n) (* n n))

(define (inc n) (+ 1 n))

((compose square inc) 6)
#lang racket

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-point x y) (cons x y))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (make-point
   (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
   (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
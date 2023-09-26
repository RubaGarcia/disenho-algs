#lang racket

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-point x y) (cons x y))

(define (make-rectangle p1 p2) (cons p1 p2))

(define (corner-1 rect) (car rect))
(define (corner-2 rect) (cdr rect))

(define (resta-abs x y)
  (if (> x y)
      (- x y)
      (- y x)))
      

(define (base rect)
  (resta-abs (x-point (corner-1 rect)) (x-point (corner-2 rect))))

(define (height rect)
  (resta-abs (y-point (corner-1 rect)) (y-point (corner-2 rect))))

(define (perimeter rect)
  (+ (* 2 (base rect)) (* 2 (height rect))))

(define (area rect)
  (* (base rect) (height rect)))
#lang racket

(define (func x)
         (if (< x 3)
             x
             (+ (func (- x 1)) (+ (* 2 (func (- x 2))) (* 3 (func (- x 3)))))))

(define (func-iter-aux n x y z)
  (cond ((= n 2) x)
        ((= n 1) y)
        ((= n 0) z)
        (else (func-iter-aux (- n 1) (+ x (+ (* 2 y) (* 3 z))) x y))))

(define (func-iter x)
  (func-iter-aux x 2 1 0))
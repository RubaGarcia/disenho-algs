#lang racket

(define (pascal x y)
  (if (or (= y 0) (= x 0) (= x y))
      1
      (+ (pascal x (- y 1)) (pascal (- x 1) (- y 1)))))
#lang racket

(define (mul-streams s1 s2)
 (stream-cons (* (stream-first s1) (stream-first s2)) (mul-streams (stream-rest s1) (stream-rest s2))))

(define (add-streams s1 s2)
 (stream-cons (+ (stream-first s1) (stream-first s2)) (add-streams (stream-rest s1) (stream-rest s2))))

(define ones (stream-cons 1 ones))

(define integers 
 (stream-cons 1 (add-streams ones integers)))

(define factorials 
 (stream-cons 1 (mul-streams integers factorials)))


(define (stream-ref s n)
 (if (zero? n)
      (stream-first s)
      (stream-ref (stream-rest s) (- n 1))))

(display "Factorial de 5: ")
(display (stream-ref factorials 5))
(newline)
#lang racket
(define mapa (make-hash))
(define value 0)

(define (p n) (p_act n n))

(define (p_act n k)
  (cond ((and (= n k) (= n 0)) 1)
        ((or (< n 0) (< k 1)) 0)
        (else (hash-ref mapa '(n k)
                        (begin (set! value (+ (p_act n (- k 1)) (p_act (- n k) k))) (hash-set! mapa '(n k) value) value)))))
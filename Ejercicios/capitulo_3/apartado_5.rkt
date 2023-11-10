#lang racket
(define (p n) (p_act n n))

(define (p_act n k)
  (cond ((and (= n k) (= n 0)) 1)
        ((and (>= n 0) (>= k 1)) (+ (p_act n (- k 1)) (p_act (- n k) k)))
        (else 0)))

;; la complejidad espacial seria n * n
;; la complejidad temporal es 2 ^ n ya que para cada elemento se calculan otros dos

#lang racket
(define mapa (make-hash))
(define value 0)

(define (p n) (p_act n n))

(define (p_act n k)
  (if (hash-has-key? mapa (list n k))
      (hash-ref mapa (list n k))
      (cond ((and (= n k) (= n 0)) 1)
            ((or (< n 0) (< k 1)) 0)
            (else (hash-ref mapa (list n k)
                            (hash-set! mapa (list n k) (+ (p_act n (- k 1)) (p_act (- n k) k))))))))
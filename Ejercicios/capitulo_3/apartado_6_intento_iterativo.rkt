#lang racket

(define mapa (make-hash))

(define (p n) (p-act n n))

(define (p-act n k)
  (cond ((< n k) (p-act n n)) (else
  (for ([i (in-range 0 (+ n 1))])
    (for ([j (in-range 0 (+ k 1))])
      (if (and (= j 0) (> i 0)) (hash-set! mapa (list i j) 0) ;si j = 0 y i > 0, valor = 0
         
      (when (or (< i 2) (< j 2))
          (hash-set! mapa (list i j) 1))))); todo valor que tenga como uno de los indices algo menor a 2 vale 1.
  
  (for ([i (in-range 2 (+ n 1))])
    (for ([j (in-range 2 (+ k 1))])
      (hash-set! mapa (list i j) (+ (hash-ref mapa (list i (- j 1))) (hash-ref mapa (list(- i j)) j))))); bucle que hace el cálculo

  (hash-ref mapa (list n k))))) ; retorna el valor p(n, k)


;hash-ref = getKey()
;hash-set! = a una llave asociar un valor
;tengo que crear una lista bidimensional...
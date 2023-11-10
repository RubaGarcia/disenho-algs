#lang racket

(define mapa (make-hash))

(define (p n) (p-act n n))

(define (p-act n k)
  (cond ((< n k) (p-act n n)) (else
  (for ([i (in-range 0 (+ n 1))])
    (for ([j (in-range 0 (+ k 1))])
      (if (and (= j 0) (> i 0)) (hash-set! mapa (list i j) 0) ;si j = 0 y i > 0, valor = 0
         
      (when (or (< i 2) (> j 0))
          (hash-set! mapa (list i j) 1))))); todo valor que tenga como uno de los indices algo menor a 2 vale 1.
  
  (for ([i (in-range 2 (+ n 1))])
    (for ([j (in-range 2 (+ k 1))])
      (if (and (or (< 0 (- i j)) (= 0 (- i j))) (> (- j 1) 1))
      (hash-set! mapa (list i j) (+ (hash-ref mapa (list i (- j 1))) (hash-ref mapa (list(- i j) j)))) ;si se puede restar j-i y j -1 > 1 se hace normal
      (if (or (< 0 (- i j)) (= 0 (- i j))) 
      (hash-set! mapa (list i j) (+ 1 (hash-ref mapa (list(- i j) j)))) ;si se puede restar j-i  pero j-1 = 1 -> se suma 1 en vez del acceso de i j-1
      (hash-set! mapa (list i j) (hash-ref mapa (list i (- j 1)))));si no se puede restar j-i solo se establece el j-1, porque para j = 0 result = 0
      ))) ;bucle que hace el cálculo

  (hash-ref mapa (list n k))))) ; retorna el valor p(n, k)


;hash-ref = getKey()
;hash-set! = a una llave asociar un valor
;tengo que crear una lista bidimensional...
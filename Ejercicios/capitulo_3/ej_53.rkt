#lang racket


(define (add-streams s1 s2) 
  (stream-map + s1 s2))
;add-streams suma los dos streams a través de los stream -maps


(define s (stream-cons 1 (add-streams s s)))

;ésta linea se define como la concatenación de un escalar(1)
;con la suma del stream s consigo mismo
;
;stream-cons crea un nuevo stream con el primer elemento siendo un 1 y el resto
;siendo los el stream s dos veces(la suma de s consigo mismo)

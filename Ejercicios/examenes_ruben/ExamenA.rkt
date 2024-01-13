#lang racket
(define (crea-triangulo . puntos)
  (define mapa-puntos (make-hash))

  
  (define (guardar-puntos lista-puntos indice)
    (if (null? lista-puntos)
        null
        (begin (hash-set! mapa-puntos indice (cons (car lista-puntos)(cadr lista-puntos)))
               (guardar-puntos (cddr lista-puntos ) (+ indice 1)))))
  (if (not (eq? 6 (length puntos)))
      (display "numero de puntos incorrecto")
      (guardar-puntos puntos 1))

  (define (dist-puntos p1 p2)
    (sqrt (+ (expt (- (car p1) (car p2)) 2) (expt (- (cdr p1) (cdr p2)) 2))))

  (define (perimetro)
    (+ (dist-puntos (dispatch 'p1) (dispatch 'p2))
       (dist-puntos (dispatch 'p3) (dispatch 'p2))
       (dist-puntos (dispatch 'p1) (dispatch 'p3))))
    

  
  (define (dispatch message)
    (cond ((eq? message 'p1) (hash-ref mapa-puntos 1))
          ((eq? message 'p2) (hash-ref mapa-puntos 2))
          ((eq? message 'p3) (hash-ref mapa-puntos 3))
          ((eq? message 'perimetro) (perimetro))))
  dispatch
  
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ejercicio 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (same-parity-list x list)
  (if (null? list)
      x
      (if (even? (get-first x))
          (if (both-even? (get-first x) (car list))
              (same-parity-list (join x (car list)) (cdr list))
              (same-parity-list x (cdr list)))
          (if (both-odd? (get-first x) (car list))
              (same-parity-list (join x (car list)) (cdr list))
              (same-parity-list x (cdr list))))))


(define (both-even? x y)
  (and (even? x) (even? y)))

(define (both-odd? x y)
  (and (odd? x) (odd? y)))

(define (join elem1 elem2)
  (if (list? elem1)
      (if (null? elem1)
          (cons elem2 null)
          (cons (car elem1) (join (cdr elem1) elem2)))
      (list elem1 elem2)))

(define (get-first x)
  (if (list? x)
      (car x)
      x))
(define (suma-impares-iter x . lista)
  (define (lista-misma-paridad) (same-parity-list x lista))
  (suma-iguales-aux (same-parity-list x lista) 0))

(define (suma-iguales-aux lista result)
  (if (null? lista)
      result
      (if (list? lista)
          (suma-iguales-aux (cdr lista) (+ result (car lista)))
          lista)))